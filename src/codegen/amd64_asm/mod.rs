mod allocator;
mod operand;
mod register;

use super::Codegen;
use crate::{
    ir::{Block, Expr, ExprKind, ExprLit, Id, Ir, Item, ItemFn, Stmt, Ty, Variable},
    parser::{BinOp, CmpOp, OpParseError},
};
use allocator::RegisterAllocator;
use derive_more::derive::Display;
use indoc::formatdoc;
use operand::{Destination, Memory, Source};
use register::Register;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
#[error("{0} is not a valid operand size")]
pub struct InvalidOperandSizeError(usize);

#[derive(Clone, Debug, PartialEq, PartialOrd, Display)]
pub enum OperandSize {
    #[display("byte ptr")]
    Byte,
    #[display("word ptr")]
    Word,
    #[display("dword ptr")]
    Dword,
    #[display("qword ptr")]
    Qword,
}

impl TryFrom<usize> for OperandSize {
    type Error = InvalidOperandSizeError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(match value {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => return Err(InvalidOperandSizeError(value)),
        })
    }
}

#[derive(Debug, Error)]
pub enum Amd64AsmError {
    #[error(transparent)]
    InvalidOperandSize(#[from] InvalidOperandSizeError),
    #[error(transparent)]
    RegisterAllocator(#[from] allocator::Error),
    #[error(transparent)]
    OpParse(#[from] OpParseError),
}

impl std::fmt::Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LessThan => "setl",
                Self::LessEqual => "setle",
                Self::GreaterThan => "setg",
                Self::GreaterEqual => "setge",
                Self::Equal => "sete",
                Self::NotEqual => "setne",
            }
        )
    }
}

pub struct Amd64Asm<'a, 'ir> {
    ir: &'a Ir<'ir>,
    allocator: RegisterAllocator,
    bss: String,
    data: String,
    text: String,
    stack_offset: isize,
    variables: HashMap<Id, Destination>,
}

impl<'a, 'ir> Codegen<'a, 'ir> for Amd64Asm<'a, 'ir> {
    fn new(ir: &'a Ir<'ir>) -> Self {
        Self {
            ir,
            allocator: RegisterAllocator::new(vec![
                Register::R15,
                Register::R14,
                Register::R13,
                Register::R12,
                Register::R11,
                Register::R10,
                Register::R9,
                Register::R8,
                Register::Rcx,
                Register::Rdx,
                Register::Rsi,
                Register::Rdi,
            ]),
            bss: String::new(),
            data: String::new(),
            text: String::new(),
            stack_offset: 0,
            variables: HashMap::new(),
        }
    }

    fn compile(&mut self) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        for item in self.ir.iter_items() {
            self.item(&item)?;
        }

        let mut result = String::new();

        if !self.bss.is_empty() {
            result.push_str(".section .bss\n");
            result.push_str(&self.bss);
        }
        if !self.data.is_empty() {
            result.push_str(".section .data\n");
            result.push_str(&self.data);
        }
        if !self.text.is_empty() {
            result.push_str(".section .text\n");
            result.push_str(&self.text);
        }

        Ok(result.into_bytes())
    }
}

impl<'a, 'ir> Amd64Asm<'a, 'ir> {
    const BITNESS: usize = 64;

    fn item(&mut self, item: &Item) -> Result<(), Amd64AsmError> {
        match item {
            Item::Fn(item) => self.function(item),
            Item::Global(item) => {
                self.global(item)?;

                Ok(())
            }
        }
    }

    fn function(&mut self, item: &ItemFn) -> Result<(), Amd64AsmError> {
        let name = item.name;

        self.text.push_str(&formatdoc!(
            "
            .global {name}
            {name}:
            "
        ));

        let stack_frame = self.stack_frame_size(&item.block);

        if stack_frame > 0 {
            let rbp = &Register::Rbp.into();
            self.push(rbp);
            self.mov(&Register::Rsp.into(), &Register::Rbp.into(), false)?;
            self.sub(
                &Register::Rsp.into(),
                &(stack_frame as u64).into(),
                &Register::Rsp.into(),
                false,
            )?;
        }

        item.block
            .0
            .iter()
            .map(|stmt| self.stmt(stmt))
            .collect::<Result<(), _>>()?;
        self.text.push_str(&format!("{name}$ret:\n"));

        if stack_frame > 0 {
            self.text.push_str(&format!("\tleave\n"));
        }

        self.text.push_str(&format!("\tret\n"));
        self.stack_offset = 0;
        self.variables.clear();

        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<(), Amd64AsmError> {
        match stmt {
            Stmt::Local(stmt) => self.local(stmt),
            Stmt::Return(expr) => self.ret(expr.as_ref()),
            Stmt::Item(item) => self.item(item),
            _ => todo!(),
        }
    }

    fn expr(&mut self, expr: &Expr, dest: &Destination) -> Result<(), Amd64AsmError> {
        match expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                let r_lhs = self.allocator.alloc(self.ty_size(lhs.ty()).try_into()?)?;
                self.expr(lhs, &r_lhs.into())?;

                let r_rhs = self.allocator.alloc(self.ty_size(rhs.ty()).try_into()?)?;
                self.expr(rhs, &r_rhs.into())?;

                match op {
                    BinOp::Add => self.add(&r_lhs.into(), &r_rhs.into(), dest, expr.ty().signed()),
                    BinOp::Sub => self.sub(&r_lhs.into(), &r_rhs.into(), dest, expr.ty().signed()),
                    BinOp::Mul => self.mul(&r_lhs.into(), &r_rhs.into(), dest, expr.ty().signed()),
                    BinOp::Div => self.div(&r_lhs.into(), &r_rhs.into(), dest, expr.ty().signed()),
                    BinOp::Equal
                    | BinOp::NotEqual
                    | BinOp::LessThan
                    | BinOp::LessEqual
                    | BinOp::GreaterThan
                    | BinOp::GreaterEqual => {
                        self.cmp(&r_lhs.into(), &r_rhs.into());
                        self.setcc(dest, CmpOp::try_from(&op)?);

                        Ok(())
                    }
                    _ => todo!(),
                }
            }
            ExprKind::Lit(lit) => match lit {
                ExprLit::Int(int) => self.mov(&int.into(), dest, true),
                ExprLit::UInt(uint) => self.mov(&uint.into(), dest, false),
                ExprLit::Bool(_) => todo!(),
                ExprLit::String(_) => todo!(),
                ExprLit::Null => todo!(),
            },
            ExprKind::Ident(id) => {
                let expr = self.variables.get(&id).unwrap();

                self.mov(&expr.clone().into(), dest, false)
            }
            _ => todo!(),
        }
    }

    fn global(&mut self, item: &Variable) -> Result<(), Amd64AsmError> {
        Ok(())
    }

    fn local(&mut self, stmt: &Variable) -> Result<(), Amd64AsmError> {
        let size = self.ty_size(stmt.ty());

        self.stack_offset -= size as isize;
        let dest = Destination::Memory(Memory {
            effective_address: Register::Rbp.into_effective_addr(self.stack_offset),
            size: size.try_into()?,
        });

        self.variables.insert(
            stmt.id,
            Destination::Memory(Memory {
                effective_address: Register::Rbp.into_effective_addr(self.stack_offset),
                size: size.try_into()?,
            }),
        );

        if let Some(expr) = stmt.initializer {
            self.expr(&expr, &dest)?;
        }

        Ok(())
    }

    fn ret(&mut self, expr: Option<&Expr>) -> Result<(), Amd64AsmError> {
        if let Some(expr) = expr {
            let r = self.allocator.alloc(self.ty_size(expr.ty()).try_into()?)?;
            let dest = r.clone().into();

            self.expr(expr, &dest)?;
            self.mov(&dest.into(), &Register::Rax.into(), false)?;
            self.allocator.free(r)?;
        }

        Ok(())
    }

    fn mov(&mut self, src: &Source, dest: &Destination, signed: bool) -> Result<(), Amd64AsmError> {
        match (dest, src) {
            (dest @ Destination::Memory(_), src @ Source::Memory(_)) => {
                let r = self.allocator.alloc(dest.size()).unwrap();

                self.mov(src, &r.clone().into(), signed)?;
                self.mov(&r.clone().into(), dest, signed)?;

                self.allocator.free(r)?;
            }
            (dest, src) => {
                let dest_size = dest.size();
                let src_size = src.size().unwrap_or_else(|| OperandSize::Qword);

                if dest_size == OperandSize::Qword && src_size == OperandSize::Dword {
                    // On x86_64 you can move 32bit value in 32bit register, and upper 32bits of the register will be zeroed
                    self.mov(src, &Register::Eax.into(), false)?;

                    if signed {
                        self.text.push_str("\tcdqe\n");
                    }

                    self.mov(&Register::Rax.into(), dest, false)?;
                } else if dest_size > src_size {
                    if signed {
                        self.text.push_str(&format!("\tmovsx {dest}, {src}\n"));
                    } else {
                        self.text.push_str(&format!("\tmovzx {dest}, {src}\n"));
                    }
                } else {
                    self.text.push_str(&format!("\tmov {dest}, {src}\n"));
                }
            }
        }

        Ok(())
    }

    fn push(&mut self, src: &Source) {
        self.text.push_str(&format!("\tpush {src}\n"));
    }

    fn pop(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tpop {dest}\n"));
    }

    fn ty_size(&self, ty: &Ty) -> usize {
        ty.size(Self::BITNESS)
    }

    fn stack_frame_size(&self, block: &Block) -> usize {
        let mut size = 0;

        for stmt in block.0 {
            match stmt {
                Stmt::Local(stmt) => {
                    size += self.ty_size(stmt.ty());
                }
                Stmt::Item(_) | Stmt::Expr(_) | Stmt::Return(_) => (),
            }
        }

        size
    }

    fn add(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        lhs.size().map(|size| assert!(size == dest.size()));
        rhs.size().map(|size| assert!(size == dest.size()));
        assert!(!(lhs == dest && rhs == dest));

        let lhs = if let Source::Immediate(_) = lhs {
            self.mov(lhs, &Register::Rax.resize(dest.size()).into(), signed)?;
            &Register::Rax.resize(dest.size()).into()
        } else {
            lhs
        };

        self.text.push_str(&format!("\tadd {lhs}, {rhs}\n"));

        if lhs != dest {
            self.mov(lhs, dest, signed)?;
        }

        Ok(())
    }

    fn sub(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        lhs.size().map(|size| assert!(size == dest.size()));
        rhs.size().map(|size| assert!(size == dest.size()));
        assert!(!(lhs == dest && rhs == dest));

        let lhs = if let Source::Immediate(_) = lhs {
            self.mov(lhs, &Register::Rax.resize(dest.size()).into(), signed)?;
            &Register::Rax.resize(dest.size()).into()
        } else {
            lhs
        };

        self.text.push_str(&format!("\tsub {lhs}, {rhs}\n"));

        if lhs != dest {
            self.mov(lhs, dest, signed)?;
        }

        Ok(())
    }

    fn mul(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        lhs.size().map(|size| assert_eq!(size, dest.size()));
        rhs.size().map(|size| assert_eq!(size, dest.size()));
        assert!(!(lhs == dest && rhs == dest));

        self.mov(
            lhs,
            &lhs.size()
                .map_or(Register::Rax, |size| Register::Rax.resize(size))
                .into(),
            signed,
        )?;

        if rhs != dest {
            self.mov(rhs, dest, signed)?;
        }
        if self.allocator.is_used(&Register::Rdx) {
            self.push(&Register::Rdx.into());
        }

        self.text.push_str(&format!("\timul {dest}\n"));

        if self.allocator.is_used(&Register::Rdx) {
            self.pop(&Register::Rdx.into());
        }

        self.mov(&Register::Rax.resize(dest.size()).into(), dest, signed)?;

        Ok(())
    }

    //NOTE: if mafs isn't mafsing, probably because of this
    fn div(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        lhs.size().map(|size| assert_eq!(size, dest.size()));
        rhs.size().map(|size| assert_eq!(size, dest.size()));
        assert!(!(lhs == dest && rhs == dest));

        self.mov(lhs, &Register::Rax.into(), signed)?;
        self.mov(rhs, dest, signed)?;
        if self.allocator.is_used(&Register::Rdx) {
            self.push(&Register::Rdx.into());
        }

        self.text.push_str(&formatdoc!(
            "
            \tcqo
            \tidiv {dest}
            ",
        ));

        if self.allocator.is_used(&Register::Rdx) {
            self.pop(&Register::Rdx.into());
        }

        self.mov(&Register::Rax.resize(dest.size()).into(), dest, signed)?;

        Ok(())
    }

    fn setcc(&mut self, dest: &Destination, condition: CmpOp) {
        self.text.push_str(&format!("\t{condition} {dest}\n"));
    }

    fn cmp(&mut self, dest: &Destination, src: &Source) {
        self.text.push_str(&format!("\tcmp {dest}, {src}\n"));
    }
}
