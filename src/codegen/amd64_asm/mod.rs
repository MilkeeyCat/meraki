mod allocator;
mod operand;
mod register;

use super::Codegen;
use crate::ir::{Block, Expr, ExprKind, ExprLit, Id, Ir, Item, ItemFn, ItemVar, Stmt, Ty};
use allocator::RegisterAllocator;
use derive_more::derive::Display;
use indoc::formatdoc;
use operand::{Destination, EffectiveAddress, Memory, Source};
use register::Register;
use std::collections::HashMap;

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
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(match value {
            1 => Self::Byte,
            2 => Self::Word,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => unreachable!(),
        })
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

    fn compile(&mut self) -> Vec<u8> {
        for item in self.ir.iter_items() {
            self.item(item).unwrap();
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

        result.into_bytes()
    }
}

impl<'a, 'ir> Amd64Asm<'a, 'ir> {
    const BITNESS: usize = 64;

    fn item(&mut self, item: Item) -> Result<(), allocator::Error> {
        match item {
            Item::Fn(item) => self.function(item),
            Item::Var(item) => {
                if item.id.node_id == 0 {
                    self.global(item);
                } else {
                    self.local(item)?;
                }

                Ok(())
            }
        }
    }

    fn function(&mut self, item: &ItemFn) -> Result<(), allocator::Error> {
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
            .map(|stmt| self.stmt(*stmt))
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

    fn stmt(&mut self, stmt: Stmt) -> Result<(), allocator::Error> {
        match stmt {
            Stmt::Return(stmt) => self.ret(stmt),
            Stmt::Item(item) => self.item(item),
            _ => todo!(),
        }
    }

    fn expr(&mut self, expr: Expr, dest: &Destination) -> Result<(), allocator::Error> {
        match expr.kind {
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

    fn global(&mut self, item: &ItemVar) {}

    fn local(&mut self, item: &ItemVar) -> Result<(), allocator::Error> {
        let size = self.ty_size(item.ty);

        self.stack_offset -= size as isize;
        let dest = Destination::Memory(Memory {
            effective_address: Register::Rbp.into_effective_addr(self.stack_offset),
            size: size.try_into().unwrap(),
        });

        self.variables.insert(
            item.id,
            Destination::Memory(Memory {
                effective_address: Register::Rbp.into_effective_addr(self.stack_offset),
                size: size.try_into().unwrap(),
            }),
        );

        if let Some(expr) = item.initializer {
            self.expr(expr, &dest)?;
        }

        Ok(())
    }

    fn ret(&mut self, expr: Option<Expr>) -> Result<(), allocator::Error> {
        if let Some(expr) = expr {
            let r = self.allocator.alloc(OperandSize::Qword)?;
            let dest = r.clone().into();

            self.expr(expr, &dest)?;
            self.mov(&dest.into(), &Register::Rax.into(), false)?;
            self.allocator.free(r)?;
        }

        Ok(())
    }

    fn sub(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), allocator::Error> {
        self.text.push_str(&format!("\tsub {lhs}, {rhs}\n"));

        if lhs != dest {
            self.mov(lhs, dest, signed)?;
        }

        Ok(())
    }

    fn mov(
        &mut self,
        src: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), allocator::Error> {
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

    fn ty_size(&self, ty: &Ty) -> usize {
        ty.size(Self::BITNESS)
    }

    fn stack_frame_size(&self, block: &Block) -> usize {
        let mut size = 0;

        for stmt in block.0 {
            match stmt {
                Stmt::Item(item) => match item {
                    Item::Var(item) => {
                        size += self.ty_size(item.ty);
                    }
                    Item::Fn(_) => (),
                },
                Stmt::Expr(_) | Stmt::Return(_) => (),
            }
        }

        size
    }
}
