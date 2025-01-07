mod allocator;
mod operand;
mod register;

use super::Codegen;
use crate::{
    ast::{BinOp, BitwiseOp, CmpOp, IntTy, OpParseError, UintTy, UnOp},
    ir::{Block, Expr, ExprKind, ExprLit, Id, Item, ItemFn, Node, Stmt, Ty, Variable},
    Context,
};
use allocator::RegisterAllocator;
use derive_more::derive::Display;
use indoc::formatdoc;
use operand::{
    Base, Destination, EffectiveAddress, Immediate, ImmediateStrLitError, Memory, Offset, Source,
};
use register::Register;
use std::collections::HashMap;
use thiserror::Error;

struct LabelGenerator(usize);

impl LabelGenerator {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn generate(&mut self) -> String {
        let str = format!(".L{}", self.0);

        self.0 += 1;

        str
    }
}

#[derive(Error, Debug)]
#[error("{0} is not a valid operand size")]
pub struct InvalidOperandSizeError(usize);

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Display)]
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
    #[error(transparent)]
    ImmediateStrLit(#[from] ImmediateStrLitError),
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

impl std::fmt::Display for BitwiseOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::And => "and",
                Self::Or => "or",
            }
        )
    }
}

#[derive(Display)]
pub enum Jump {
    #[display("jmp")]
    Unconditional,
    #[display("je")]
    Equal,
    #[display("jne")]
    NotEqual,
    #[display("jg")]
    GreaterThan,
    #[display("jge")]
    GreaterEqual,
    #[display("jl")]
    LessThan,
    #[display("jle")]
    LessEqual,
}

#[derive(Debug, Clone)]
enum Location {
    EffectiveAddress(EffectiveAddress),
    Register(Register),
}

impl Location {
    fn dest(&self, size: OperandSize) -> Destination {
        match self {
            Self::EffectiveAddress(addr) => Destination::Memory(Memory {
                effective_address: addr.clone(),
                size,
            }),
            Self::Register(r) => {
                assert_eq!(r.size(), size);

                Destination::Register(*r)
            }
        }
    }

    fn src(&self, size: OperandSize) -> Source {
        match self {
            Self::EffectiveAddress(addr) => Source::Memory(Memory {
                effective_address: addr.clone(),
                size,
            }),
            Self::Register(r) => {
                assert_eq!(r.size(), size);

                Source::Register(*r)
            }
        }
    }
}

impl From<EffectiveAddress> for Location {
    fn from(value: EffectiveAddress) -> Self {
        Self::EffectiveAddress(value)
    }
}

impl From<Register> for Location {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

pub struct Amd64Asm<'a, 'ir> {
    ctx: &'a Context<'ir>,
    allocator: RegisterAllocator,
    label_gen: LabelGenerator,
    bss: String,
    data: String,
    text: String,
    stack_offset: isize,
    variables: HashMap<Id, EffectiveAddress>,
    fields_offsets: HashMap<Id, HashMap<&'ir str, Offset>>,
}

impl<'a, 'ir> Codegen<'a, 'ir> for Amd64Asm<'a, 'ir> {
    fn new(ctx: &'a Context<'ir>) -> Self {
        Self {
            ctx,
            label_gen: LabelGenerator::new(),
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
            fields_offsets: HashMap::new(),
        }
    }

    fn compile(&mut self) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        // TODO: that's hacky, get those ids instead of creating them
        self.ctx
            .ir
            .iter_items()
            .enumerate()
            .filter_map(|(i, item)| match item {
                Item::Struct(_) => Some(Id {
                    global_id: i,
                    node_id: 0,
                }),
                _ => None,
            })
            .for_each(|id| self.set_ty_fields_offsets(id));

        for item in self.ctx.ir.iter_items() {
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

    fn expr_addr(&mut self, expr: &Expr) -> Result<EffectiveAddress, Amd64AsmError> {
        Ok(match expr.kind {
            ExprKind::Ident(id) => self.variables[&id].clone(),
            ExprKind::Unary(op, expr) if op == UnOp::Deref => {
                let r = self.allocator.alloc(OperandSize::Qword)?;

                self.expr(expr, Some(&r.into()))?;

                r.into()
            }
            ExprKind::Field(expr, field) => {
                let id = match self.ctx.resolve_ty(expr.ty) {
                    Ty::Struct(id) => id,
                    _ => unreachable!(),
                };

                self.expr_addr(expr)? + self.fields_offsets[id][field]
            }
            expr => unreachable!("{expr:?} is not a valid lvalue expression"),
        })
    }

    fn item(&mut self, item: &Item) -> Result<(), Amd64AsmError> {
        match item {
            Item::Fn(item) => self.function(item),
            Item::Global(item) => {
                self.global(item)?;

                Ok(())
            }
            Item::Struct(_) => Ok(()),
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

        let ret_label = &self.label_gen.generate();
        self.write_label(&ret_label);

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
            Stmt::Expr(expr) => self.expr(expr, None),
        }
    }

    fn expr(&mut self, expr: &Expr, loc: Option<&Location>) -> Result<(), Amd64AsmError> {
        Ok(match expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                macro_rules! eval_expr {
                    // This shit looks scary
                    ($(($out: ident, $expr: ident)),+) => {
                        $(
                            let $out = self.allocator.alloc(self.ty_size(self.ctx.resolve_ty($expr.ty)).try_into()?)?;
                            self.expr($expr, Some(&$out.into()))?;
                        )+
                    };
                }

                let signed = self.ctx.resolve_ty(expr.ty).signed();

                match op {
                    BinOp::Assign => {
                        let expr_addr = self.expr_addr(lhs)?;

                        self.expr(rhs, Some(&expr_addr.clone().into()))?;

                        if let Some(loc) = loc {
                            let size = self.ty_size(self.ctx.resolve_ty(expr.ty));

                            self.mov_loc(&expr_addr.into(), loc, size, signed)?;
                        }
                    }
                    BinOp::Add => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.add(
                                &r_lhs.into(),
                                &r_rhs.into(),
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                signed,
                            )?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Sub => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.sub(
                                &r_lhs.into(),
                                &r_rhs.into(),
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                signed,
                            )?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Mul => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.mul(
                                &r_lhs.into(),
                                &r_rhs.into(),
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                signed,
                            )?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Div => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.div(
                                &r_lhs.into(),
                                &r_rhs.into(),
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                signed,
                            )?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Equal
                    | BinOp::NotEqual
                    | BinOp::LessThan
                    | BinOp::LessEqual
                    | BinOp::GreaterThan
                    | BinOp::GreaterEqual => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.cmp(&r_lhs.into(), &r_rhs.into());
                            self.setcc(
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                CmpOp::try_from(&op)?,
                            );

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::LogicalOr => {
                        if let Some(loc) = loc {
                            self.logical_or(expr, loc, None)?
                        }
                    }
                    BinOp::LogicalAnd => {
                        if let Some(loc) = loc {
                            self.logical_and(expr, loc, None)?
                        }
                    }
                    BinOp::BitwiseOr | BinOp::BitwiseAnd => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));

                            self.bitwise(
                                &r_lhs.into(),
                                &r_rhs.into(),
                                &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                                BitwiseOp::try_from(&op)?,
                                self.ctx.resolve_ty(expr.ty).signed(),
                            )?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Shl => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));
                            let dest =
                                loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?);

                            self.mov(&r_lhs.into(), &dest, lhs.ty.signed())?;
                            self.shl(&dest, &r_rhs.into())?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                    BinOp::Shr => {
                        if let Some(loc) = loc {
                            eval_expr!((r_lhs, lhs), (r_rhs, rhs));
                            let dest =
                                loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?);

                            self.mov(&r_lhs.into(), &dest, lhs.ty.signed())?;
                            self.shr(&dest, &r_rhs.into())?;

                            self.allocator.free(r_lhs)?;
                            self.allocator.free(r_rhs)?;
                        }
                    }
                };
            }
            ExprKind::Unary(op, inner_expr) => {
                if let Some(loc) = loc {
                    let ty = self.ctx.resolve_ty(expr.ty);
                    let signed = ty.signed();
                    let dest = loc.dest(self.ty_size(ty).try_into()?);

                    match op {
                        UnOp::LogicalNot => {
                            self.expr(inner_expr, Some(loc))?;
                            self.cmp(&dest, &Source::Immediate(Immediate::UInt(0)));
                            self.setcc(&dest, CmpOp::Equal);
                        }
                        UnOp::Negative => {
                            self.expr(inner_expr, Some(loc))?;
                            self.negate(&dest);
                        }
                        UnOp::Address => {
                            let expr_loc = self.expr_addr(inner_expr)?;
                            let r = self.allocator.alloc(OperandSize::Qword)?;

                            self.lea(&r.into(), &expr_loc.dest(OperandSize::Qword).into());
                            self.mov(&r.into(), &loc.dest(OperandSize::Qword), signed)?;

                            self.allocator.free(r)?;
                        }
                        UnOp::Deref => {
                            let expr_addr = self.expr_addr(expr)?;
                            let src = Source::Memory(Memory {
                                effective_address: expr_addr,
                                size: self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?,
                            });

                            self.mov(&src, &dest, signed)?;
                        }
                        UnOp::BitwiseNot => {
                            self.expr(inner_expr, Some(loc))?;
                            self.bitwise_not(&dest);
                        }
                    }
                }
            }
            ExprKind::Lit(lit) => {
                if let Some(loc) = loc {
                    if let ExprLit::String(str) = lit {
                        let label = self.define_str_literal(str);

                        self.mov(
                            &Source::Immediate(label.into()),
                            &loc.dest(self.ty_size(expr.ty).try_into()?),
                            false,
                        )?;
                    } else {
                        let signed = if let ExprLit::Int(_) = lit {
                            true
                        } else {
                            false
                        };

                        self.mov(
                            &Source::Immediate(lit.try_into()?),
                            &loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                            signed,
                        )?;
                    }
                }
            }
            ExprKind::Ident(id) => {
                if let Some(loc) = loc {
                    let expr_addr = &self.variables[&id];
                    let size = self.ty_size(self.ctx.resolve_ty(expr.ty));

                    self.mov_loc(&expr_addr.clone().into(), loc, size, false)?;
                }
            }
            ExprKind::Struct(fields) => {
                if let Some(loc) = loc {
                    let id = match self.ctx.resolve_ty(expr.ty) {
                        Ty::Struct(id) => id,
                        _ => unreachable!(),
                    };

                    for (field, expr) in fields {
                        match loc {
                            Location::EffectiveAddress(addr) => {
                                self.expr(
                                    expr,
                                    Some(&(addr.clone() + self.fields_offsets[id][field]).into()),
                                )?;
                            }
                            Location::Register(_) => unreachable!(),
                        }
                    }
                }
            }
            ExprKind::Field(_, _) => {
                if let Some(loc) = loc {
                    let ty = self.ctx.resolve_ty(expr.ty);
                    let size = self.ty_size(ty);
                    let expr_addr = self.expr_addr(expr)?;

                    self.mov_loc(&expr_addr.into(), loc, size, ty.signed())?;
                }
            }
            ExprKind::Cast(expr, ty) => {
                if let Some(loc) = loc {
                    let casted_ty = self.ctx.resolve_ty(ty);
                    let casted_ty_size = self.ty_size(casted_ty).try_into()?;
                    let expr_ty = self.ctx.resolve_ty(expr.ty);
                    let expr_ty_size = self.ty_size(expr_ty).try_into()?;

                    if self.ty_size(casted_ty) != self.ty_size(expr_ty) {
                        let (r, new) = match loc {
                            Location::Register(r) => (r.resize(expr_ty_size), false),
                            Location::EffectiveAddress(_) => {
                                (self.allocator.alloc(expr_ty_size)?, true)
                            }
                        };

                        self.expr(expr, Some(&r.into()))?;

                        if casted_ty_size > expr_ty_size {
                            self.mov(
                                &r.resize(expr_ty_size).into(),
                                &r.resize(casted_ty_size).into(),
                                expr_ty.signed(),
                            )?;
                        }

                        if new {
                            self.mov(
                                &r.resize(casted_ty_size).into(),
                                &loc.dest(casted_ty_size),
                                casted_ty.signed(),
                            )?;
                            self.allocator.free(r)?;
                        }
                    } else {
                        self.expr(expr, Some(loc))?;
                    }
                }
            }
        })
    }

    fn global(&mut self, item: &Variable) -> Result<(), Amd64AsmError> {
        Ok(())
    }

    fn local(&mut self, stmt: &Variable) -> Result<(), Amd64AsmError> {
        let ty = self.ctx.resolve_ty(stmt.ty);
        let size = self.ty_size(ty);

        self.stack_offset -= size as isize;
        let addr = EffectiveAddress {
            base: Base::Register(Register::Rbp),
            index: None,
            scale: None,
            displacement: Some(Offset(self.stack_offset)),
        };

        self.variables.insert(stmt.id, addr.clone());

        if let Some(expr) = stmt.initializer {
            self.expr(&expr, Some(&addr.into()))?;
        }

        Ok(())
    }

    fn define_str_literal(&mut self, literal: &str) -> String {
        let label = self.label_gen.generate();

        self.data.push_str(&formatdoc!(
            "
            {}:
                .string \"{}\"
            ",
            label,
            literal
        ));

        label
    }

    fn ret(&mut self, expr: Option<&Expr>) -> Result<(), Amd64AsmError> {
        if let Some(expr) = expr {
            let ty = self.ctx.resolve_ty(expr.ty);
            let r = self.allocator.alloc(self.ty_size(ty).try_into()?)?;
            let loc = r.into();

            self.expr(expr, Some(&loc))?;
            self.mov(
                &loc.src(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?),
                &Register::Rax.into(),
                false,
            )?;
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

    fn mov_loc(
        &mut self,
        src: &Location,
        dest: &Location,
        size: usize,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        match (dest, src) {
            (Location::EffectiveAddress(lhs), Location::EffectiveAddress(rhs)) => {
                match OperandSize::try_from(size) {
                    Ok(size) => {
                        self.mov(&rhs.src(size), &lhs.dest(size), signed)?;
                    }
                    Err(_) => {
                        self.inline_memcpy(rhs, lhs, size)?;
                    }
                }
            }
            (lhs, rhs) => {
                let size = size.try_into()?;

                self.mov(&rhs.src(size), &lhs.dest(size), signed)?;
            }
        };

        Ok(())
    }

    fn push(&mut self, src: &Source) {
        self.text.push_str(&format!("\tpush {src}\n"));
    }

    fn pop(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tpop {dest}\n"));
    }

    fn ty_size(&self, ty: &Ty) -> usize {
        match ty {
            Ty::Int(int) if int == &IntTy::Isize => Self::BITNESS / 8,
            Ty::UInt(uint) if uint == &UintTy::Usize => Self::BITNESS / 8,
            Ty::Ptr(_) | Ty::Array(_) => Self::BITNESS / 8,
            Ty::Struct(id) => self.struct_size(*id),
            _ => ty.size(|ty| self.ty_size(ty)),
        }
    }

    fn struct_size(&self, id: Id) -> usize {
        let mut size = 0;
        let mut max = 0;

        match self.ctx.ir.get_node(id) {
            Node::Item(Item::Struct(fields)) => {
                for (_, ty) in *fields {
                    let ty_size = self.ty_size(ty);

                    if max < ty_size {
                        max = ty_size;
                    }

                    size = (size + ty_size).next_multiple_of(ty_size);
                }
            }
            _ => unreachable!(),
        }

        size.next_multiple_of(max)
    }

    fn set_ty_fields_offsets(&mut self, id: Id) {
        match self.ctx.ir.get_node(id) {
            Node::Item(Item::Struct(fields)) => {
                let mut offset: usize = 0;

                for (field, ty) in fields.iter() {
                    let size = self.ty_size(ty);

                    offset = offset.next_multiple_of(size);
                    self.fields_offsets
                        .entry(id)
                        .or_default()
                        .insert(field, Offset(offset as isize));
                    offset += size;
                }
            }
            _ => unreachable!(),
        }
    }

    fn stack_frame_size(&self, block: &Block) -> usize {
        let mut size = 0;

        for stmt in block.0 {
            match stmt {
                Stmt::Local(stmt) => {
                    size += self.ty_size(self.ctx.resolve_ty(stmt.ty));
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

    fn jcc(&mut self, label: &str, kind: Jump) {
        self.text.push_str(&format!("\t{kind} {label}\n"));
    }

    fn write_label(&mut self, label: &str) {
        self.text.push_str(&format!("{label}:\n"));
    }

    fn logical_or(
        &mut self,
        expr: &Expr,
        loc: &Location,
        labels: Option<(String, String)>,
    ) -> Result<(), Amd64AsmError> {
        let mut empty = false;
        let (set, end) = labels.unwrap_or_else(|| {
            empty = true;

            (self.label_gen.generate(), self.label_gen.generate())
        });
        let dest = loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?);

        match expr.kind {
            ExprKind::Binary(BinOp::LogicalOr, lhs, rhs) => {
                self.logical_or(lhs, loc, Some((set.clone(), end.clone())))?;
                self.logical_or(rhs, loc, Some((set.clone(), end.clone())))?;
            }
            _ => {
                self.expr(expr, Some(loc))?;
                self.cmp(&dest, &Source::Immediate(Immediate::UInt(0)));
                self.jcc(&set, Jump::NotEqual);
            }
        };

        if empty {
            self.mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
            self.jcc(&end, Jump::Unconditional);
            self.write_label(&set);
            self.mov(&Source::Immediate(Immediate::UInt(1)), &dest, false)?;
            self.write_label(&end);
        }

        Ok(())
    }

    fn logical_and(
        &mut self,
        expr: &Expr,
        loc: &Location,
        labels: Option<(String, String)>,
    ) -> Result<(), Amd64AsmError> {
        let mut empty = false;
        let (set, end) = labels.unwrap_or_else(|| {
            empty = true;

            (self.label_gen.generate(), self.label_gen.generate())
        });
        let dest = loc.dest(self.ty_size(self.ctx.resolve_ty(expr.ty)).try_into()?);

        match expr.kind {
            ExprKind::Binary(BinOp::LogicalAnd, lhs, rhs) => {
                self.logical_and(lhs, loc, Some((set.clone(), end.clone())))?;
                self.logical_and(rhs, loc, Some((set.clone(), end.clone())))?;
            }
            _ => {
                self.expr(expr, Some(loc))?;
                self.cmp(&dest, &Source::Immediate(Immediate::UInt(0)));
                self.jcc(&set, Jump::Equal);
            }
        };

        if empty {
            self.mov(&Source::Immediate(Immediate::UInt(1)), &dest, false)?;
            self.jcc(&end, Jump::Unconditional);
            self.write_label(&set);
            self.mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
            self.write_label(&end);
        }

        Ok(())
    }

    fn bitwise(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        op: BitwiseOp,
        signed: bool,
    ) -> Result<(), Amd64AsmError> {
        lhs.size().map(|size| assert_eq!(size, dest.size()));
        rhs.size().map(|size| assert_eq!(size, dest.size()));
        assert!(!(lhs == dest && rhs == dest));

        let lhs = if let Source::Immediate(_) = lhs {
            self.mov(lhs, &Register::Rax.resize(dest.size()).into(), signed)?;
            &Register::Rax.into()
        } else {
            lhs
        };

        self.text.push_str(&format!("\t{op} {lhs}, {rhs}\n"));

        if lhs != dest {
            self.mov(lhs, dest, signed)?;
        }

        Ok(())
    }

    fn shl(&mut self, dest: &Destination, src: &Source) -> Result<(), Amd64AsmError> {
        self.mov(src, &Register::Rcx.into(), false)?;
        self.text
            .push_str(&format!("\tshl {dest}, {}\n", Register::Cl));

        Ok(())
    }

    fn shr(&mut self, dest: &Destination, src: &Source) -> Result<(), Amd64AsmError> {
        self.mov(src, &Register::Rcx.into(), false)?;
        self.text
            .push_str(&format!("\tshr {dest}, {}\n", Register::Cl));

        Ok(())
    }

    fn lea(&mut self, dest: &Destination, address: &EffectiveAddress) {
        self.text.push_str(&format!("\tlea {dest}, {address}\n"));
    }

    fn negate(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tneg {dest}\n"));
    }

    fn bitwise_not(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tnot {dest}\n"));
    }

    fn inline_memcpy(
        &mut self,
        src: &EffectiveAddress,
        dest: &EffectiveAddress,
        size: usize,
    ) -> Result<(), Amd64AsmError> {
        self.mov(
            &Source::Immediate(Immediate::UInt(size as u64)),
            &Register::Rcx.into(),
            false,
        )?;
        self.lea(&Register::Rsi.into(), src);
        self.lea(&Register::Rdi.into(), dest);
        self.text.push_str("\trep movsb\n");

        Ok(())
    }
}
