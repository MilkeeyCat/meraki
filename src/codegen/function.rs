use super::Codegen;
use crate::{
    ast,
    ir::{self, FunctionIdx, Rvalue},
};
use std::{collections::HashMap, rc::Rc};
use tja::repr;

pub struct FunctionCtx<'a, 'b, 'ir> {
    ctx: &'a mut Codegen<'b, 'ir>,
    idx: FunctionIdx,
    locals: HashMap<ir::LocalIdx, repr::Operand>,
}

impl<'a, 'b, 'ir> FunctionCtx<'a, 'b, 'ir> {
    pub fn new(ctx: &'a mut Codegen<'b, 'ir>, idx: FunctionIdx) -> Self {
        let func = &ctx.module.functions[idx];
        let arg_count = func.arg_count;
        let empty = func.basic_blocks.is_empty();
        let locals: Vec<_> = func.locals.iter().map(|ty| ctx.lower_ty(ty)).collect();
        let ret_ty = ctx.lower_ty(func.ret_ty);
        let fn_idx = ctx.get_module().create_fn(
            func.name.clone(),
            locals[..func.arg_count].to_vec(),
            ret_ty,
        );

        ctx.functions.insert(idx, fn_idx);

        let mut module = ctx.get_module();
        let mut func = module.get_fn(fn_idx);
        let idx = func.create_block(None);
        let mut bb = func.get_block(idx);
        if !empty {
            bb.create_br(idx + 1);
        }

        let mut locals: HashMap<ir::LocalIdx, repr::Operand> = locals[arg_count..]
            .iter()
            .enumerate()
            .map(|(idx, ty)| (idx + arg_count, bb.create_alloca(*ty)))
            .collect();

        locals.extend(
            (0..arg_count)
                .into_iter()
                .map(|idx| (idx, repr::Operand::Local(idx))),
        );

        Self { ctx, idx, locals }
    }

    fn local_ty(&self, idx: ir::LocalIdx) -> &'ir ir::Ty<'ir> {
        self.ctx.module.functions[self.idx].locals[idx]
    }

    fn get_fn(&mut self) -> repr::Wrapper<&mut repr::Function> {
        let repr::Wrapper { ty_storage, inner } = self.ctx.tja_ctx.get_module(self.ctx.module_idx);

        repr::Wrapper {
            ty_storage,
            inner: inner.get_fn_mut(self.idx),
        }
    }

    fn get_basic_block(
        &mut self,
        bb_idx: repr::basic_block::BlockIdx,
    ) -> repr::basic_block::Wrapper {
        let repr::Wrapper { ty_storage, inner } = self.ctx.get_module();
        let func = inner.get_fn_mut(self.idx);

        repr::basic_block::Wrapper {
            ty_storage,
            fn_locals: &mut func.locals,
            block: &mut func.blocks[bb_idx],
        }
    }

    fn lower_place(
        &mut self,
        bb_idx: repr::basic_block::BlockIdx,
        place: &ir::Place,
    ) -> (repr::Operand, &'ir ir::Ty<'ir>) {
        let (mut operand, mut ty) = match place.storage {
            ir::Storage::Local(idx) => (self.locals[&idx].clone(), self.local_ty(idx)),
            ir::Storage::Global(idx) => (
                repr::Operand::Global(Rc::clone(&self.ctx.get_module().globals[idx])),
                self.ctx.module.globals[idx].ty,
            ),
        };

        for proj in &place.projection {
            operand = match proj {
                ir::Projection::Deref => {
                    ty = ty.pointee();
                    let ty = self.ctx.lower_ty(ty);

                    self.get_basic_block(bb_idx).create_load(operand, ty)
                }
                _ => todo!(),
            }
        }

        (operand, ty)
    }

    fn lower_rvalue(
        &mut self,
        bb_idx: repr::basic_block::BlockIdx,
        rvalue: &Rvalue<'ir>,
    ) -> repr::Operand {
        match rvalue {
            Rvalue::Use(operand) => self.lower_operand(bb_idx, operand).0,
            Rvalue::BinaryOp(op, lhs, rhs) => {
                let (lhs, ty) = self.lower_operand(bb_idx, lhs);
                let (rhs, _) = self.lower_operand(bb_idx, rhs);
                let op = match op {
                    ast::BinOp::Add => repr::op::BinOp::Add,
                    ast::BinOp::Sub => repr::op::BinOp::Sub,
                    ast::BinOp::Mul => repr::op::BinOp::Mul,
                    ast::BinOp::Div => {
                        if ty.signed() {
                            repr::op::BinOp::SDiv
                        } else {
                            repr::op::BinOp::UDiv
                        }
                    }
                    _ => unreachable!(),
                };

                self.get_basic_block(bb_idx).create_bin(lhs, rhs, op)
            }
            Rvalue::UnaryOp(_, _) => todo!(),
            Rvalue::Call { .. } => todo!(),
        }
    }

    fn lower_operand(
        &mut self,
        bb_idx: repr::basic_block::BlockIdx,
        operand: &ir::Operand<'ir>,
    ) -> (repr::Operand, &'ir ir::Ty<'ir>) {
        match operand {
            ir::Operand::Place(place) => self.load_place(bb_idx, place),
            ir::Operand::Const(valtree, ty) => (
                repr::Operand::Const(self.lower_value_tree(valtree, ty), self.ctx.lower_ty(ty)),
                ty,
            ),
        }
    }

    fn load_place(
        &mut self,
        bb_idx: repr::basic_block::BlockIdx,
        place: &ir::Place,
    ) -> (repr::Operand, &'ir ir::Ty<'ir>) {
        let (operand, ty) = self.lower_place(bb_idx, place);
        let ty_idx = self.ctx.lower_ty(ty);

        (
            self.get_basic_block(bb_idx).create_load(operand, ty_idx),
            ty,
        )
    }

    fn lower_value_tree(&self, valtree: &ir::ValueTree, ty: &'ir ir::Ty<'ir>) -> repr::Const {
        match valtree {
            ir::ValueTree::Leaf(c) => match c {
                ir::Const::Bool(value) => repr::Const::Int(*value as u64),
                ir::Const::I8(value) => repr::Const::Int(*value as u64),
                ir::Const::I16(value) => repr::Const::Int(*value as u64),
                ir::Const::I32(value) => repr::Const::Int(*value as u64),
                ir::Const::I64(value) => repr::Const::Int(*value as u64),
                ir::Const::U8(value) => repr::Const::Int(*value as u64),
                ir::Const::U16(value) => repr::Const::Int(*value as u64),
                ir::Const::U32(value) => repr::Const::Int(*value as u64),
                ir::Const::U64(value) => repr::Const::Int(*value as u64),
            },
            ir::ValueTree::Branch(valtrees) => {
                let tys: Vec<_> = match ty {
                    ir::Ty::Adt(idx) => {
                        let adt = self.ctx.ctx.get_adt(*idx);

                        match adt.kind {
                            ir::AdtKind::Struct => adt.variants[0]
                                .fields
                                .iter()
                                .map(|field| field.ty)
                                .collect(),
                            _ => todo!(),
                        }
                    }
                    _ => unreachable!(),
                };

                repr::Const::Aggregate(
                    valtrees
                        .iter()
                        .enumerate()
                        .map(|(idx, valtree)| self.lower_value_tree(valtree, tys[idx]))
                        .collect(),
                )
            }
        }
    }
}

pub fn compile_function<'a, 'b, 'ir>(ctx: &'a mut Codegen<'b, 'ir>, idx: FunctionIdx) {
    let mut ctx = FunctionCtx::new(ctx, idx);

    for bb in &ctx.ctx.module.functions[ctx.idx].basic_blocks {
        let bb_idx = ctx.get_fn().create_block(None);

        for stmt in &bb.statements {
            match stmt {
                ir::Statement::Assign(place, rvalue) => {
                    let (operand, _) = ctx.lower_place(bb_idx, place);
                    let value = ctx.lower_rvalue(bb_idx, rvalue);

                    ctx.get_basic_block(bb_idx).create_store(operand, value);
                }
            }
        }

        match &bb.terminator {
            ir::Terminator::Goto(idx) => ctx.get_basic_block(bb_idx).create_br(*idx),
            ir::Terminator::Return(value) => {
                let value = value.as_ref().map(|value| ctx.lower_rvalue(bb_idx, value));

                ctx.get_basic_block(bb_idx).create_ret(value);
            }
        }
    }
}
