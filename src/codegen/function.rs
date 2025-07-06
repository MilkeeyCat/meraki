use super::Codegen;
use crate::{
    ast,
    ir::{self, FunctionIdx, Rvalue},
};
use std::collections::HashMap;
use tja::hir;

pub struct FunctionCtx<'a, 'b, 'ir> {
    ctx: &'a mut Codegen<'b, 'ir>,
    idx: FunctionIdx,
    locals: HashMap<ir::LocalIdx, hir::Operand>,
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

        let locals: HashMap<ir::LocalIdx, hir::Operand> = locals
            .into_iter()
            .enumerate()
            .map(|(idx, ty)| {
                let operand = bb.create_alloca(ty);

                if idx < arg_count {
                    bb.create_store(operand.clone(), hir::Operand::Local(idx));
                }

                (idx, operand)
            })
            .collect();

        Self {
            ctx,
            idx: fn_idx,
            locals,
        }
    }

    fn local_ty(&self, idx: ir::LocalIdx) -> &'ir ir::Ty<'ir> {
        self.ctx.module.functions[self.idx].locals[idx]
    }

    fn get_fn(&mut self) -> hir::Wrapper<&mut hir::Function> {
        let hir::Wrapper { ty_storage, inner } = self.ctx.tja_ctx.get_module(self.ctx.module_idx);

        hir::Wrapper {
            ty_storage,
            inner: inner.get_fn_mut(self.idx),
        }
    }

    fn get_basic_block(&mut self, bb_idx: hir::basic_block::BlockIdx) -> hir::basic_block::Wrapper {
        let hir::Wrapper { ty_storage, inner } = self.ctx.get_module();
        let func = inner.get_fn_mut(self.idx);

        hir::basic_block::Wrapper {
            ty_storage,
            fn_locals: &mut func.locals,
            block: &mut func.blocks[bb_idx],
        }
    }

    fn lower_place(
        &mut self,
        bb_idx: hir::basic_block::BlockIdx,
        place: &ir::Place,
    ) -> (hir::Operand, &'ir ir::Ty<'ir>) {
        let (mut operand, mut ty) = match place.storage {
            ir::Storage::Local(idx) => (self.locals[&idx].clone(), self.local_ty(idx)),
            ir::Storage::Global(idx) => (
                hir::Operand::const_global(idx, &self.ctx.tja_ctx.ty_storage),
                self.ctx.module.globals[idx].ty,
            ),
        };

        for proj in &place.projection {
            operand = match proj {
                ir::Projection::Deref => {
                    let lowered_ty = self.ctx.lower_ty(ty);
                    let operand = self
                        .get_basic_block(bb_idx)
                        .create_load(operand, lowered_ty);

                    ty = ty.pointee();

                    operand
                }
                ir::Projection::Field(idx) => {
                    let lowered_ty = self.ctx.lower_ty(ty);
                    let mut bb = self.get_basic_block(bb_idx);

                    let operand = bb.create_gep(
                        lowered_ty,
                        operand,
                        vec![
                            hir::Operand::const_i64(0, &bb.ty_storage),
                            hir::Operand::const_i64(*idx as u64, &bb.ty_storage),
                        ],
                    );

                    ty = self.ctx.ctx.get_adt(ty.adt_idx()).variants[0].fields[*idx].ty;

                    operand
                }
                _ => todo!(),
            }
        }

        (operand, ty)
    }

    fn lower_rvalue(
        &mut self,
        bb_idx: hir::basic_block::BlockIdx,
        rvalue: &Rvalue<'ir>,
    ) -> hir::Operand {
        match rvalue {
            Rvalue::Use(operand) => self.lower_operand(bb_idx, operand).0,
            Rvalue::Ptr(place) => self.lower_place(bb_idx, place).0,
            Rvalue::BinaryOp(op, lhs, rhs) => {
                let (lhs, ty) = self.lower_operand(bb_idx, lhs);
                let (rhs, _) = self.lower_operand(bb_idx, rhs);
                let op = match op {
                    ast::BinOp::Add => hir::op::BinOp::Add,
                    ast::BinOp::Sub => hir::op::BinOp::Sub,
                    ast::BinOp::Mul => hir::op::BinOp::Mul,
                    ast::BinOp::Div => {
                        if ty.signed() {
                            hir::op::BinOp::SDiv
                        } else {
                            hir::op::BinOp::UDiv
                        }
                    }
                    _ => unreachable!(),
                };

                self.get_basic_block(bb_idx).create_bin(lhs, rhs, op)
            }
            Rvalue::UnaryOp(_, _) => todo!(),
            Rvalue::Call { operand, args } => {
                let (operand, ty) = self.lower_operand(bb_idx, operand);
                let ret_ty = match ty {
                    ir::Ty::Fn(_, ty) => ty,
                    _ => unreachable!(),
                };
                let ty = self.ctx.lower_ty(ret_ty);
                let args = args
                    .iter()
                    .map(|rvalue| self.lower_rvalue(bb_idx, rvalue))
                    .collect();

                //FIXME: what to do when the function returns void? what Operand to return?
                self.get_basic_block(bb_idx)
                    .create_call(operand, ty, args)
                    .unwrap_or_else(|| hir::Operand::const_i8(0, &self.ctx.tja_ctx.ty_storage))
            }
        }
    }

    fn lower_operand(
        &mut self,
        bb_idx: hir::basic_block::BlockIdx,
        operand: &ir::Operand<'ir>,
    ) -> (hir::Operand, &'ir ir::Ty<'ir>) {
        match operand {
            ir::Operand::Place(place) => self.load_place(bb_idx, place),
            ir::Operand::Const(valtree, ty) => (
                hir::Operand::Const(self.lower_value_tree(valtree, ty), self.ctx.lower_ty(ty)),
                ty,
            ),
        }
    }

    fn load_place(
        &mut self,
        bb_idx: hir::basic_block::BlockIdx,
        place: &ir::Place,
    ) -> (hir::Operand, &'ir ir::Ty<'ir>) {
        let (operand, ty) = self.lower_place(bb_idx, place);
        let ty_idx = self.ctx.lower_ty(ty);

        (
            self.get_basic_block(bb_idx).create_load(operand, ty_idx),
            ty,
        )
    }

    fn lower_value_tree(&self, valtree: &ir::ValueTree, ty: &'ir ir::Ty<'ir>) -> hir::Const {
        match valtree {
            ir::ValueTree::Leaf(c) => match c {
                ir::Const::Bool(value) => hir::Const::Int(*value as u64),
                ir::Const::I8(value) => hir::Const::Int(*value as u64),
                ir::Const::I16(value) => hir::Const::Int(*value as u64),
                ir::Const::I32(value) => hir::Const::Int(*value as u64),
                ir::Const::I64(value) => hir::Const::Int(*value as u64),
                ir::Const::U8(value) => hir::Const::Int(*value as u64),
                ir::Const::U16(value) => hir::Const::Int(*value as u64),
                ir::Const::U32(value) => hir::Const::Int(*value as u64),
                ir::Const::U64(value) => hir::Const::Int(*value as u64),
                ir::Const::Function(idx) => hir::Const::Function(*idx),
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

                hir::Const::Aggregate(
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
