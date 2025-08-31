use crate::{
    ast,
    codegen::module::ModuleCtx,
    ir::{Expr, ExprKind, ExprLit, ItemFn, Package, Stmt, StmtKind, SymbolId, Ty},
};
use std::collections::HashMap;
use tja::{
    FunctionIdx,
    hir::{self, BlockIdx, op},
};

struct FunctionCtx<'a, 'b, 'ir> {
    package: &'a Package<'ir>,
    mod_ctx: &'a mut ModuleCtx<'b, 'ir>,
    idx: FunctionIdx,
    cur_bb_idx: BlockIdx,
    locals: HashMap<SymbolId, hir::Operand>,
}

impl<'a, 'b, 'ir> FunctionCtx<'a, 'b, 'ir> {
    fn new(
        mod_ctx: &'a mut ModuleCtx<'b, 'ir>,
        id: SymbolId,
        func: &ItemFn<'ir>,
        package: &'a Package<'ir>,
    ) -> Self {
        let param_tys: Vec<_> = func
            .params
            .iter()
            .map(|(id, ty)| (id, mod_ctx.lower_ty(ty)))
            .collect();
        let fn_idx = mod_ctx.functions[&id];
        let mut module = mod_ctx.get_module();
        let mut func = module.get_fn(fn_idx);
        let bb_idx = func.create_block(None);
        let mut bb = func.get_block(bb_idx);
        let locals = param_tys
            .into_iter()
            .enumerate()
            .map(|(idx, (id, ty))| {
                let operand = bb.create_alloca(ty);

                bb.create_store(operand.clone(), hir::Operand::Local(idx.into()));

                ((*id).into(), operand)
            })
            .collect();

        Self {
            mod_ctx,
            idx: fn_idx,
            cur_bb_idx: bb_idx,
            locals,
            package,
        }
    }

    fn create_bb(&mut self) -> BlockIdx {
        let idx = self
            .mod_ctx
            .get_module()
            .get_fn(self.idx)
            .create_block(None);

        self.cur_bb_idx = idx;

        idx
    }

    fn get_bb(&mut self, bb_idx: BlockIdx) -> hir::basic_block::Wrapper<'_> {
        let hir::Wrapper { ty_storage, inner } = self.mod_ctx.get_module();
        let func = inner.get_fn_mut(self.idx);

        hir::basic_block::Wrapper {
            ty_storage,
            fn_locals: &mut func.locals,
            block: &mut func.blocks[bb_idx],
        }
    }

    fn get_cur_bb(&mut self) -> hir::basic_block::Wrapper<'_> {
        self.get_bb(self.cur_bb_idx)
    }

    fn lower_expr(&mut self, expr: &Expr<'ir>, rvalue: bool) -> Option<hir::Operand> {
        match expr.kind.as_ref() {
            ExprKind::Binary(op, lhs, rhs) => {
                let op = match op {
                    ast::BinOp::Add => op::BinOp::Add,
                    ast::BinOp::Sub => op::BinOp::Sub,
                    ast::BinOp::Mul => op::BinOp::Mul,
                    ast::BinOp::Div => {
                        if matches!(self.package.expr_tys[&expr.id.into()], Ty::UInt(_)) {
                            op::BinOp::UDiv
                        } else {
                            op::BinOp::SDiv
                        }
                    }
                    ast::BinOp::Assign => {
                        let lhs = self.lower_expr(lhs, false).unwrap();
                        let rhs = self.lower_expr(rhs, true).unwrap();

                        self.get_cur_bb().create_store(lhs.clone(), rhs);

                        return Some(lhs);
                    }
                    _ => unimplemented!(),
                };
                let lhs = self.lower_expr(lhs, true).unwrap();
                let rhs = self.lower_expr(rhs, true).unwrap();

                Some(self.get_cur_bb().create_bin(lhs, rhs, op))
            }
            //ExprKind::Unary(UnOp, Expr<'ir>),
            ExprKind::Ident(id) => {
                let ty = self.package.symbols[id].ty();
                let ty = self.mod_ctx.lower_ty(ty);

                Some(
                    self.locals
                        .get(id)
                        .cloned()
                        .map(|operand| {
                            if rvalue {
                                self.get_cur_bb().create_load(operand, ty)
                            } else {
                                operand
                            }
                        })
                        .or_else(|| {
                            self.mod_ctx.globals.get(id).map(|idx| {
                                hir::Operand::const_global(*idx, &self.mod_ctx.tja_ctx.ty_storage)
                            })
                        })
                        .or_else(|| {
                            self.mod_ctx.functions.get(id).map(|idx| {
                                hir::Operand::Const(
                                    tja::Const::Function(*idx),
                                    self.mod_ctx.tja_ctx.ty_storage.ptr_ty,
                                )
                            })
                        }),
                )
                .unwrap()
            }
            ExprKind::Lit(lit) => match lit {
                ExprLit::Int(value) => Some(hir::Operand::const_int(
                    (*value) as u64,
                    self.mod_ctx
                        .lower_ty(self.package.expr_tys[&expr.id.into()]),
                )),
                ExprLit::UInt(value) => Some(hir::Operand::const_int(
                    (*value) as u64,
                    self.mod_ctx
                        .lower_ty(self.package.expr_tys[&expr.id.into()]),
                )),
                ExprLit::Bool(value) => Some(hir::Operand::const_int(
                    (*value) as u64,
                    self.mod_ctx
                        .lower_ty(self.package.expr_tys[&expr.id.into()]),
                )),
                ExprLit::String(_) | ExprLit::Null => unimplemented!(),
            },
            ExprKind::Struct(ty, fields) => {
                let lowered_ty = self.mod_ctx.lower_ty(ty);
                let ptr = self.get_cur_bb().create_alloca(lowered_ty);
                let variant = &self.mod_ctx.ctx.get_adt(ty.adt_idx()).variants[0];

                for (name, expr) in fields {
                    let operand = self.lower_expr(expr, true).unwrap();
                    let mut bb = self.get_cur_bb();
                    let ptr = bb.create_gep(
                        lowered_ty,
                        ptr.clone(),
                        vec![
                            hir::Operand::const_i64(0, bb.ty_storage),
                            hir::Operand::const_i64(
                                variant.get_field_by_name(name).unwrap().0 as u64,
                                bb.ty_storage,
                            ),
                        ],
                    );

                    bb.create_store(ptr, operand);
                }

                Some(self.get_cur_bb().create_load(ptr, lowered_ty))
            }
            // This generates inefficient ir because GEP isn't utilized well
            // Currently it creates a new vreg for each field access, e.g.
            // foo.bar.baz would result in
            // %foo = gep %struct_ptr, 0, {foo_field_idx}
            // %bar = gep %foo, 0, {bar_field_idx}
            // %baz = gep %bar, 0, {baz_field_idx}
            // TODO: it would be nice to generate simply
            // %baz = gep %struct_ptr, 0, {foo_field_idx}, {bar_field_idx}, {baz_field_idx}
            ExprKind::Field(expr, field) => {
                let struct_ty = self.package.expr_tys[&expr.id.into()];
                let lowered_struct_ty = self.mod_ctx.lower_ty(struct_ty);
                let variant = &self.mod_ctx.ctx.get_adt(struct_ty.adt_idx()).variants[0];
                let (idx, field_def) = variant.get_field_by_name(field).unwrap();
                let lowered_field_ty = self.mod_ctx.lower_ty(field_def.ty);
                let ptr = self.lower_expr(expr, false).unwrap();
                let mut bb = self.get_cur_bb();
                let ptr = bb.create_gep(
                    lowered_struct_ty,
                    ptr,
                    vec![
                        hir::Operand::const_i64(0, bb.ty_storage),
                        hir::Operand::const_i64(idx as u64, bb.ty_storage),
                    ],
                );

                Some(bb.create_load(ptr, lowered_field_ty))
            }
            _ => unimplemented!(),
            //ExprKind::Cast(Expr<'ir>, &'ir Ty<'ir>),
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt<'ir>) {
        match &stmt.kind {
            StmtKind::Local(variable) => {
                let ty = self.package.symbols[&stmt.id.into()].ty();
                let ty = self.mod_ctx.lower_ty(ty);
                let value = variable
                    .value
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, true).unwrap());
                let mut bb = self.get_cur_bb();
                let operand = bb.create_alloca(ty);

                if let Some(value) = value {
                    bb.create_store(operand.clone(), value);
                }

                self.locals.insert(stmt.id.into(), operand);
            }
            StmtKind::Expr(expr) => _ = self.lower_expr(expr, true),
            StmtKind::Return(expr) => {
                let value = expr
                    .as_ref()
                    .map(|expr| self.lower_expr(expr, true))
                    .flatten();

                self.get_cur_bb().create_ret(value);
            }
            _ => unimplemented!(),
        }
    }
}

pub fn compile_function<'a, 'b, 'ir>(
    mod_ctx: &'a mut ModuleCtx<'b, 'ir>,
    id: SymbolId,
    func: &ItemFn<'ir>,
    package: &'a Package<'ir>,
) {
    let mut fn_ctx = FunctionCtx::new(mod_ctx, id, func, package);

    if let Some(block) = &func.block {
        for stmt in &block.0 {
            fn_ctx.lower_stmt(stmt);
        }
    }
}
