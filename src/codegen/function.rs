use crate::{
    codegen::module::ModuleCtx,
    ir::{Expr, ExprKind, ExprLit, ItemFn, Package, Stmt, StmtKind, SymbolId},
};
use std::collections::HashMap;
use tja::{
    FunctionIdx,
    hir::{self, BlockIdx},
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

    fn lower_expr(&mut self, expr: &Expr<'ir>) -> Option<hir::Operand> {
        match expr.kind.as_ref() {
            //ExprKind::Binary(BinOp, Expr<'ir>, Expr<'ir>),
            //ExprKind::Unary(UnOp, Expr<'ir>),
            //ExprKind::Ident(SymbolId)
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
            _ => unimplemented!(),
            //ExprKind::Struct(Vec<(String, Expr<'ir>)>),
            //ExprKind::Field(Expr<'ir>, String),
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
                    .map(|expr| self.lower_expr(expr).unwrap());
                let mut bb = self.get_cur_bb();
                let operand = bb.create_alloca(ty);

                if let Some(value) = value {
                    bb.create_store(operand.clone(), value);
                }

                self.locals.insert(stmt.id.into(), operand);
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
