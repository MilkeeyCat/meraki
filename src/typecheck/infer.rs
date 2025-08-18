use crate::{
    Context,
    ast::BinOp,
    ir::{
        Expr, ExprKind, ExprLit, Id, Item, Stmt, StmtKind, Ty, TyArray,
        visitor::{Visitor, walk_expr, walk_stmt},
    },
    typecheck::ty_problem::{self, TyProblem},
};
use std::collections::HashMap;

struct Inferer<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    id_to_ty: HashMap<Id, &'ir Ty<'ir>>,
    ty_problem: TyProblem<'ir>,
}

impl<'ir> Visitor<'ir> for Inferer<'_, 'ir> {
    fn visit_expr(&mut self, expr: &Expr<'ir>) {
        walk_expr(self, expr);

        let ty = match expr.kind.as_ref() {
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_ty_var_id = self.tys_ty_var_id(self.id_to_ty[&lhs.id]);
                let rhs_ty_var_id = self.tys_ty_var_id(self.id_to_ty[&rhs.id]);

                match op {
                    BinOp::Add => {
                        let ty = self.new_infer_ty();
                        let ty_var_id = self.tys_ty_var_id(ty);

                        self.ty_problem
                            .bin_add(ty_var_id, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    BinOp::Sub => {
                        let ty = self.new_infer_ty();
                        let ty_var_id = self.tys_ty_var_id(ty);

                        self.ty_problem
                            .bin_sub(ty_var_id, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    _ => {
                        self.ty_problem.eq(lhs_ty_var_id, rhs_ty_var_id);

                        self.id_to_ty[&lhs.id]
                    }
                }
            }
            ExprKind::Unary(op, expr) => todo!(),
            ExprKind::Ident(id) => self.id_to_ty[id],
            ExprKind::Lit(lit) => match lit {
                ExprLit::Bool(_) => self.ctx.types.bool,
                ExprLit::String(_) => self.ctx.allocator.alloc(Ty::Ptr(self.ctx.types.u8)),
                _ => self.new_infer_ty(),
            },
            _ => unimplemented!(),
        };

        self.id_to_ty.insert(expr.id, ty);
    }

    fn visit_stmt(&mut self, stmt: &Stmt<'ir>) {
        walk_stmt(self, stmt);

        match &stmt.kind {
            StmtKind::Local(variable) => {
                let ty = self.lower_ty(variable.ty);

                if let Some(expr) = &variable.value {
                    let let_ty_var_id = self.tys_ty_var_id(ty);
                    let expr_ty_var_id = self.tys_ty_var_id(self.id_to_ty[&expr.id]);

                    self.ty_problem.eq(let_ty_var_id, expr_ty_var_id);
                }

                self.id_to_ty.insert(stmt.id, ty);
            }
            StmtKind::Item(item) => unimplemented!(),
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Return(expr) => unimplemented!(),
        }
    }
}

impl<'ir> Inferer<'_, 'ir> {
    fn lower_ty(&mut self, ty: &'ir Ty<'ir>) -> &'ir Ty<'ir> {
        match ty {
            Ty::Ptr(ty) => self.ctx.allocator.alloc(Ty::Ptr(self.lower_ty(ty))),
            Ty::Array(TyArray { ty, len }) => self.ctx.allocator.alloc(Ty::Array(TyArray {
                ty: self.lower_ty(ty),
                len: *len,
            })),
            Ty::Infer(None) => self.new_infer_ty(),
            ty => ty,
        }
    }

    fn tys_ty_var_id(&mut self, ty: &'ir Ty<'ir>) -> ty_problem::Id {
        match ty {
            Ty::Infer(id) => id.unwrap(),
            ty => self.ty_problem.new_typed_var(ty),
        }
    }

    fn new_infer_ty(&mut self) -> &'ir Ty<'ir> {
        self.ctx
            .allocator
            .alloc(Ty::Infer(Some(self.ty_problem.new_infer_var())))
    }
}

pub fn infer_types_in_item<'ir>(
    ctx: &mut Context<'ir>,
    id_to_ty: HashMap<Id, &'ir Ty<'ir>>,
    ty_problem: TyProblem<'ir>,
    item: &Item<'ir>,
) -> HashMap<Id, &'ir Ty<'ir>> {
    let mut inferer = Inferer {
        ctx,
        id_to_ty,
        ty_problem,
    };

    inferer.visit_item(item);

    let id_to_ty = inferer.id_to_ty;
    let ty_var_to_ty = inferer.ty_problem.solve(ctx);

    id_to_ty
        .into_iter()
        .map(|(id, ty)| (id, resolve_ty(ctx, &ty_var_to_ty, ty)))
        .collect()
}

fn resolve_ty<'ir>(
    ctx: &mut Context<'ir>,
    types: &HashMap<ty_problem::Id, &'ir Ty<'ir>>,
    ty: &'ir Ty<'ir>,
) -> &'ir Ty<'ir> {
    match ty {
        Ty::Infer(id) => resolve_ty(ctx, types, types[&id.unwrap()]),
        Ty::Ptr(ty) => ctx.allocator.alloc(Ty::Ptr(resolve_ty(ctx, types, *ty))),
        ty => ty,
    }
}
