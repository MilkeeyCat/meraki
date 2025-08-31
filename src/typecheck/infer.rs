use crate::{
    Context,
    ast::BinOp,
    ir::{
        Expr, ExprKind, ExprLit, Id, Item, ItemKind, Stmt, StmtKind, Symbol, SymbolId, Ty, TyArray,
        Variable,
        ty::{AdtKind, InferTy},
        visitor::{Visitor, walk_expr, walk_item, walk_stmt},
    },
    typecheck::ty_problem::{self, TyProblem},
};
use std::collections::HashMap;

struct InferCtx<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    symbols: &'a HashMap<SymbolId, Symbol<'ir>>,
    types: HashMap<Id, &'ir Ty<'ir>>,
    ty_problem: TyProblem<'ir>,
}

impl<'ir> Visitor<'ir> for InferCtx<'_, 'ir> {
    fn visit_expr(&mut self, expr: &Expr<'ir>) {
        walk_expr(self, expr);

        let ty = match expr.kind.as_ref() {
            ExprKind::Binary(op, lhs, rhs) => match op {
                BinOp::Add => {
                    let ty = self.new_infer_ty();

                    self.ty_problem
                        .bin_add(ty, self.types[&lhs.id], self.types[&rhs.id]);

                    ty
                }
                BinOp::Sub => {
                    let ty = self.new_infer_ty();

                    self.ty_problem
                        .bin_sub(ty, self.types[&lhs.id], self.types[&rhs.id]);

                    ty
                }
                _ => {
                    self.ty_problem.eq(self.types[&lhs.id], self.types[&rhs.id]);

                    self.types[&lhs.id]
                }
            },
            ExprKind::Unary(op, expr) => todo!(),
            ExprKind::Ident(id) => self
                .types
                .get(&(*id).into())
                .map(|ty| *ty)
                .unwrap_or_else(|| self.symbols[&(*id).into()].ty()),
            ExprKind::Lit(lit) => match lit {
                ExprLit::Bool(_) => self.ctx.types.bool,
                ExprLit::String(_) => self.ctx.allocator.alloc(Ty::Ptr(self.ctx.types.u8)),
                ExprLit::Int(_) | ExprLit::UInt(_) => self.new_infer_int_ty(),
                ExprLit::Null => self.new_infer_ty(),
            },
            ExprKind::Struct(ty, fields) => {
                let adt = self.ctx.get_adt(ty.adt_idx());

                match adt.kind {
                    AdtKind::Struct => {
                        let variant = &adt.variants[0];

                        for (name, expr) in fields {
                            self.ty_problem.eq(
                                self.types[&expr.id],
                                variant.get_field_by_name(name).unwrap().1.ty,
                            );
                        }
                    }
                    _ => unimplemented!(),
                }

                ty
            }
            ExprKind::Field(expr, field) => {
                let ty = self.new_infer_ty();

                self.ty_problem
                    .field(self.types[&expr.id], ty, field.to_string());

                ty
            }
            _ => unimplemented!(),
        };

        self.types.insert(expr.id, ty);
    }

    fn visit_stmt(&mut self, stmt: &Stmt<'ir>) {
        walk_stmt(self, stmt);

        match &stmt.kind {
            StmtKind::Local(variable) => {
                self.infer_var_decl(stmt.id, variable);
            }
            StmtKind::Item(item) => unimplemented!(),
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr)
                }
            }
        }
    }

    fn visit_item(&mut self, item: &Item<'ir>) {
        walk_item(self, item);

        match &item.kind {
            ItemKind::Global(variable) => self.infer_var_decl(item.id, variable),
            ItemKind::Fn(_) => (),
        }
    }
}

impl<'ir> InferCtx<'_, 'ir> {
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

    fn new_infer_ty(&mut self) -> &'ir Ty<'ir> {
        self.ctx.allocator.alloc(Ty::Infer(Some(InferTy::TyVar(
            self.ty_problem.new_infer_ty_var(),
        ))))
    }

    fn new_infer_int_ty(&mut self) -> &'ir Ty<'ir> {
        self.ctx.allocator.alloc(Ty::Infer(Some(InferTy::IntVar(
            self.ty_problem.new_infer_ty_var(),
        ))))
    }

    fn infer_var_decl(&mut self, id: Id, variable: &Variable<'ir>) {
        let ty = self.lower_ty(variable.ty);

        if let Some(expr) = &variable.value {
            self.ty_problem.eq(ty, self.types[&expr.id]);
        }

        self.types.insert(id, ty);
    }
}

pub fn infer_types_in_item<'ir>(
    ctx: &mut Context<'ir>,
    item: &Item<'ir>,
    symbols: &HashMap<SymbolId, Symbol<'ir>>,
) -> HashMap<Id, &'ir Ty<'ir>> {
    let mut inferer = InferCtx {
        ctx,
        types: HashMap::new(),
        ty_problem: TyProblem::new(),
        symbols,
    };

    inferer.visit_item(item);

    let types = inferer.types;
    let infered_types = inferer.ty_problem.solve(ctx);

    types
        .into_iter()
        .map(|(id, ty)| (id, resolve_ty(ctx, &infered_types, ty)))
        .collect()
}

fn resolve_ty<'ir>(
    ctx: &mut Context<'ir>,
    types: &HashMap<ty_problem::Id, &'ir Ty<'ir>>,
    ty: &'ir Ty<'ir>,
) -> &'ir Ty<'ir> {
    match ty {
        Ty::Infer(ty) => resolve_ty(ctx, types, types[&ty.clone().unwrap().into()]),
        Ty::Ptr(ty) => ctx.allocator.alloc(Ty::Ptr(resolve_ty(ctx, types, *ty))),
        ty => ty,
    }
}
