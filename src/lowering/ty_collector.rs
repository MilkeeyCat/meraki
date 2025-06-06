use crate::{
    ast::{
        self, BinOp, Block, Expr, ExprKind, ExprLit, Stmt, StmtKind, UnOp, node_id::NodeId,
        visitor::Visitor,
    },
    ir::{self, Ty},
    ty_problem::{self, TyProblem},
};
use std::collections::HashMap;

const DUMMY_LOCAL_IDX: ir::LocalIdx = 0;

struct Collector<'a, 'ctx, 'ir> {
    lowering: &'a mut super::Lowering<'ctx, 'ir>,
    ty_problem: TyProblem<'ir>,
    node_id: NodeId,
    nodes_types: HashMap<NodeId, &'ir Ty<'ir>>,
}

impl<'a, 'ctx, 'ir> Collector<'a, 'ctx, 'ir> {
    fn new(lowering: &'a mut super::Lowering<'ctx, 'ir>, node_id: NodeId) -> Self {
        Self {
            lowering,
            ty_problem: TyProblem::new(),
            node_id,
            nodes_types: HashMap::new(),
        }
    }

    fn expr_ty(&mut self, expr: &Expr) -> &'ir Ty<'ir> {
        match &expr.kind {
            ExprKind::Binary { .. } => self
                .lowering
                .ctx
                .allocator
                .alloc(Ty::Infer(self.ty_problem.new_infer_ty_var())),
            ExprKind::Lit(lit) => match lit {
                ExprLit::Bool(_) => self.lowering.ctx.types.bool,
                ExprLit::String(_) => self
                    .lowering
                    .ctx
                    .allocator
                    .alloc(Ty::Ptr(self.lowering.ctx.types.u8)),
                _ => self
                    .lowering
                    .ctx
                    .allocator
                    .alloc(Ty::Infer(self.ty_problem.new_infer_ty_var())),
            },
            ExprKind::Ident(ident) => self.lowering.scopes.get_variable(&ident).unwrap().ty,
            _ => todo!(),
        }
    }

    fn tys_ty_var_id(&mut self, ty: &'ir Ty<'ir>) -> ty_problem::Id {
        match ty {
            Ty::Infer(id) => *id,
            ty => self.ty_problem.new_typed_ty_var(ty),
        }
    }

    fn lower_ty(&mut self, ty: ast::Ty) -> &'ir Ty<'ir> {
        match ty {
            ast::Ty::Infer => self
                .lowering
                .ctx
                .allocator
                .alloc(Ty::Infer(self.ty_problem.new_infer_ty_var())),
            _ => self.lowering.lower_ty(ty),
        }
    }
}

impl<'ast> Visitor<'ast> for Collector<'_, '_, '_> {
    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Local(variable) => {
                let ty = self.lower_ty(variable.ty.clone());

                if let Some(expr) = &mut variable.value {
                    self.visit_expr(expr);

                    let let_ty_var_id = self.tys_ty_var_id(ty);
                    let expr_ty_var_id = self.tys_ty_var_id(self.nodes_types[&expr.id]);

                    self.ty_problem.eq(let_ty_var_id, expr_ty_var_id);
                };

                self.nodes_types.insert(stmt.id, ty);
                self.lowering
                    .scopes
                    .insert_local(variable.name.clone(), ty, DUMMY_LOCAL_IDX);
            }
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);

                    let expr_ty_var_id = self.tys_ty_var_id(self.nodes_types[&expr.id]);
                    let ty = self.lowering.get_fn().ret_ty;
                    let ret_ty_var_id = self.tys_ty_var_id(ty);

                    self.ty_problem.eq(expr_ty_var_id, ret_ty_var_id);
                }
            }
            _ => unimplemented!(),
        }
    }

    fn visit_expr(&mut self, expr: &'ast mut Expr) {
        let ty = match &mut expr.kind {
            ExprKind::Binary { op, left, right } => {
                self.visit_expr(left.as_mut());
                self.visit_expr(right.as_mut());

                let lhs_ty_var_id = self.tys_ty_var_id(self.nodes_types[&left.id]);
                let rhs_ty_var_id = self.tys_ty_var_id(self.nodes_types[&right.id]);

                match op {
                    BinOp::Add => {
                        let ty = self.expr_ty(expr);
                        let ty_var_id = self.tys_ty_var_id(ty);

                        self.ty_problem
                            .bin_add(ty_var_id, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    BinOp::Sub => {
                        let ty = self.expr_ty(&expr);
                        let ty_var_id = self.tys_ty_var_id(ty);

                        self.ty_problem
                            .bin_sub(ty_var_id, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    _ => {
                        self.ty_problem.eq(lhs_ty_var_id, rhs_ty_var_id);

                        self.nodes_types[&left.id]
                    }
                }
            }
            ExprKind::Ident(ident) => self
                .lowering
                .scopes
                .get_symbol_ty(&ident, self.lowering.ctx)
                .unwrap(),
            ExprKind::Lit(_) => self.expr_ty(expr),
            ExprKind::Unary { op, expr } => {
                self.visit_expr(expr);

                match op {
                    UnOp::Address => self
                        .lowering
                        .ctx
                        .allocator
                        .alloc(Ty::Ptr(self.nodes_types[&expr.id])),
                    UnOp::Deref => {
                        let deref = &*self
                            .lowering
                            .ctx
                            .allocator
                            .alloc(Ty::Infer(self.ty_problem.new_infer_ty_var()));
                        let reference = self
                            .ty_problem
                            .new_typed_ty_var(self.lowering.ctx.allocator.alloc(Ty::Ptr(deref)));
                        let expr_ty_var_id = self.tys_ty_var_id(self.nodes_types[&expr.id]);

                        self.ty_problem.eq(expr_ty_var_id, reference);

                        deref
                    }
                    _ => self.nodes_types[&expr.id],
                }
            }
            ExprKind::Struct { name, fields } => {
                for (field, expr) in fields {
                    self.visit_expr(expr);

                    let expr_ty_var_id =
                        self.tys_ty_var_id(self.lowering.scopes.get_ty(&name).unwrap());
                    let field_ty_var_id = self.tys_ty_var_id(self.nodes_types[&expr.id]);

                    self.ty_problem
                        .field(expr_ty_var_id, field_ty_var_id, field.clone());
                }

                self.lowering.scopes.get_ty(&name).unwrap()
            }
            ExprKind::Field { expr, field } => {
                self.visit_expr(expr);

                let expr_ty = self.nodes_types[&expr.id];
                let expr_ty_var_id = self.tys_ty_var_id(expr_ty);
                let ty = self.lower_ty(ast::Ty::Infer);
                let field_ty_var_id = self.tys_ty_var_id(ty);

                self.ty_problem
                    .field(expr_ty_var_id, field_ty_var_id, field.clone());

                ty
            }
            ExprKind::Cast { expr, ty } => {
                self.visit_expr(expr);

                self.lower_ty(ty.clone())
            }
            ExprKind::FunctionCall {
                expr: func,
                arguments,
            } => {
                self.visit_expr(func);

                for (idx, arg) in arguments.iter_mut().enumerate() {
                    self.visit_expr(arg);

                    let expr_ty_var_id = self.tys_ty_var_id(self.nodes_types[&func.id]);
                    let arg_ty_var_id = self.tys_ty_var_id(self.nodes_types[&arg.id]);

                    self.ty_problem
                        .fn_argument(expr_ty_var_id, arg_ty_var_id, idx);
                }

                let fn_ty_var_id = self.tys_ty_var_id(self.nodes_types[&func.id]);
                let ty = self.lower_ty(ast::Ty::Infer);
                let expr_ty_var_id = self.tys_ty_var_id(ty);

                self.ty_problem.ret(fn_ty_var_id, expr_ty_var_id);

                ty
            }
            _ => unimplemented!(),
        };

        self.nodes_types.insert(expr.id, ty);
    }
}

pub fn collect_nodes_types<'ctx, 'ir>(
    lowering: &mut super::Lowering<'ctx, 'ir>,
    block: &mut Block,
    node_id: NodeId,
) -> HashMap<NodeId, &'ir Ty<'ir>> {
    let mut collector = Collector::new(lowering, node_id);
    collector.visit_block(block);
    let inferred_types = collector.ty_problem.solve(collector.lowering.ctx);

    fn resolve_ty<'ctx, 'ir>(
        lowering: &super::Lowering<'ctx, 'ir>,
        types: &HashMap<ty_problem::Id, &'ir Ty<'ir>>,
        ty: &'ir Ty<'ir>,
    ) -> &'ir Ty<'ir> {
        match ty {
            Ty::Infer(id) => resolve_ty(lowering, types, types[id]),
            Ty::Ptr(ty) => lowering
                .ctx
                .allocator
                .alloc(Ty::Ptr(resolve_ty(lowering, types, *ty))),
            ty => ty,
        }
    }

    collector
        .nodes_types
        .into_iter()
        .map(|(node_id, ty)| (node_id, resolve_ty(lowering, &inferred_types, ty)))
        .collect()
}
