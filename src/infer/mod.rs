mod type_problem;

use crate::{
    ir::{Expr, ExprKind, Ir, Item, ItemFn, Node, Stmt, Ty},
    parser::BinOp,
};
use type_problem::{TyProblem, TyVar};

#[derive(PartialEq, Debug)]
enum StmtOrExpr<'ir> {
    Expr(&'ir Expr<'ir>),
    Stmt(&'ir Stmt<'ir>),
}

pub struct InferCtx<'a, 'ir> {
    ir: &'a Ir<'ir>,
    ty_problem: TyProblem<'ir>,
    ty_vars: Vec<(StmtOrExpr<'ir>, type_problem::Id)>,
    ret_ty: Option<&'ir Ty<'ir>>,
}

impl<'a, 'ir> InferCtx<'a, 'ir> {
    pub fn new(ir: &'a Ir<'ir>) -> Self {
        Self {
            ir,
            ty_problem: TyProblem::default(),
            ty_vars: Vec::new(),
            ret_ty: None,
        }
    }

    fn find_or_push_infer_ty_var(&mut self, item: StmtOrExpr<'ir>) -> type_problem::Id {
        match self
            .ty_vars
            .iter()
            .find(|(expr, _)| expr == &item)
            .map(|(_, id)| id)
        {
            Some(id) => *id,
            None => self.ty_problem.new_ty_var(TyVar::Infer),
        }
    }

    fn visit_expr(&mut self, expr: &'ir Expr<'ir>) -> type_problem::Id {
        let id = match expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_ty_var = self.visit_expr(lhs);
                let rhs_ty_var = self.visit_expr(rhs);

                self.ty_vars.push((StmtOrExpr::Expr(lhs), lhs_ty_var));
                self.ty_vars.push((StmtOrExpr::Expr(rhs), rhs_ty_var));

                match &op {
                    BinOp::Add => self.ty_problem.bin_add(lhs_ty_var, rhs_ty_var),
                    BinOp::Sub => self.ty_problem.bin_sub(lhs_ty_var, rhs_ty_var),
                    _ => self.ty_problem.eq(lhs_ty_var, rhs_ty_var),
                };

                lhs_ty_var
            }
            ExprKind::Lit(_) => self.ty_problem.new_ty_var(expr.ty().into()),
            ExprKind::Ident(id) => match self.ir.get_node(id) {
                Node::Stmt(stmt) => self.find_or_push_infer_ty_var(StmtOrExpr::Stmt(stmt)),
                _ => unreachable!(),
            },
        };

        self.ty_vars.push((StmtOrExpr::Expr(expr), id));

        id
    }

    fn infer_stmt(&mut self, stmt: &'ir Stmt<'ir>) {
        match stmt {
            Stmt::Local(var) => {
                let ty_var = self.ty_problem.new_ty_var(var.ty().into());
                let expr_ty_var = if let Some(expr) = &var.initializer {
                    self.visit_expr(expr)
                } else {
                    self.ty_problem.new_ty_var(TyVar::Infer)
                };

                self.ty_vars.push((StmtOrExpr::Stmt(stmt), ty_var));
                self.ty_problem.eq(ty_var, expr_ty_var);
            }
            Stmt::Item(item) => self.infer_item(item),
            Stmt::Expr(expr) => _ = self.visit_expr(expr),
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let expr_ty_var = self.visit_expr(expr);
                    let ret_ty_var = self
                        .ty_problem
                        .new_ty_var(TyVar::Typed(self.ret_ty.unwrap()));

                    self.ty_problem.eq(expr_ty_var, ret_ty_var);
                }
            }
        }
    }

    fn infer_fn(&mut self, item: &ItemFn<'ir>) {
        self.ret_ty = Some(item.signature.ret_ty);
        for stmt in item.block.0 {
            self.infer_stmt(stmt);
        }
        self.ret_ty = None;

        let ty_problem = std::mem::take(&mut self.ty_problem);
        let ty_vars = ty_problem.solve();
        let replace = std::mem::take(&mut self.ty_vars);

        replace.into_iter().for_each(|(item, id)| {
            let ty_var = &ty_vars[id.0];

            assert!(ty_var.ty().unwrap() != &Ty::Infer, "Failed to infer types");
            let ty = ty_var.ty().unwrap();

            match item {
                StmtOrExpr::Expr(expr) => {
                    expr.set_ty(ty);
                }
                StmtOrExpr::Stmt(stmt) => match stmt {
                    Stmt::Local(stmt) => stmt.set_ty(ty),
                    _ => unreachable!(),
                },
            }
        });
    }

    fn infer_item(&mut self, item: &Item<'ir>) {
        match item {
            Item::Fn(item) => self.infer_fn(item),
            Item::Global(_) => (),
        }
    }

    pub fn infer(mut self) {
        self.ir.iter_items().for_each(|item| self.infer_item(&item));
    }
}
