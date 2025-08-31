use crate::ir::{Block, Expr, ExprKind, Item, ItemFn, ItemKind, Stmt, StmtKind};

pub trait Visitor<'ir>: Sized {
    fn visit_expr(&mut self, expr: &Expr<'ir>) {
        walk_expr(self, expr)
    }

    fn visit_stmt(&mut self, stmt: &Stmt<'ir>) {
        walk_stmt(self, stmt);
    }

    fn visit_item(&mut self, item: &Item<'ir>) {
        walk_item(self, item);
    }

    fn visit_block(&mut self, block: &Block<'ir>) {
        walk_block(self, block);
    }
}

pub fn walk_expr<'ir, V: Visitor<'ir>>(visitor: &mut V, expr: &Expr<'ir>) {
    match expr.kind.as_ref() {
        ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ExprKind::Unary(_, expr) => {
            visitor.visit_expr(expr);
        }
        ExprKind::Cast(expr, _) => {
            visitor.visit_expr(expr);
        }
        ExprKind::Struct(_, fields) => {
            for (_, expr) in fields {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Field(expr, _) => {
            visitor.visit_expr(expr);
        }
        ExprKind::Lit(..) | ExprKind::Ident(..) => (),
    }
}

pub fn walk_stmt<'ir, V: Visitor<'ir>>(visitor: &mut V, stmt: &Stmt<'ir>) {
    match &stmt.kind {
        StmtKind::Local(variable) => {
            if let Some(expr) = &variable.value {
                visitor.visit_expr(expr);
            }
        }
        StmtKind::Item(item) => visitor.visit_item(item),
        StmtKind::Expr(expr) => visitor.visit_expr(expr),
        StmtKind::Return(expr) => {
            if let Some(expr) = expr {
                visitor.visit_expr(expr);
            }
        }
    }
}

pub fn walk_item<'ir, V: Visitor<'ir>>(visitor: &mut V, item: &Item<'ir>) {
    match &item.kind {
        ItemKind::Global(variable) => {
            if let Some(expr) = &variable.value {
                visitor.visit_expr(expr);
            }
        }
        ItemKind::Fn(ItemFn { block, .. }) => {
            if let Some(block) = block {
                visitor.visit_block(block);
            }
        }
    }
}

pub fn walk_block<'ir, V: Visitor<'ir>>(visitor: &mut V, block: &Block<'ir>) {
    for stmt in &block.0 {
        visitor.visit_stmt(stmt);
    }
}
