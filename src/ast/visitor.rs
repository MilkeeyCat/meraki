use super::{Block, Expr, ExprKind, Item, ItemKind, Stmt, StmtKind, Ty};

pub trait Visitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast mut Expr) {
        walk_expr(self, expr)
    }

    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_item(&mut self, item: &'ast mut Item) {
        walk_item(self, item);
    }

    fn visit_block(&mut self, block: &'ast mut Block) {
        walk_block(self, block);
    }

    fn visit_ty(&mut self, ty: &'ast mut Ty) {
        walk_ty(self, ty);
    }
}

pub fn walk_expr<'ast, T: Visitor<'ast>>(visitor: &mut T, expr: &'ast mut Expr) {
    match &mut expr.kind {
        ExprKind::Binary { left, right, .. } => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        ExprKind::Unary { expr, .. } => {
            visitor.visit_expr(expr);
        }
        ExprKind::Cast { expr, ty } => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        ExprKind::Struct { fields, .. } => {
            for (_, expr) in fields {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Array(exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Field { expr, .. } => {
            visitor.visit_expr(expr);
        }
        ExprKind::StructMethod {
            expr, arguments, ..
        } => {
            visitor.visit_expr(expr);
            for expr in arguments {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::ArrayAccess { expr, index } => {
            visitor.visit_expr(expr);
            visitor.visit_expr(index);
        }
        ExprKind::FunctionCall { expr, arguments } => {
            visitor.visit_expr(expr);
            for expr in arguments {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Lit(..) | ExprKind::Ident(..) | ExprKind::MacroCall { .. } => (),
    }
}

pub fn walk_stmt<'ast, T: Visitor<'ast>>(visitor: &mut T, stmt: &'ast mut Stmt) {
    match &mut stmt.kind {
        StmtKind::Local(variable) => {
            visitor.visit_ty(&mut variable.ty);
            if let Some(expr) = &mut variable.value {
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
        StmtKind::If {
            condition,
            consequence,
            alternative,
        } => {
            visitor.visit_expr(condition);
            visitor.visit_block(consequence);
            if let Some(alternative) = alternative {
                visitor.visit_block(alternative);
            }
        }
        StmtKind::While { condition, block } => {
            visitor.visit_expr(condition);
            visitor.visit_block(block);
        }
        StmtKind::For {
            initializer,
            condition,
            increment,
            block,
        } => {
            if let Some(initializer) = initializer {
                visitor.visit_stmt(initializer);
            }
            if let Some(condition) = condition {
                visitor.visit_expr(condition);
            }
            if let Some(increment) = increment {
                visitor.visit_expr(increment);
            }
            visitor.visit_block(block);
        }
        StmtKind::Continue | StmtKind::Break => (),
    }
}

pub fn walk_item<'ast, T: Visitor<'ast>>(visitor: &mut T, item: &'ast mut Item) {
    match &mut item.kind {
        ItemKind::Global(variable) => {
            visitor.visit_ty(&mut variable.ty);
            if let Some(expr) = &mut variable.value {
                visitor.visit_expr(expr);
            }
        }
        ItemKind::Fn {
            ret_ty,
            params,
            block,
            ..
        } => {
            visitor.visit_ty(ret_ty);
            for (_, ty) in params {
                visitor.visit_ty(ty);
            }
            if let Some(block) = block {
                visitor.visit_block(block);
            }
        }
        ItemKind::Struct { fields, .. } => {
            for (_, ty) in fields {
                visitor.visit_ty(ty);
            }
        }
    }
}

pub fn walk_block<'ast, T: Visitor<'ast>>(visitor: &mut T, block: &'ast mut Block) {
    for stmt in &mut block.stmts {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_ty<'ast, T: Visitor<'ast>>(visitor: &mut T, ty: &'ast mut Ty) {
    match ty {
        Ty::Ptr(ty) => visitor.visit_ty(ty),
        Ty::Array { ty, .. } => visitor.visit_ty(ty),
        Ty::Fn(params, ret_ty) => {
            for ty in params {
                visitor.visit_ty(ty);
            }
            visitor.visit_ty(ret_ty);
        }
        Ty::Null | Ty::Void | Ty::Bool | Ty::Int(_) | Ty::UInt(_) | Ty::Ident(_) | Ty::Infer => (),
    }
}
