use crate::{
    Context,
    ir::{
        Id, Item, ItemKind, Package, Stmt, StmtKind, Symbol, Ty, Variable,
        visitor::{Visitor, walk_stmt},
    },
    typecheck::infer::infer_types_in_item,
};
use std::collections::HashMap;

struct TypeCheck<'a, 'ir> {
    types: &'a HashMap<Id, &'ir Ty<'ir>>,
    ret_ty: Option<&'ir Ty<'ir>>,
}

impl<'ir> Visitor<'ir> for TypeCheck<'_, 'ir> {
    fn visit_stmt(&mut self, stmt: &Stmt<'ir>) {
        walk_stmt(self, stmt);

        match &stmt.kind {
            StmtKind::Local(variable) => {
                if let Some(expr) = &variable.value {
                    let lhs = self.types[&stmt.id];
                    let rhs = self.types[&expr.id];

                    assert!(
                        lhs == rhs,
                        "Type mismatch, expected: {lhs:?} but got {rhs:?}",
                    );
                }
            }
            StmtKind::Item(_) | StmtKind::Expr(_) => (),
            StmtKind::Return(expr) => {
                let ret_ty = self.ret_ty.unwrap();
                let expr_ty = &expr
                    .as_ref()
                    .map(|expr| self.types[&expr.id])
                    .unwrap_or(&&Ty::Void);

                assert!(
                    *expr_ty == ret_ty,
                    "Return type mismatch: expected {ret_ty:?} but got {expr_ty:?}"
                );
            }
        }
    }

    fn visit_item(&mut self, item: &Item<'ir>) {
        match &item.kind {
            ItemKind::Fn(func) => {
                self.ret_ty = Some(func.ret_ty);

                if let Some(block) = &func.block {
                    self.visit_block(block);
                }

                self.ret_ty = None;
            }
            ItemKind::Global(variable) => self.typecheck_var_decl(item.id, variable),
        }
    }
}

impl TypeCheck<'_, '_> {
    fn typecheck_var_decl(&self, id: Id, variable: &Variable) {
        if let Some(expr) = &variable.value {
            assert_eq!(
                self.types[&id], self.types[&expr.id],
                "Variable definition type mismatch!"
            );
        }
    }
}

pub fn typecheck<'ir>(ctx: &mut Context<'ir>, package: &mut Package<'ir>) {
    for item in &package.items {
        let types = infer_types_in_item(ctx, item, &package.symbols);
        let mut typecheck = TypeCheck {
            types: &types,
            ret_ty: None,
        };

        typecheck.visit_item(item);

        for (id, new_ty) in types {
            if let Some(symbol) = package.symbols.get_mut(&id.into()) {
                match symbol {
                    Symbol::Variable(ty) => *ty = new_ty,
                    Symbol::Fn { .. } => (),
                }
            } else {
                package.expr_tys.insert(id.into(), new_ty);
            }
        }
    }
}
