use crate::{
    Context,
    ir::{Id, Item, ItemKind, Package, Symbol, Ty, Variable, visitor::Visitor},
    typecheck::infer::infer_types_in_item,
};
use std::collections::HashMap;

struct TypeCheck<'a, 'ir> {
    types: &'a HashMap<Id, &'ir Ty<'ir>>,
}

impl<'ir> Visitor<'ir> for TypeCheck<'_, 'ir> {
    fn visit_item(&mut self, item: &Item<'ir>) {
        match &item.kind {
            ItemKind::Fn(func) => {
                if let Some(block) = &func.block {
                    self.visit_block(block);
                }
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
        let mut typecheck = TypeCheck { types: &types };

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
