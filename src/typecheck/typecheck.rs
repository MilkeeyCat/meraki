use crate::{
    Context,
    ir::{Id, ItemFn, ItemKind, Package, Ty},
    typecheck::{infer::infer_types_in_item, ty_problem::TyProblem},
};
use std::collections::HashMap;

fn collect_global_info<'ir>(
    ctx: &mut Context<'ir>,
    package: &Package<'ir>,
) -> (HashMap<Id, &'ir Ty<'ir>>, TyProblem<'ir>) {
    let mut id_to_ty = HashMap::new();
    let mut ty_problem = TyProblem::new();

    for item in &package.root_items {
        let ty = match &item.kind {
            ItemKind::Fn(ItemFn { params, ret_ty, .. }) => ctx.allocator.alloc(Ty::Fn(
                ctx.allocator
                    .alloc_slice_copy(&params.iter().map(|(_, ty)| *ty).collect::<Vec<_>>()),
                ret_ty,
            )),
            ItemKind::Global(variable) => variable.ty,
        };

        id_to_ty.insert(
            item.id,
            &*ctx
                .allocator
                .alloc(Ty::Infer(Some(ty_problem.new_typed_var(ty)))),
        );
    }

    (id_to_ty, ty_problem)
}

struct TypeCheck<'ir> {
    id_to_ty: HashMap<Id, &'ir Ty<'ir>>,
    ty_problem: TyProblem<'ir>,
}

pub fn typecheck<'ir>(ctx: &mut Context<'ir>, package: &Package<'ir>) -> HashMap<Id, &'ir Ty<'ir>> {
    let (id_to_ty, ty_problem) = collect_global_info(ctx, package);

    for item in &package.root_items {
        let types = infer_types_in_item(ctx, id_to_ty.clone(), ty_problem.clone(), item);
        dbg!(&types);
    }

    todo!();
}
