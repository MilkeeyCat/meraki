use crate::{
    Context,
    ast::IntTy,
    ir::{Ir, Item, Node, OrderedMap, Ty},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Id(usize);

#[derive(Debug, Clone)]
pub enum TyVar<'ir> {
    Typed(&'ir Ty<'ir>),
    Infer(Id),
}

impl<'ir> TyVar<'ir> {
    pub fn ty(&self) -> Option<&'ir Ty<'ir>> {
        match self {
            Self::Typed(ty) => Some(ty),
            Self::Infer(_) => None,
        }
    }
}

impl<'ir> From<&'ir Ty<'ir>> for TyVar<'ir> {
    fn from(value: &'ir Ty<'ir>) -> Self {
        match value {
            Ty::Infer(id) => Self::Infer(*id),
            ty => Self::Typed(ty),
        }
    }
}

#[derive(Debug)]
enum Constraint<'ir> {
    Eq(Id, Id),
    BinAdd {
        expr: Id,
        lhs: Id,
        rhs: Id,
    },
    BinSub {
        expr: Id,
        lhs: Id,
        rhs: Id,
    },
    Field {
        expr: Id,
        field_ty: Id,
        field: &'ir str,
    },
}

#[derive(Debug)]
pub struct TyProblem<'ir> {
    ty_vars: Vec<TyVar<'ir>>,
    constraints: Vec<Constraint<'ir>>,
}

impl<'ir> TyProblem<'ir> {
    pub fn new() -> Self {
        Self {
            ty_vars: Vec::new(),
            constraints: Vec::new(),
        }
    }

    fn new_ty_var(&mut self, ty_var: TyVar<'ir>) -> Id {
        let i = self.ty_vars.len();

        self.ty_vars.push(ty_var);

        Id(i)
    }

    pub fn new_infer_ty_var(&mut self) -> Id {
        let i = self.ty_vars.len();

        self.new_ty_var(TyVar::Infer(Id(i)))
    }

    pub fn new_typed_ty_var(&mut self, ty: &'ir Ty<'ir>) -> Id {
        self.new_ty_var(TyVar::Typed(ty))
    }

    pub fn get_ty_var(&self, id: Id) -> &TyVar<'ir> {
        &self.ty_vars[id.0]
    }

    pub fn get_ty_var_mut(&mut self, id: Id) -> &mut TyVar<'ir> {
        &mut self.ty_vars[id.0]
    }

    pub fn eq(&mut self, lhs: Id, rhs: Id) {
        self.constraints.push(Constraint::Eq(lhs, rhs));
    }

    pub fn bin_add(&mut self, expr: Id, lhs: Id, rhs: Id) {
        self.constraints.push(Constraint::BinAdd { expr, lhs, rhs });
    }

    pub fn bin_sub(&mut self, expr: Id, lhs: Id, rhs: Id) {
        self.constraints.push(Constraint::BinSub { expr, lhs, rhs });
    }

    pub fn field(&mut self, expr_ty_var: Id, field_ty: Id, field: &'ir str) {
        self.constraints.push(Constraint::Field {
            expr: expr_ty_var,
            field_ty,
            field,
        });
    }

    fn unify(&mut self, lhs: TyVar<'ir>, rhs: TyVar<'ir>) -> bool {
        match (lhs, rhs) {
            (TyVar::Infer(lhs), TyVar::Infer(rhs)) => {
                self.eq(lhs, rhs);

                false
            }
            (TyVar::Typed(ty), TyVar::Infer(id)) | (TyVar::Infer(id), TyVar::Typed(ty)) => {
                *self.get_ty_var_mut(id) = TyVar::Typed(ty);

                true
            }
            (TyVar::Typed(lhs), TyVar::Typed(rhs)) => match (lhs, rhs) {
                (Ty::Ptr(lhs), Ty::Ptr(rhs)) => self.unify((*lhs).into(), (*rhs).into()),
                _ => {
                    assert_eq!(lhs, rhs, "Failed to unify {lhs} and {rhs}");

                    false
                }
            },
        }
    }

    fn apply_constraints(&mut self, ir: &Ir<'ir>) -> bool {
        let mut constraints = std::mem::take(&mut self.constraints);
        let mut progress = false;

        constraints.retain(|constraint| match constraint {
            Constraint::Eq(lhs, rhs) => {
                progress |=
                    self.unify(self.get_ty_var(*lhs).clone(), self.get_ty_var(*rhs).clone());

                false
            }
            Constraint::BinAdd { expr, lhs, rhs } => {
                let (lhs, rhs) = if let Some(Ty::Ptr(_)) = self.get_ty_var(*rhs).ty() {
                    (rhs, lhs)
                } else {
                    (lhs, rhs)
                };

                if let Some(ty) = self.get_ty_var(*lhs).ty() {
                    match ty {
                        Ty::Ptr(_) => {
                            *self.get_ty_var_mut(*rhs) = TyVar::Typed(&Ty::Int(IntTy::Isize));
                            *self.get_ty_var_mut(*expr) = TyVar::Typed(&Ty::Int(IntTy::Isize));
                            progress |= true;
                        }
                        Ty::Int(_) | Ty::UInt(_) => {
                            *self.get_ty_var_mut(*rhs) = TyVar::Typed(ty);
                            *self.get_ty_var_mut(*expr) = TyVar::Typed(ty);
                            progress |= true;
                        }
                        _ => unreachable!("Bad type, expected integer or pointer, got {}", ty),
                    };

                    false
                } else {
                    self.eq(*expr, *lhs);

                    true
                }
            }
            Constraint::BinSub { expr, lhs, rhs } => {
                let (lhs, rhs) = if let Some(Ty::Ptr(_)) = self.get_ty_var(*rhs).ty() {
                    (rhs, lhs)
                } else {
                    (lhs, rhs)
                };

                match (self.get_ty_var(*lhs).ty(), self.get_ty_var(*rhs).ty()) {
                    (Some(Ty::Ptr(_)), Some(Ty::Ptr(_))) => {
                        *self.get_ty_var_mut(*expr) = TyVar::Typed(&Ty::Int(IntTy::Isize));
                        progress |= true;

                        false
                    }
                    (Some(ty @ Ty::Ptr(_)), Some(Ty::Int(_) | Ty::UInt(_))) => {
                        *self.get_ty_var_mut(*expr) = TyVar::Typed(ty);
                        progress |= true;

                        false
                    }
                    (None, None) => {
                        self.eq(*expr, *lhs);
                        self.eq(*lhs, *rhs);

                        progress |= true;

                        false
                    }
                    _ => true,
                }
            }
            Constraint::Field {
                expr,
                field_ty,
                field,
            } => match self.get_ty_var(*expr) {
                TyVar::Typed(ty) => match ty {
                    Ty::Struct(id) => match ir.get_node(*id) {
                        Node::Item(item) => match item {
                            Item::Struct(fields) => {
                                let ty = OrderedMap::get(fields, field).unwrap();

                                *self.get_ty_var_mut(*field_ty) = TyVar::Typed(ty);

                                progress |= true;

                                false
                            }
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                },
                TyVar::Infer(_) => true,
            },
        });
        self.constraints.append(&mut constraints);

        progress
    }

    pub fn solve(&mut self, ir: &Ir<'ir>) {
        loop {
            if !self.apply_constraints(ir) {
                break;
            }
        }

        assert!(self.constraints.is_empty());
    }

    pub fn resolve_ty(&self, ctx: &Context<'ir>, ty: &'ir Ty<'ir>) -> &'ir Ty<'ir> {
        // TODO: check if there already exist such a type instead of allocationg a new one
        match ty {
            Ty::Infer(id) => self.resolve_ty(ctx, self.get_ty_var(*id).ty().unwrap()),
            Ty::Ptr(ty) => ctx.allocator.alloc(Ty::Ptr(self.resolve_ty(ctx, *ty))),
            ty => ty,
        }
    }
}
