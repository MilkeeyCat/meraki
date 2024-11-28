use crate::ir::{self, Ty};

#[derive(Clone, Copy, Debug)]
pub struct Id(pub usize);

#[derive(Debug)]
pub enum TyVar<'ir> {
    Typed(&'ir Ty<'ir>),
    Infer,
}

impl<'ir> TyVar<'ir> {
    pub fn ty(&self) -> Option<&'ir Ty<'ir>> {
        match self {
            Self::Typed(ty) => Some(ty),
            Self::Infer => None,
        }
    }
}

impl<'ir> From<&'ir Ty<'ir>> for TyVar<'ir> {
    fn from(value: &'ir Ty<'ir>) -> Self {
        match value {
            Ty::Infer => Self::Infer,
            _ => Self::Typed(value),
        }
    }
}

#[derive(Debug)]
enum Constraint {
    Eq(Id, Id),
    BinAdd(Id, Id),
    BinSub(Id, Id),
}

#[derive(Debug)]
pub struct TyProblem<'ir> {
    ty_vars: Vec<TyVar<'ir>>,
    constraints: Vec<Constraint>,
}

impl<'ir> TyProblem<'ir> {
    pub fn new_ty_var(&mut self, ty_var: TyVar<'ir>) -> Id {
        let i = self.ty_vars.len();

        self.ty_vars.push(ty_var);

        Id(i)
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

    pub fn bin_add(&mut self, lhs: Id, rhs: Id) {
        self.constraints.push(Constraint::BinAdd(lhs, rhs));
    }

    pub fn bin_sub(&mut self, lhs: Id, rhs: Id) {
        self.constraints.push(Constraint::BinSub(lhs, rhs));
    }

    fn apply_constraints(&mut self) -> bool {
        let mut constraints = std::mem::take(&mut self.constraints);
        let mut progress = false;

        constraints.retain(|constraint| match constraint {
            Constraint::Eq(lhs, rhs) => {
                let left_ty_var = self.get_ty_var(*lhs);
                let right_ty_var = self.get_ty_var(*rhs);

                match (left_ty_var.ty(), right_ty_var.ty()) {
                    (None, None) => true,
                    (Some(ty), None) => {
                        let ty_var = self.get_ty_var_mut(*rhs);

                        *ty_var = TyVar::Typed(ty);
                        progress |= true;

                        false
                    }
                    (None, Some(ty)) => {
                        let ty_var = self.get_ty_var_mut(*lhs);

                        *ty_var = TyVar::Typed(ty);
                        progress |= true;

                        false
                    }
                    (Some(lhs), Some(rhs)) => {
                        assert_eq!(lhs, rhs);

                        true
                    }
                }
            }
            Constraint::BinAdd(lhs, rhs) => {
                if let Some(ty) = self.get_ty_var(*lhs).ty() {
                    match ty {
                        Ty::Ptr(_) => {
                            *self.get_ty_var_mut(*rhs) = TyVar::Typed(&Ty::Int(ir::IntTy::Isize));
                            progress |= true;
                        }
                        Ty::Int(_) | Ty::UInt(_) => {
                            *self.get_ty_var_mut(*rhs) = TyVar::Typed(ty);
                            progress |= true;
                        }
                        _ => unreachable!("Bad type, expected integer or pointer, got {}", ty),
                    };

                    false
                } else {
                    true
                }
            }
            Constraint::BinSub(lhs, rhs) => {
                if let Some(ty) = self.get_ty_var(*lhs).ty() {
                    match ty {
                        Ty::Int(_) | Ty::UInt(_) => {
                            *self.get_ty_var_mut(*rhs) = TyVar::Typed(ty);
                            progress |= true;
                        }
                        // Can't assume a type because it can be both: pointer or integer
                        Ty::Ptr(_) => (),
                        _ => unreachable!("Bad type, expected integer or pointer, got {}", ty),
                    };

                    false
                } else {
                    true
                }
            }
        });
        self.constraints.append(&mut constraints);

        progress
    }

    pub fn solve(mut self) -> Vec<TyVar<'ir>> {
        loop {
            if !self.apply_constraints() {
                break;
            }
        }

        self.ty_vars
    }
}

impl<'ir> Default for TyProblem<'ir> {
    fn default() -> Self {
        Self {
            ty_vars: Vec::new(),
            constraints: Vec::new(),
        }
    }
}
