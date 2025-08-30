use crate::{
    Context,
    ast::IntTy,
    ir::{Ty, ty::InferTy},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

#[derive(Debug)]
enum Constraint<'ir> {
    Eq(&'ir Ty<'ir>, &'ir Ty<'ir>),
    BinAdd {
        expr: &'ir Ty<'ir>,
        lhs: &'ir Ty<'ir>,
        rhs: &'ir Ty<'ir>,
    },
    BinSub {
        expr: &'ir Ty<'ir>,
        lhs: &'ir Ty<'ir>,
        rhs: &'ir Ty<'ir>,
    },
    Field {
        expr: &'ir Ty<'ir>,
        field_ty: &'ir Ty<'ir>,
        field_name: String,
    },
    FunctionArgument {
        expr: &'ir Ty<'ir>,
        argument_ty: &'ir Ty<'ir>,
        argument_idx: usize,
    },
    Return {
        function: &'ir Ty<'ir>,
        expr: &'ir Ty<'ir>,
    },
}

#[derive(Debug)]
pub struct TyProblem<'ir> {
    ty_vars: Vec<Option<&'ir Ty<'ir>>>,
    constraints: Vec<Constraint<'ir>>,
}

impl<'ir> TyProblem<'ir> {
    pub fn new() -> Self {
        Self {
            ty_vars: Vec::new(),
            constraints: Vec::new(),
        }
    }

    fn new_ty_var(&mut self, ty: Option<&'ir Ty<'ir>>) -> Id {
        let id = self.ty_vars.len();

        self.ty_vars.push(ty);

        Id(id)
    }

    pub fn new_infer_ty_var(&mut self) -> Id {
        self.new_ty_var(None)
    }

    pub fn new_typed_ty_var(&mut self, ty: &'ir Ty<'ir>) -> Id {
        self.new_ty_var(Some(ty))
    }

    pub fn eq(&mut self, lhs: &'ir Ty<'ir>, rhs: &'ir Ty<'ir>) {
        self.constraints.push(Constraint::Eq(lhs, rhs));
    }

    pub fn bin_add(&mut self, expr: &'ir Ty<'ir>, lhs: &'ir Ty<'ir>, rhs: &'ir Ty<'ir>) {
        self.constraints.push(Constraint::BinAdd { expr, lhs, rhs });
    }

    pub fn bin_sub(&mut self, expr: &'ir Ty<'ir>, lhs: &'ir Ty<'ir>, rhs: &'ir Ty<'ir>) {
        self.constraints.push(Constraint::BinSub { expr, lhs, rhs });
    }

    pub fn field(&mut self, expr_ty: &'ir Ty<'ir>, field_ty: &'ir Ty<'ir>, field_name: String) {
        self.constraints.push(Constraint::Field {
            expr: expr_ty,
            field_ty,
            field_name,
        });
    }

    pub fn fn_argument(
        &mut self,
        expr_ty: &'ir Ty<'ir>,
        argument_ty: &'ir Ty<'ir>,
        argument_idx: usize,
    ) {
        self.constraints.push(Constraint::FunctionArgument {
            expr: expr_ty,
            argument_ty,
            argument_idx,
        });
    }

    pub fn ret(&mut self, function: &'ir Ty<'ir>, expr: &'ir Ty<'ir>) {
        self.constraints.push(Constraint::Return { function, expr });
    }

    fn resolve_ty(&self, ty: &'ir Ty<'ir>) -> Option<&'ir Ty<'ir>> {
        match ty {
            Ty::Infer(Some(infer_ty)) => {
                let id: Id = infer_ty.clone().into();

                self.ty_vars[id.0].map(|ty| self.resolve_ty(ty)).flatten()
            }
            Ty::Infer(None) => unreachable!(),
            ty => Some(ty),
        }
    }

    fn bind_ty_var(&mut self, id: Id, ty: &'ir Ty<'ir>) {
        assert!(
            &self.ty_vars[id.0].is_none(),
            "Type variable was already bound"
        );

        self.ty_vars[id.0] = Some(ty);
    }

    fn unify(&mut self, lhs: &'ir Ty<'ir>, rhs: &'ir Ty<'ir>) {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => (),

            (Ty::Infer(Some(infer_ty)), ty) | (ty, Ty::Infer(Some(infer_ty))) => match infer_ty {
                InferTy::TyVar(id) | InferTy::IntVar(id) if self.ty_vars[id.0].is_some() => {
                    self.unify(self.ty_vars[id.0].unwrap(), ty);
                }
                InferTy::TyVar(id) => {
                    self.bind_ty_var(*id, ty);
                }
                InferTy::IntVar(id) => {
                    assert!(matches!(ty, Ty::UInt(_) | Ty::Int(_)), "Invalid int type");

                    self.bind_ty_var(*id, ty);
                }
            },

            (Ty::Ptr(lhs), Ty::Ptr(rhs)) => self.unify(lhs, rhs),

            (lhs, rhs) => panic!("Failed to unify {lhs:?} and {rhs:?}"),
        }
    }

    fn apply_constraints(&mut self, ctx: &Context<'ir>) -> bool {
        let mut constraints = std::mem::take(&mut self.constraints);
        let mut progress = false;

        constraints.retain(|constraint| match constraint {
            Constraint::Eq(lhs, rhs) => {
                self.unify(lhs, rhs);
                progress |= true;

                false
            }
            Constraint::BinAdd { expr, lhs, rhs } => {
                let (lhs, rhs) = if let Some(Ty::Ptr(_)) = self.resolve_ty(rhs) {
                    (rhs, lhs)
                } else {
                    (lhs, rhs)
                };

                if let Some(ty) = self.resolve_ty(lhs) {
                    match ty {
                        Ty::Ptr(_) => {
                            self.unify(rhs, &Ty::Int(IntTy::Isize));
                            self.unify(expr, &Ty::Int(IntTy::Isize));
                            progress |= true;
                        }
                        Ty::Int(_) | Ty::UInt(_) => {
                            self.unify(rhs, ty);
                            self.unify(expr, ty);
                            progress |= true;
                        }
                        _ => unreachable!("Bad type, expected integer or pointer, got {ty:?}"),
                    };

                    false
                } else {
                    self.unify(expr, lhs);

                    true
                }
            }
            Constraint::BinSub { expr, lhs, rhs } => {
                let (lhs, rhs) = if let Some(Ty::Ptr(_)) = self.resolve_ty(rhs) {
                    (rhs, lhs)
                } else {
                    (lhs, rhs)
                };

                match (self.resolve_ty(lhs), self.resolve_ty(rhs)) {
                    (Some(Ty::Ptr(_)), Some(Ty::Ptr(_))) => {
                        self.unify(expr, &Ty::Int(IntTy::Isize));
                        progress |= true;

                        false
                    }
                    (Some(ty @ Ty::Ptr(_)), Some(Ty::Int(_) | Ty::UInt(_))) => {
                        self.unify(expr, ty);
                        progress |= true;

                        false
                    }
                    (None, None) => {
                        self.unify(expr, lhs);
                        self.unify(lhs, rhs);

                        progress |= true;

                        false
                    }
                    _ => true,
                }
            }
            Constraint::Field {
                expr,
                field_ty,
                field_name,
            } => match self.resolve_ty(expr) {
                Some(ty) => match ty {
                    Ty::Adt(idx) => {
                        let variant = &ctx.get_adt(*idx).variants[0];
                        let (_, field) = variant.get_field_by_name(&field_name).unwrap();

                        self.unify(field_ty, field.ty);
                        progress |= true;

                        false
                    }
                    _ => unreachable!(),
                },
                None => true,
            },
            Constraint::FunctionArgument {
                expr,
                argument_ty,
                argument_idx,
            } => match self.resolve_ty(expr) {
                Some(ty) => match ty {
                    Ty::Fn(params, _) => {
                        self.unify(argument_ty, params[*argument_idx]);
                        progress |= true;

                        false
                    }
                    _ => unreachable!(),
                },
                None => true,
            },
            Constraint::Return { function, expr } => match self.resolve_ty(function) {
                Some(ty) => match ty {
                    Ty::Fn(_, ty) => {
                        self.unify(expr, ty);
                        progress |= true;

                        false
                    }
                    _ => unreachable!(),
                },
                None => true,
            },
        });
        self.constraints.append(&mut constraints);

        progress
    }

    pub fn solve(mut self, ctx: &Context<'ir>) -> HashMap<Id, &'ir Ty<'ir>> {
        loop {
            if !self.apply_constraints(ctx) {
                break;
            }
        }

        assert!(
            self.constraints.is_empty(),
            "Failed to infer variables' types"
        );
        self.ty_vars
            .into_iter()
            .enumerate()
            .map(|(id, ty_var)| match ty_var {
                Some(ty) => (Id(id), ty),
                None => unreachable!(),
            })
            .collect()
    }
}
