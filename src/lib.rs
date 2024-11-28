pub mod codegen;
pub mod compile;
pub mod ir;
pub mod lexer;
pub mod lowering;
pub mod macros;
pub mod parser;
pub mod passes;
pub mod ty_problem;

use bumpalo::Bump;
use ir::{Ir, Ty};
use ty_problem::TyProblem;

#[derive(Debug)]
pub struct Context<'ir> {
    pub allocator: &'ir Bump,
    pub ir: Ir<'ir>,
    pub ty_problem: TyProblem<'ir>,
}

impl<'ir> Context<'ir> {
    pub fn new(allocator: &'ir Bump) -> Self {
        Self {
            allocator,
            ir: Ir::new(allocator),
            ty_problem: TyProblem::new(),
        }
    }

    pub fn resolve_ty(&self, ty: &'ir Ty<'ir>) -> &'ir Ty<'ir> {
        self.ty_problem.resolve_ty(self, ty)
    }
}
