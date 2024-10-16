use crate::{parser::Stmt, scope::Scope};

pub trait Pass {
    type Output;
    type State;

    fn new(state: Self::State) -> Self;
    fn run_pass(self, stmts: &mut Vec<Stmt>, scope: &mut Scope) -> Self::Output;
}
