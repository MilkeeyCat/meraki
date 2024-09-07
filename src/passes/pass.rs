use crate::{parser::Stmt, scope::Scope};

pub trait Pass {
    type Output;

    fn proccess(stmts: &mut Vec<Stmt>, scope: &mut Scope) -> Self::Output;
}
