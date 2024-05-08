use super::{type_::Type, Expr};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(StmtVarDecl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtVarDecl {
    pub type_: Type,
    pub name: String,
    pub value: Option<Expr>,
}

impl StmtVarDecl {
    pub fn new(type_: Type, name: String, value: Option<Expr>) -> Self {
        Self { type_, name, value }
    }
}
