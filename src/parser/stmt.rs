use super::Expr;
use crate::{scope::ScopeImpl, type_::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(StmtVarDecl),
    Expr(Expr),
    Function(StmtFunction),
    Return(StmtReturn),
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

#[derive(Debug, Clone, PartialEq)]
pub struct StmtReturn {
    pub expr: Option<Expr>,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtFunction {
    pub return_type: Type,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Vec<Stmt>,
    pub scope: Box<ScopeImpl>,
}
