use super::Expr;
use crate::{scope::ScopeImpl, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(StmtVarDecl),
    Expr(Expr),
    Function(StmtFunction),
    Return(StmtReturn),
    If(StmtIf),
    While(StmtWhile),
    For(StmtFor),
    Continue,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub scope: ScopeImpl,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf {
    pub condition: Expr,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtWhile {
    pub condition: Expr,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtFor {
    pub initializer: Option<Box<Stmt>>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtFunction {
    pub return_type: Type,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub block: Block,
}
