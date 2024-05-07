use super::{Expr, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(StmtVarDecl),
    Expr(Expr),
    Return(StmtReturn),
    Function(StmtFunction),
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
pub struct StmtReturn(pub Option<Box<Expr>>);

impl StmtReturn {
    pub fn new(value: Option<Box<Expr>>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtFunction {
    pub name: String,
    pub arguments: Vec<Type>,
    pub return_type: Type,
    pub stmts: Vec<Stmt>,
}

impl StmtFunction {
    pub fn new(name: String, ret: Type, args: Vec<Type>, stmts: Vec<Stmt>) -> Self {
        Self {
            name,
            return_type: ret,
            arguments: args,
            stmts,
        }
    }
}
