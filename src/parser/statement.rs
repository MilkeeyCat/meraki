use super::{Expr, Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    Local(StmtLocal),
    Expr(Expr),
    Return(StmtReturn),
    Function(StmtFunction),
}

#[derive(Debug, Clone)]
pub struct StmtLocal {
    name: String,
    value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct StmtReturn(pub Option<Box<Expr>>);

impl StmtReturn {
    pub fn new(value: Option<Box<Expr>>) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone)]
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
