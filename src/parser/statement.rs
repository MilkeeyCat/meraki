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
    arguments: Vec<Type>,
    return_type: Type,
    stmts: Vec<Stmt>,
}

impl StmtFunction {
    pub fn new(ret: Type, args: Vec<Type>, stmts: Vec<Stmt>) -> Self {
        Self {
            return_type: ret,
            arguments: args,
            stmts,
        }
    }
}
