use super::{type_::Type, Expr};
use crate::symtable::SymbolTable;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(StmtVarDecl),
    Expr(Expr),
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
pub struct StmtFunction {
    pub return_type: Type,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Vec<Stmt>,
    pub symtable: SymbolTable,
}
