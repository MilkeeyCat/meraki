use crate::{
    lexer::Token,
    parser::{
        op::{BinOp, UnOp},
        Ty,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(ExprIdent),
    Struct(ExprStruct),
    Array(ExprArray),
    Field(ExprField),
    StructMethod(ExprStructMethod),
    ArrayAccess(ExprArrayAccess),
    FunctionCall(ExprFunctionCall),
    MacroCall(MacroCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIdent(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprArray(pub Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct ExprField {
    pub expr: Box<Expr>,
    pub field: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStructMethod {
    pub expr: Box<Expr>,
    pub method: String,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprFunctionCall {
    pub expr: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MacroCall {
    pub name: String,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprArrayAccess {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCast {
    pub expr: Box<Expr>,
    pub ty: Ty,
}
