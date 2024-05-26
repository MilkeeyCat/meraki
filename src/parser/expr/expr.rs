use super::IntLitRepr;
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Assign,
}

impl From<&Token> for BinOp {
    fn from(value: &Token) -> Self {
        match value {
            Token::Asterisk => Self::Mul,
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Slash => Self::Div,
            Token::Equal => Self::Equal,
            Token::NotEqual => Self::NotEqual,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,
            Token::LessEqual => Self::LessEqual,
            Token::GreaterEqual => Self::GreaterEqual,
            Token::Assign => Self::Assign,
            token => panic!("Couldn't convert {:?} into binary operator", token),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Lit(ExprLit),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl ExprBinary {
    pub fn new(op: BinOp, left: Box<Expr>, right: Box<Expr>) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(IntLitRepr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Negative,
}

impl From<&Token> for UnOp {
    fn from(value: &Token) -> Self {
        match value {
            Token::Bang => Self::Not,
            Token::Minus => Self::Negative,
            token => panic!("Couldn't convert {:?} into unary operator", token),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

impl ExprUnary {
    pub fn new(op: UnOp, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }
}
