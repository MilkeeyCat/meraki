use crate::lexer::Token;

use super::Type;

#[derive(Debug, Clone)]
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
            token => unreachable!("cant convert token {:?} into binary operator", token),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(ExprBinary),
    Lit(ExprLit),
    Unary(ExprUnary),
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct ExprBinary {
    op: BinOp,
    left: Option<Box<Expr>>,
    right: Option<Box<Expr>>,
}

impl ExprBinary {
    pub fn new(op: BinOp, left: Option<Box<Expr>>, right: Option<Box<Expr>>) -> Self {
        Self { op, left, right }
    }

    pub fn op(&self) -> &BinOp {
        return &self.op;
    }

    pub fn left(&self) -> &Option<Box<Expr>> {
        return &self.left;
    }

    pub fn right(&self) -> &Option<Box<Expr>> {
        return &self.right;
    }
}

#[derive(Debug, Clone)]
pub enum ExprLit {
    Bool(bool),
    Float(f64),
    Int(i64),
    Str(String),
}

impl ExprLit {
    fn kind(&self) -> Type {
        match self {
            Self::Int(int) => match int {
                -128..=127 => Type::I8,
                0..=255 => Type::U8,
                -32768..=32767 => Type::I16,
                0..=65535 => Type::U16,
                -2147483648..=2147483647 => Type::I32,
                0..=4294967295 => Type::U32,
                -9223372036854775808..=9223372036854775807 => Type::I64,
                //TODO: what do i do here?
                //0..=18446744073709551615 => Type::U64,
            },
            Self::Str(_) => Type::Ptr(Box::new(Type::Char)),
            Self::Bool(_) => Type::Bool,
            //Who needs floats, amiright
            Self::Float(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Negative,
}

impl From<&Token> for UnOp {
    fn from(value: &Token) -> Self {
        match value {
            Token::Bang => Self::Not,
            Token::Minus => Self::Negative,
            token => unreachable!("cant convert token {:?} into unary operator", token),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprUnary {
    op: UnOp,
    expr: Box<Expr>,
}

impl ExprUnary {
    pub fn new(op: UnOp, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }
}
