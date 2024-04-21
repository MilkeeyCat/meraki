use crate::lexer::Token;

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
    Return(ExprReturn),
    Binary(ExprBinary),
    Lit(ExprLit),
    Unary(ExprUnary),
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct ExprReturn {}

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
