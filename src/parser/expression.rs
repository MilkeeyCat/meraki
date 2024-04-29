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
    Int(IntLitRepr),
    Str(String),
}

#[derive(Debug, Clone)]
pub struct IntLitRepr(Vec<u8>);

impl IntLitRepr {
    pub fn new() -> Self {
        Self(vec![0])
    }

    fn div2(&self, num: &str) -> Option<(String, bool)> {
        let mut remainder = false;
        let result = num
            .chars()
            .map(|ch| {
                let mut new = ch as u8 - b'0';
                if remainder {
                    new += 10;
                }

                if new & 1 != 0 {
                    remainder = true;
                } else {
                    remainder = false;
                }

                new >>= 1;

                (new + b'0') as char
            })
            .collect::<String>();

        if result.len() > 0 {
            Some((result, remainder))
        } else {
            None
        }
    }

    fn set_bit(&mut self, n: usize) {
        if let None = self.0.get(n / 8) {
            self.0.push(0);
            self.set_bit(n);
        } else {
            self.0[n / 8] |= 1 << (n % 8);
        }
    }

    pub fn from_str(s: String) -> Self {
        let mut int_repr = IntLitRepr::new();
        let mut result = s;

        for i in 0.. {
            match int_repr.div2(&result) {
                Some((string, remainder)) => {
                    result = string;

                    if result.as_bytes()[0] == b'0' {
                        result.remove(0);
                    }

                    if remainder {
                        int_repr.set_bit(i);
                    }
                }
                None => break,
            }
        }

        int_repr
    }

    pub fn bytes(&self) -> &[u8] {
        &self.0
    }

    pub fn i64(&mut self) -> i64 {
        if self.0.len() < 8 {
            self.0.resize(8, 0);
        }

        i64::from_le_bytes(self.0[0..8].try_into().unwrap())
    }
}

impl From<String> for IntLitRepr {
    fn from(value: String) -> Self {
        Self::from_str(value)
    }
}

impl ExprLit {
    fn kind(&self) -> Type {
        match self {
            Self::Int(int) => match int.bytes().len() {
                1 => Type::U8,
                2 => Type::U16,
                3..=4 => Type::U32,
                5..=8 => Type::U64,
                bytes => panic!("Int out of bounds, max: 64 bit, got: {:?}", bytes * 8),
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
