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
pub struct IntLitRepr(Vec<u8>);

impl IntLitRepr {
    pub fn new(str_num: String) -> Self {
        Self::parse_string(str_num)
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

    pub fn parse_string(str_num: String) -> Self {
        let mut int_repr = IntLitRepr(vec![]);
        let mut result = str_num;

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
    op: UnOp,
    expr: Box<Expr>,
}

impl ExprUnary {
    pub fn new(op: UnOp, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }
}
