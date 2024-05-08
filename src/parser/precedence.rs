use crate::lexer::Token;

#[derive(Debug, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Prefix,
    Product,
    Sum,
    Assign,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        use Token::*;

        match value {
            Plus | Minus => Self::Sum,
            Asterisk | Slash => Self::Product,
            _ => Self::Lowest,
        }
    }
}
