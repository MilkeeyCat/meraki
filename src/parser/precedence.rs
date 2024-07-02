use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Assign,
    Comparison,
    Equality,
    Sum,
    Product,
    Prefix,
    Call,
    Highest,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        use Token::*;

        match value {
            Plus | Minus => Self::Sum,
            Asterisk | Slash => Self::Product,
            LessThan | LessEqual | GreaterThan | GreaterEqual => Self::Comparison,
            Equal | NotEqual => Self::Equality,
            Assign => Self::Assign,
            LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}
