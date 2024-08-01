use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Assign,
    Access,
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
            Period => Self::Access,
            _ => Self::Lowest,
        }
    }
}

impl Precedence {
    pub fn lower(self) -> Self {
        match self {
            Self::Lowest => panic!(),
            Self::Assign => Self::Lowest,
            Self::Access => Self::Assign,
            Self::Comparison => Self::Access,
            Self::Equality => Self::Comparison,
            Self::Sum => Self::Equality,
            Self::Product => Self::Sum,
            Self::Prefix => Self::Product,
            Self::Call => Self::Prefix,
            Self::Highest => Self::Call,
        }
    }
}
