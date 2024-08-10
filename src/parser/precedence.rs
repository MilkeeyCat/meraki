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
    Cast,
    Access,
    Call,
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
            Self::Comparison => Self::Assign,
            Self::Equality => Self::Comparison,
            Self::Sum => Self::Equality,
            Self::Product => Self::Sum,
            Self::Prefix => Self::Product,
            Self::Cast => Self::Prefix,
            Self::Access => Self::Cast,
            Self::Call => Self::Access,
        }
    }
}
