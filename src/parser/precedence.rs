use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Assign,
    LogicalOr,
    LogicalAnd,
    Comparison,
    Equality,
    Sum,
    Product,
    Cast,
    Prefix,
    Access,
    Call,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus | Token::Minus => Self::Sum,
            Token::Asterisk | Token::Slash => Self::Product,
            Token::LessThan | Token::LessEqual | Token::GreaterThan | Token::GreaterEqual => {
                Self::Comparison
            }
            Token::Equal | Token::NotEqual => Self::Equality,
            Token::Assign => Self::Assign,
            Token::LParen => Self::Call,
            Token::As => Self::Cast,
            Token::Period | Token::Arrow | Token::LBracket => Self::Access,
            Token::And => Self::LogicalAnd,
            Token::Or => Self::LogicalOr,
            _ => Self::Lowest,
        }
    }
}

impl Precedence {
    pub fn lower(self) -> Self {
        match self {
            Self::Lowest => panic!(),
            Self::Assign => Self::Lowest,
            Self::LogicalOr => Self::Assign,
            Self::LogicalAnd => Self::LogicalOr,
            Self::Comparison => Self::LogicalAnd,
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
