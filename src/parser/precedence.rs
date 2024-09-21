use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Assign,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseAnd,
    Comparison,
    Equality,
    Shift,
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
            Token::Shl | Token::Shr => Self::Shift,
            Token::Assign => Self::Assign,
            Token::LParen => Self::Call,
            Token::As => Self::Cast,
            Token::Period | Token::Arrow | Token::LBracket => Self::Access,
            Token::And => Self::LogicalAnd,
            Token::Or => Self::LogicalOr,
            Token::Ampersand => Self::BitwiseAnd,
            Token::Bar => Self::BitwiseOr,
            _ => Self::Lowest,
        }
    }
}

impl Precedence {
    pub fn lower(self) -> Self {
        match self {
            Self::Lowest => unreachable!(),
            Self::Assign => Self::Lowest,
            Self::LogicalOr => Self::Assign,
            Self::LogicalAnd => Self::LogicalOr,
            Self::BitwiseOr => Self::LogicalAnd,
            Self::BitwiseAnd => Self::BitwiseOr,
            Self::Comparison => Self::BitwiseAnd,
            Self::Equality => Self::Comparison,
            Self::Shift => Self::Equality,
            Self::Sum => Self::Shift,
            Self::Product => Self::Sum,
            Self::Prefix => Self::Product,
            Self::Cast => Self::Prefix,
            Self::Access => Self::Cast,
            Self::Call => Self::Access,
        }
    }
}
