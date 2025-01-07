use crate::lexer::TokenKind;

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

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Plus | TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk | TokenKind::Slash => Self::Product,
            TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual => Self::Comparison,
            TokenKind::Equal | TokenKind::NotEqual => Self::Equality,
            TokenKind::Shl | TokenKind::Shr => Self::Shift,
            TokenKind::Assign => Self::Assign,
            TokenKind::LParen | TokenKind::Bang => Self::Call,
            TokenKind::As => Self::Cast,
            TokenKind::Period | TokenKind::Arrow | TokenKind::LBracket => Self::Access,
            TokenKind::And => Self::LogicalAnd,
            TokenKind::Or => Self::LogicalOr,
            TokenKind::Ampersand => Self::BitwiseAnd,
            TokenKind::Bar => Self::BitwiseOr,
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
