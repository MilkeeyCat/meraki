use crate::lexer::TokenKind;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum OpParseError {
    #[error("Failed to parse binary operator from {0}")]
    Bin(TokenKind),
    #[error("Failed to parse unary operator from {0}")]
    Un(TokenKind),
    #[error("Failed to parse comparison operator from binary operator {0:?}")]
    Cmp(BinOp),
    #[error("Failed to parse bitwise operator from binary operator {0:?}")]
    Bitwise(BinOp),
}

#[derive(Debug, Clone, PartialEq, Copy)]
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
    Assign,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Shl,
    Shr,
}

impl TryFrom<&TokenKind> for BinOp {
    type Error = OpParseError;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Asterisk => Ok(Self::Mul),
            TokenKind::Plus => Ok(Self::Add),
            TokenKind::Minus => Ok(Self::Sub),
            TokenKind::Slash => Ok(Self::Div),
            TokenKind::Equal => Ok(Self::Equal),
            TokenKind::NotEqual => Ok(Self::NotEqual),
            TokenKind::LessThan => Ok(Self::LessThan),
            TokenKind::GreaterThan => Ok(Self::GreaterThan),
            TokenKind::LessEqual => Ok(Self::LessEqual),
            TokenKind::GreaterEqual => Ok(Self::GreaterEqual),
            TokenKind::Assign => Ok(Self::Assign),
            TokenKind::And => Ok(Self::LogicalAnd),
            TokenKind::Or => Ok(Self::LogicalOr),
            TokenKind::Ampersand => Ok(Self::BitwiseAnd),
            TokenKind::Bar => Ok(Self::BitwiseOr),
            TokenKind::Shl => Ok(Self::Shl),
            TokenKind::Shr => Ok(Self::Shr),
            token => Err(OpParseError::Bin(token.to_owned())),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    LogicalNot,
    Negative,
    Address,
    Deref,
    BitwiseNot,
}

impl TryFrom<&TokenKind> for UnOp {
    type Error = OpParseError;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Bang => Ok(Self::LogicalNot),
            TokenKind::Minus => Ok(Self::Negative),
            TokenKind::Ampersand => Ok(Self::Address),
            TokenKind::Asterisk => Ok(Self::Deref),
            TokenKind::Tilde => Ok(Self::BitwiseNot),
            token => Err(OpParseError::Un(token.to_owned())),
        }
    }
}

pub enum CmpOp {
    LessEqual,
    LessThan,
    GreaterEqual,
    GreaterThan,
    Equal,
    NotEqual,
}

impl TryFrom<&BinOp> for CmpOp {
    type Error = OpParseError;

    fn try_from(value: &BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::LessThan => Ok(Self::LessThan),
            BinOp::LessEqual => Ok(Self::LessEqual),
            BinOp::GreaterThan => Ok(Self::GreaterThan),
            BinOp::GreaterEqual => Ok(Self::GreaterEqual),
            BinOp::Equal => Ok(Self::Equal),
            BinOp::NotEqual => Ok(Self::NotEqual),
            _ => Err(OpParseError::Cmp(value.to_owned())),
        }
    }
}

pub enum BitwiseOp {
    And,
    Or,
}

impl TryFrom<&BinOp> for BitwiseOp {
    type Error = OpParseError;

    fn try_from(value: &BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::BitwiseAnd => Ok(Self::And),
            BinOp::BitwiseOr => Ok(Self::Or),
            _ => Err(OpParseError::Bitwise(value.to_owned())),
        }
    }
}
