use crate::lexer::Token;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum OpParseError {
    #[error("Failed to parse binary operator from {0}")]
    Bin(Token),
    #[error("Failed to parse unary operator from {0}")]
    Un(Token),
    #[error("Failed to parse comparison operator from binary operator {0:?}")]
    Cmp(BinOp),
    #[error("Failed to parse bitwise operator from binary operator {0:?}")]
    Bitwise(BinOp),
}

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
    Assign,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Shl,
    Shr,
}

impl TryFrom<&Token> for BinOp {
    type Error = OpParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Asterisk => Ok(Self::Mul),
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Slash => Ok(Self::Div),
            Token::Equal => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            Token::LessThan => Ok(Self::LessThan),
            Token::GreaterThan => Ok(Self::GreaterThan),
            Token::LessEqual => Ok(Self::LessEqual),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::Assign => Ok(Self::Assign),
            Token::And => Ok(Self::LogicalAnd),
            Token::Or => Ok(Self::LogicalOr),
            Token::Ampersand => Ok(Self::BitwiseAnd),
            Token::Bar => Ok(Self::BitwiseOr),
            Token::Shl => Ok(Self::Shl),
            Token::Shr => Ok(Self::Shr),
            token => Err(OpParseError::Bin(token.to_owned())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    LogicalNot,
    Negative,
    Address,
    Deref,
    BitwiseNot,
}

impl TryFrom<&Token> for UnOp {
    type Error = OpParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Bang => Ok(Self::LogicalNot),
            Token::Minus => Ok(Self::Negative),
            Token::Ampersand => Ok(Self::Address),
            Token::Asterisk => Ok(Self::Deref),
            Token::Tilde => Ok(Self::BitwiseNot),
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
