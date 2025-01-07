use super::{OpParseError, Ty};
use crate::lexer::TokenKind;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Type(#[from] TyError),
    #[error(transparent)]
    Operator(#[from] OpParseError),
    #[error("Expected token {0}, got {1}")]
    UnexpectedToken(TokenKind, TokenKind),
    #[error("Expected {0}")]
    Expected(TokenKind),
    #[error("Failed to parse type, found {0}")]
    ParseType(TokenKind),
    #[error("Failed to parse prefix token {0}")]
    Prefix(TokenKind),
    #[error("Failed to parse infix token {0}")]
    Infix(TokenKind),
}

#[derive(Error, Debug, PartialEq)]
pub enum TyError {
    #[error("Operation between {0} and {1} are not allowed")]
    Promotion(Ty, Ty),
    #[error("Ident {0} not found")]
    IdentNotFound(String),
    #[error("Can't assign {0} to {1}")]
    Assignment(Ty, Ty),
    #[error("Can't cast {0} into {1}")]
    Cast(Ty, Ty),
    #[error("Expected return value of type {1}, got {0} instead")]
    Return(Ty, Ty),
    #[error("Variable can't be of type void")]
    VoidVariable,
    #[error("Type '{0}' doens't exits")]
    Nonexistent(String),
    #[error("Type {0} is not pointer")]
    Deref(Ty),
    #[error("Mismatched types expected {0}, found {1}")]
    Mismatched(Ty, Ty),
}
