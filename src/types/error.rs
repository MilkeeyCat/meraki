use super::Type;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("Operation between {0} and {1} are not allowed")]
    Promotion(Type, Type),
    #[error("Ident {0} not found")]
    IdentNotFound(String),
    #[error("Can't assign {0} to {1}")]
    Assignment(Type, Type),
    #[error("Can't cast {0} into {1}")]
    Cast(Type, Type),
    #[error("Expected return value of type {1}, got {0} instead")]
    Return(Type, Type),
    #[error("Variable can't be of type void")]
    VoidVariable,
    #[error("Type '{0}' doens't exits")]
    Nonexistent(String),
    #[error("Type {0} is not pointer")]
    Deref(Type),
    #[error("Mismatched types expected {0}, found {1}")]
    Mismatched(Type, Type),
}
