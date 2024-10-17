use crate::types::TypeError;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum SymbolTableError {
    #[error("Redeclaration of '{0}'")]
    Redeclaration(String),
    #[error("Symbol '{0}' not found")]
    NotFound(String),
    #[error(transparent)]
    Type(TypeError),
}
