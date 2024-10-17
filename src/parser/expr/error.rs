use crate::{symbol_table::SymbolTableError, types::TypeError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExprError {
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    SymbolTable(#[from] SymbolTableError),
}
