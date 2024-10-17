use crate::{
    parser::ExprError, register::allocator::AllocatorError, symbol_table::SymbolTableError,
    types::TypeError,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ArchError {
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    Allocator(#[from] AllocatorError),
    #[error(transparent)]
    SymbolTable(#[from] SymbolTableError),
    #[error(transparent)]
    Expr(#[from] ExprError),
}
