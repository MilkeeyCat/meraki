use crate::{
    archs::ArchError,
    parser::{ExprError, OpParseError},
    register::allocator::AllocatorError,
    symbol_table::SymbolTableError,
    types::TypeError,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error(transparent)]
    OpParse(#[from] OpParseError),
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    Allocator(#[from] AllocatorError),
    #[error(transparent)]
    SymbolTable(#[from] SymbolTableError),
    #[error(transparent)]
    Arch(#[from] ArchError),
    #[error(transparent)]
    Expr(#[from] ExprError),
}
