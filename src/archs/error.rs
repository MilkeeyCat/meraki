use crate::{
    parser::ExprError, register::allocator::AllocatorError, symbol_table::SymbolTableError,
    types::TypeError,
};

pub enum ArchError {
    Type(TypeError),
    Allocator(AllocatorError),
    SymbolTable(SymbolTableError),
}

impl From<TypeError> for ArchError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<AllocatorError> for ArchError {
    fn from(value: AllocatorError) -> Self {
        Self::Allocator(value)
    }
}

impl From<ExprError> for ArchError {
    fn from(value: ExprError) -> Self {
        match value {
            ExprError::Type(e) => Self::Type(e),
            ExprError::SymbolTable(e) => Self::SymbolTable(e),
        }
    }
}
