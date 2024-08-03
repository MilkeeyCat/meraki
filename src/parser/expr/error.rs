use crate::{symbol_table::SymbolTableError, types::TypeError};

#[derive(Debug)]
pub enum ExprError {
    Type(TypeError),
    SymbolTable(SymbolTableError),
}

impl From<TypeError> for ExprError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<SymbolTableError> for ExprError {
    fn from(value: SymbolTableError) -> Self {
        Self::SymbolTable(value)
    }
}
