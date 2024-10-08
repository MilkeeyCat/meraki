use crate::{
    archs::ArchError,
    parser::{Expr, ExprError, OpParseError},
    register::allocator::AllocatorError,
    symbol_table::SymbolTableError,
    types::TypeError,
};

#[derive(Debug)]
pub enum CodeGenError {
    OpParse(OpParseError),
    Type(TypeError),
    Allocator(AllocatorError),
    Assign(Expr),
    SymbolTable(SymbolTableError),
}

impl std::error::Error for CodeGenError {}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpParse(e) => e.fmt(f),
            Self::Type(e) => e.fmt(f),
            Self::Allocator(e) => e.fmt(f),
            Self::Assign(expr) => write!(f, "Can't assign to rvalue expression {expr:?}"),
            Self::SymbolTable(e) => e.fmt(f),
        }
    }
}

impl From<TypeError> for CodeGenError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<AllocatorError> for CodeGenError {
    fn from(value: AllocatorError) -> Self {
        Self::Allocator(value)
    }
}

impl From<OpParseError> for CodeGenError {
    fn from(value: OpParseError) -> Self {
        Self::OpParse(value)
    }
}

impl From<SymbolTableError> for CodeGenError {
    fn from(value: SymbolTableError) -> Self {
        Self::SymbolTable(value)
    }
}

impl From<ExprError> for CodeGenError {
    fn from(value: ExprError) -> Self {
        match value {
            ExprError::Type(e) => Self::Type(e),
            ExprError::SymbolTable(e) => Self::SymbolTable(e),
        }
    }
}

impl From<ArchError> for CodeGenError {
    fn from(value: ArchError) -> Self {
        match value {
            ArchError::Type(e) => Self::Type(e),
            ArchError::Allocator(e) => Self::Allocator(e),
            ArchError::SymbolTable(e) => Self::SymbolTable(e),
        }
    }
}
