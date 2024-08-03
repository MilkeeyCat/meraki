use crate::types::TypeError;

#[derive(Debug, PartialEq)]
pub enum SymbolTableError {
    Redeclaration(String),
    NotFound(String),
    Type(TypeError),
}

impl std::fmt::Display for SymbolTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Redeclaration(name) => write!(f, "Redeclaration of '{name}'"),
            Self::NotFound(name) => write!(f, "Symbol '{name}' not found"),
            Self::Type(e) => write!(f, "{e}"),
        }
    }
}

impl From<TypeError> for SymbolTableError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}
