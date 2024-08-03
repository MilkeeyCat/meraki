#[derive(Debug, PartialEq)]
pub enum SymbolTableError {
    Redeclaration(String),
    NotFound(String),
}

impl std::fmt::Display for SymbolTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Redeclaration(name) => write!(f, "Redeclaration of '{name}'"),
            Self::NotFound(name) => write!(f, "Symbol '{name}' not found"),
        }
    }
}
