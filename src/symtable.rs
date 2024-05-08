const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone)]
pub enum Symbol {
    //
}

#[derive(Debug, Clone)]
pub struct SymbolTable(Vec<Symbol>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(Vec::with_capacity(MAX_SYMBOLS))
    }
}
