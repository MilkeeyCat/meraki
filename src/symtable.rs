use crate::parser::Type;

const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone)]
pub enum Symbol {
    GlobalVar(SymbolGlobalVar),
}

#[derive(Debug, Clone)]
pub struct SymbolGlobalVar {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct SymbolTable(Vec<Symbol>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(Vec::with_capacity(MAX_SYMBOLS))
    }

    pub fn find(&self, name: &str) -> Option<&Symbol> {
        self.0.iter().find(|symbol| match symbol {
            Symbol::GlobalVar(global_var) => global_var.name == name,
        })
    }

    pub fn exists(&self, name: &str) -> bool {
        for symbol in &self.0 {
            match symbol {
                Symbol::GlobalVar(global_var) => {
                    return global_var.name == name;
                }
            }
        }

        false
    }

    pub fn push(&mut self, symbol: Symbol) {
        assert!(self.0.len() < MAX_SYMBOLS);

        self.0.push(symbol);
    }
}
