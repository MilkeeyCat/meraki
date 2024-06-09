use crate::parser::Type;

const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    GlobalVar(SymbolGlobalVar),
    LocalVar(SymbolLocalVar),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolGlobalVar {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolLocalVar {
    pub name: String,
    pub offset: usize,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    inner: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::with_capacity(MAX_SYMBOLS),
            inner: None,
        }
    }

    fn symtable(&self) -> &Self {
        if self.inner.is_none() {
            return self;
        } else {
            self.inner.as_ref().unwrap().symtable()
        }
    }

    fn symtable_mut(&mut self) -> &mut Self {
        if self.inner.is_none() {
            return self;
        } else {
            self.inner.as_mut().unwrap().symtable_mut()
        }
    }

    pub fn find(&self, name: &str) -> Option<&Symbol> {
        self.symtable().symbols.iter().find(|symbol| match symbol {
            Symbol::GlobalVar(global_var) => global_var.name == name,
            Symbol::LocalVar(local) => local.name == name,
        })
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symtable_mut()
            .symbols
            .iter_mut()
            .find(|symbol| match symbol {
                Symbol::GlobalVar(global_var) => global_var.name == name,
                Symbol::LocalVar(local) => local.name == name,
            })
    }

    pub fn exists(&self, name: &str) -> bool {
        for symbol in &self.symtable().symbols {
            match symbol {
                Symbol::GlobalVar(global_var) => {
                    if global_var.name == name {
                        return true;
                    }
                }
                Symbol::LocalVar(local) => {
                    if local.name == name {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn push(&mut self, symbol: Symbol) {
        assert!(self.symtable().symbols.len() < MAX_SYMBOLS);

        self.symtable_mut().symbols.push(symbol);
    }

    pub fn enter(&mut self, symtable: Box<SymbolTable>) {
        match self.inner.as_mut() {
            None => {
                self.inner = Some(symtable);
            }
            Some(inner) => {
                inner.enter(symtable);
            }
        }
    }

    pub fn leave(&mut self) {
        if self.inner.as_ref().is_some_and(|sb| sb.inner.is_none()) {
            self.inner = None;
        } else {
            self.inner.as_mut().unwrap().leave();
        }
    }

    pub fn inner(&mut self) -> Self {
        *std::mem::take(&mut self.inner).unwrap()
    }
}
