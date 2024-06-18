use crate::type_::Type;

const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    GlobalVar(SymbolGlobalVar),
    LocalVar(SymbolLocalVar),
}

impl Symbol {
    pub fn name(&self) -> &str {
        match self {
            Self::GlobalVar(global_var) => &global_var.name,
            Self::LocalVar(local) => &local.name,
        }
    }
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

#[derive(Debug, PartialEq)]
pub enum SymbolTableError {
    Redeclaration(String),
}

impl std::fmt::Display for SymbolTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Redeclaration(name) => write!(f, "Redeclaration of '{}'", name),
        }
    }
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
        self.inner
            .as_deref()
            .and_then(|inner| inner.find(name))
            .or_else(|| self.symbols.iter().find(|symbol| symbol.name() == name))
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.inner
            .as_deref_mut()
            .and_then(|inner| inner.find_mut(name))
            .or_else(|| self.symbols.iter_mut().find(|symbol| symbol.name() == name))
    }

    pub fn push(&mut self, symbol: Symbol) -> Result<(), SymbolTableError> {
        assert!(self.symtable().symbols.len() < MAX_SYMBOLS);

        if self
            .symtable()
            .symbols
            .iter()
            .map(|symbol| symbol.name())
            .collect::<Vec<&str>>()
            .contains(&symbol.name())
        {
            Err(SymbolTableError::Redeclaration(symbol.name().to_owned()))
        } else {
            self.symtable_mut().symbols.push(symbol);

            Ok(())
        }
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

#[cfg(test)]
mod test {
    use crate::{symtable::SymbolLocalVar, type_::Type};

    use super::{Symbol, SymbolGlobalVar, SymbolTable, SymbolTableError};

    #[test]
    fn scopes() -> Result<(), SymbolTableError> {
        let mut symtable = SymbolTable::new();
        let symbol = Symbol::GlobalVar(SymbolGlobalVar {
            name: "foo".to_owned(),
            type_: Type::U8,
        });

        symtable.push(symbol.clone())?;

        let symbol2 = Symbol::GlobalVar(SymbolGlobalVar {
            name: "foo".to_owned(),
            type_: Type::U8,
        });

        assert_eq!(
            symtable.push(symbol2),
            Err(SymbolTableError::Redeclaration("foo".to_owned()))
        );

        symtable.enter(Box::new(SymbolTable::new()));

        assert_eq!(
            symtable.find("foo"),
            Some(&Symbol::GlobalVar(SymbolGlobalVar {
                name: "foo".to_owned(),
                type_: Type::U8,
            }))
        );

        let symbol3 = Symbol::LocalVar(SymbolLocalVar {
            name: "foo".to_owned(),
            type_: Type::U16,
            offset: 0,
        });

        symtable.push(symbol3.clone())?;

        assert_eq!(symtable.find("foo"), Some(&symbol3));

        symtable.push(Symbol::LocalVar(SymbolLocalVar {
            name: "bar".to_owned(),
            type_: Type::I16,
            offset: 0,
        }))?;

        assert!(symtable.find("bar").is_some());

        symtable.leave();

        assert_eq!(symtable.find("bar"), None);
        assert_eq!(symtable.find("foo"), Some(&symbol));

        Ok(())
    }
}
