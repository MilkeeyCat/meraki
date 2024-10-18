use super::SymbolTableError;
use crate::{codegen::Offset, types::Type};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Global(SymbolGlobal),
    Local(SymbolLocal),
    Param(SymbolParam),
    Function(SymbolFunction),
}

impl Symbol {
    pub fn type_(&self) -> Type {
        match self {
            Self::Global(global) => global.type_.clone(),
            Self::Local(local) => local.type_.clone(),
            Self::Param(param) => param.type_.clone(),
            Self::Function(func) => {
                Type::Fn(func.parameters.clone(), Box::new(func.return_type.clone()))
            }
        }
    }

    pub fn function_unchecked(&self) -> &SymbolFunction {
        match self {
            Symbol::Function(symbol) => symbol,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolGlobal {
    pub label: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolLocal {
    pub offset: Offset,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolParam {
    pub preceding: Vec<Type>,
    pub type_: Type,
    pub offset: Offset,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolFunction {
    pub return_type: Type,
    pub parameters: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable(HashMap<String, Symbol>);

impl SymbolTable {
    const MAX_SYMBOLS: usize = 512;

    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn find(&self, name: &str) -> Option<&Symbol> {
        self.0.get(name)
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.0.get_mut(name)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Symbol> {
        self.0.values_mut().into_iter()
    }

    pub fn push(&mut self, name: String, symbol: Symbol) -> Result<(), SymbolTableError> {
        assert!(self.0.len() < Self::MAX_SYMBOLS);

        if self.0.contains_key(&name) {
            Err(SymbolTableError::Redeclaration(name))
        } else {
            self.0.insert(name, symbol);

            Ok(())
        }
    }
}
