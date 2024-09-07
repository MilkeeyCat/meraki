use operands::{Base, Memory};

use super::SymbolTableError;
use crate::{
    archs::Arch,
    codegen::{operands, Destination, EffectiveAddress, Offset, Source},
    register::Register,
    scope::Scope,
    types::Type,
};

const MAX_SYMBOLS: usize = 512;
//FIXME: arch dependent stuff shouldn't be in symbol table file xd
const RBP: Register = Register::new("there's no one byte one, hmmmm", "bp", "ebp", "rbp");

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Global(SymbolGlobal),
    Local(SymbolLocal),
    Param(SymbolParam),
    Function(SymbolFunction),
}

impl Symbol {
    pub fn name(&self) -> &str {
        match self {
            Self::Global(global) => &global.name,
            Self::Local(local) => &local.name,
            Self::Param(param) => &param.name,
            Self::Function(function) => &function.name,
        }
    }

    pub fn type_(&self) -> Type {
        match self {
            Self::Global(global) => global.type_.clone(),
            Self::Local(local) => local.type_.clone(),
            Self::Param(param) => param.type_.clone(),
            Self::Function(_) => {
                panic!("Type of function, you wanted to get return type of the function or wat?")
            }
        }
    }

    pub fn function_unchecked(&self) -> &SymbolFunction {
        match self {
            Symbol::Function(symbol) => symbol,
            _ => unreachable!(),
        }
    }

    pub fn source(&self, arch: &Arch, scope: &Scope) -> Result<Source, SymbolTableError> {
        Ok(self.dest(arch, scope)?.into())
    }

    pub fn dest(&self, arch: &Arch, scope: &Scope) -> Result<Destination, SymbolTableError> {
        Ok(match self {
            Self::Local(symbol) => Destination::Memory(Memory {
                effective_address: EffectiveAddress {
                    base: Base::Register(RBP),
                    index: None,
                    scale: None,
                    displacement: Some(symbol.offset.clone()),
                },
                size: symbol.type_.size(arch, scope)?,
            }),
            Self::Global(symbol) => Destination::Memory(Memory {
                effective_address: EffectiveAddress {
                    base: Base::Label(symbol.name.clone()),
                    index: None,
                    scale: None,
                    displacement: None,
                },
                size: symbol.type_.size(arch, scope)?,
            }),
            Self::Param(symbol) => Destination::Memory(Memory {
                effective_address: EffectiveAddress {
                    base: Base::Register(RBP),
                    index: None,
                    scale: None,
                    displacement: Some(symbol.offset.clone()),
                },
                size: symbol.type_.size(arch, scope)?,
            }),
            Self::Function(_) => unreachable!(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolGlobal {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolLocal {
    pub name: String,
    pub offset: Offset,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolParam {
    pub name: String,
    pub preceding: Vec<Type>,
    pub type_: Type,
    pub offset: Offset,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolFunction {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable(pub Vec<Symbol>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(Vec::with_capacity(MAX_SYMBOLS))
    }

    pub fn find(&self, name: &str) -> Option<&Symbol> {
        self.0.iter().find(|symbol| symbol.name() == name)
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.0.iter_mut().find(|symbol| symbol.name() == name)
    }

    pub fn push(&mut self, symbol: Symbol) -> Result<(), SymbolTableError> {
        assert!(self.0.len() < MAX_SYMBOLS);

        if self
            .0
            .iter()
            .map(|symbol| symbol.name())
            .collect::<Vec<&str>>()
            .contains(&symbol.name())
        {
            Err(SymbolTableError::Redeclaration(symbol.name().to_owned()))
        } else {
            self.0.push(symbol);

            Ok(())
        }
    }
}
