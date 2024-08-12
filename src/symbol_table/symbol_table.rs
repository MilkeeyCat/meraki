use super::SymbolTableError;
use crate::{
    archs::Arch,
    codegen::locations::{Global, Local, MoveDestination, MoveSource, Offset},
    scope::Scope,
    types::Type,
};

const MAX_SYMBOLS: usize = 512;

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

    pub fn to_source<'a>(
        &'a self,
        arch: &'a Arch,
        scope: &Scope,
    ) -> Result<MoveSource, SymbolTableError> {
        Ok(match self {
            Self::Local(symbol) => MoveSource::Local(
                Local {
                    size: symbol.type_.size(arch, scope)?,
                    offset: symbol.offset.clone(),
                },
                symbol.type_.signed(),
            ),
            Self::Global(symbol) => MoveSource::Global(
                Global {
                    label: &symbol.name,
                    size: symbol.type_.size(arch, scope)?,
                    offset: None,
                },
                symbol.type_.signed(),
            ),
            Self::Param(symbol) => arch
                .param_dest(scope, &symbol.type_, &symbol.preceding)
                .to_source(symbol.type_.signed()),
            Self::Function(_) => unreachable!(),
        })
    }

    pub fn to_dest(&self, arch: &Arch, scope: &Scope) -> Result<MoveDestination, SymbolTableError> {
        Ok(match self {
            Symbol::Local(symbol) => MoveDestination::Local(Local {
                offset: symbol.offset.clone(),
                size: symbol.type_.size(&arch, scope)?,
            }),
            Symbol::Global(symbol) => MoveDestination::Global(Global {
                label: &symbol.name,
                size: symbol.type_.size(&arch, scope)?,
                offset: None,
            }),
            Symbol::Param(_symbol) => todo!(),
            Symbol::Function(_) => unreachable!(),
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolFunction {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable(Vec<Symbol>);

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
