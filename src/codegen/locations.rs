use crate::{parser::ExprLit, register_allocator::Register, symbol_table::Symbol, type_::Type};

#[derive(Debug)]
pub enum MoveSource<'a> {
    Global(&'a str, Type),
    Local(usize, Type),
    Param(usize, Type),
    Register(&'a Register, Type),
    Lit(ExprLit),
}

impl<'a> From<&'a Symbol> for MoveSource<'a> {
    fn from(value: &'a Symbol) -> Self {
        match value {
            Symbol::Local(symbol) => Self::Local(symbol.offset, symbol.type_.clone()),
            Symbol::Global(symbol) => Self::Global(&symbol.name, symbol.type_.clone()),
            Symbol::Param(symbol) => Self::Param(symbol.n, symbol.type_.clone()),
            Symbol::Function(_) => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MoveDestination<'a> {
    Global(&'a str),
    Local(usize),
    Register(&'a Register),
}

impl<'a> MoveDestination<'a> {
    pub fn to_source(self, type_: Type) -> MoveSource<'a> {
        match self {
            MoveDestination::Global(label) => MoveSource::Global(label, type_),
            MoveDestination::Local(offset) => MoveSource::Local(offset, type_),
            MoveDestination::Register(register) => MoveSource::Register(register, type_),
        }
    }

    pub fn register(self) -> Option<&'a Register> {
        match self {
            Self::Register(register) => Some(register),
            _ => None,
        }
    }
}

impl<'a> From<&'a Register> for MoveDestination<'a> {
    fn from(value: &'a Register) -> Self {
        Self::Register(value)
    }
}

impl<'a> From<&'a Symbol> for MoveDestination<'a> {
    fn from(value: &'a Symbol) -> Self {
        match value {
            Symbol::Local(symbol) => Self::Local(symbol.offset),
            Symbol::Global(symbol) => Self::Global(&symbol.name),
            Symbol::Param(symbol) => {
                todo!();
            }
            Symbol::Function(_) => unreachable!(),
        }
    }
}
