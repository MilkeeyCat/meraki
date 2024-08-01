use crate::{parser::ExprLit, register_allocator::Register, symbol_table::Symbol};

#[derive(Debug)]
pub struct Offset(pub isize);

impl std::fmt::Display for Offset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 > 0 {
            write!(f, " + {}", self.0)
        } else if self.0 < 0 {
            write!(f, " - {}", self.0.abs())
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug)]
pub struct SourceGlobal<'a> {
    pub label: &'a str,
    pub size: usize,
    pub signed: bool,
    pub offset: Option<usize>,
}

#[derive(Debug)]
pub struct SourceLocal {
    pub size: usize,
    pub signed: bool,
    pub offset: usize,
}

#[derive(Debug)]
pub struct SourceParam {
    pub size: usize,
    pub signed: bool,
    pub n: usize,
}

#[derive(Debug)]
pub struct SourceRegister<'a> {
    pub register: &'a Register,
    pub size: usize,
    pub signed: bool,
    pub offset: Option<Offset>,
}

#[derive(Debug)]
pub enum MoveSource<'a> {
    Global(SourceGlobal<'a>),
    Local(SourceLocal),
    Param(SourceParam),
    Register(SourceRegister<'a>),
    Lit(ExprLit),
}

#[derive(Clone, Debug)]
pub struct DestinationGlobal<'a> {
    pub label: &'a str,
    pub offset: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct DestinationLocal {
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct DestinationRegister<'a> {
    pub register: &'a Register,
    pub offset: Option<usize>,
}

#[derive(Clone, Debug)]
pub enum MoveDestination<'a> {
    Global(DestinationGlobal<'a>),
    Local(DestinationLocal),
    Register(DestinationRegister<'a>),
}

impl<'a> MoveDestination<'a> {
    pub fn to_source(self, size: usize) -> MoveSource<'a> {
        match self {
            MoveDestination::Global(global) => MoveSource::Global(SourceGlobal {
                label: global.label,
                offset: global.offset,
                signed: false,
                size,
            }),
            MoveDestination::Local(local) => MoveSource::Local(SourceLocal {
                offset: local.offset,
                signed: false,
                size,
            }),
            MoveDestination::Register(register) => MoveSource::Register(SourceRegister {
                register: register.register,
                offset: register
                    .offset
                    .map(|offset| Offset(offset.try_into().unwrap())),
                signed: false,
                size,
            }),
        }
    }

    pub fn register(self) -> &'a Register {
        match self {
            Self::Register(register) => register.register,
            _ => unreachable!(),
        }
    }

    pub fn local_offset(&self) -> usize {
        match self {
            Self::Local(local) => local.offset,
            _ => unreachable!(),
        }
    }
}

impl<'a> From<&'a Register> for MoveDestination<'a> {
    fn from(value: &'a Register) -> Self {
        Self::Register(DestinationRegister {
            register: value,
            offset: None,
        })
    }
}

impl<'a> From<&'a Symbol> for MoveDestination<'a> {
    fn from(value: &'a Symbol) -> Self {
        match value {
            Symbol::Local(symbol) => Self::Local(DestinationLocal {
                offset: symbol.offset,
            }),
            Symbol::Global(symbol) => Self::Global(DestinationGlobal {
                label: &symbol.name,
                offset: None,
            }),
            Symbol::Param(symbol) => todo!(),
            Symbol::Function(_) => unreachable!(),
        }
    }
}
