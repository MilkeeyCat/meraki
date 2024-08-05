use crate::{
    archs::Architecture,
    parser::{ExprError, ExprLit, Expression},
    register,
    scope::Scope,
};

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Global<'a> {
    pub label: &'a str,
    pub size: usize,
    pub offset: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct Local {
    pub size: usize,
    pub offset: usize,
}

#[derive(Clone, Debug)]
pub struct SourceParam {
    pub size: usize,
    pub n: usize,
}

#[derive(Clone, Debug)]
pub struct Register<'a> {
    pub register: &'a register::Register,
    pub size: usize,
    pub offset: Option<Offset>,
}

#[derive(Clone, Debug)]
pub enum MoveSource<'a> {
    Global(Global<'a>, bool),
    Local(Local, bool),
    Param(SourceParam, bool),
    Register(Register<'a>, bool),
    Lit(ExprLit),
}

impl<'a> MoveSource<'a> {
    pub fn signed(&self) -> bool {
        match self {
            Self::Global(_, signed) => *signed,
            Self::Local(_, signed) => *signed,
            Self::Param(_, signed) => *signed,
            Self::Register(_, signed) => *signed,
            Self::Lit(lit) => lit.signed(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Global(global, _) => global.size,
            Self::Local(local, _) => local.size,
            Self::Register(register, _) => register.size,
            Self::Lit(lit) => todo!(),
            Self::Param(_, _) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MoveDestination<'a> {
    Global(Global<'a>),
    Local(Local),
    Register(Register<'a>),
}

impl<'a> MoveDestination<'a> {
    pub fn to_source(self) -> MoveSource<'a> {
        match self {
            MoveDestination::Global(global) => MoveSource::Global(global, false),
            MoveDestination::Local(local) => MoveSource::Local(local, false),
            MoveDestination::Register(register) => MoveSource::Register(register, false),
        }
    }

    pub fn register(self) -> &'a register::Register {
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

    pub fn size(&self) -> usize {
        match self {
            MoveDestination::Global(global) => global.size,
            MoveDestination::Local(local) => local.size,
            MoveDestination::Register(register) => register.size,
        }
    }
}

impl<'a> From<&'a register::Register> for MoveDestination<'a> {
    fn from(value: &'a register::Register) -> Self {
        Self::Register(Register {
            register: value,
            offset: None,
            size: 8,
        })
    }
}
