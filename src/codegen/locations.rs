use crate::{parser::ExprLit, register};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Offset(pub isize);

impl std::ops::Add<isize> for &Offset {
    type Output = Offset;

    fn add(self, rhs: isize) -> Self::Output {
        Offset(self.0 + rhs)
    }
}

impl std::ops::Add<&Offset> for &Offset {
    type Output = Offset;

    fn add(self, rhs: &Offset) -> Self::Output {
        Offset(self.0 + rhs.0)
    }
}

impl std::ops::Sub<isize> for &Offset {
    type Output = Offset;

    fn sub(self, rhs: isize) -> Self::Output {
        Offset(self.0 - rhs)
    }
}

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
    pub offset: Option<Offset>,
}

#[derive(Clone, Debug)]
pub struct Local {
    pub size: usize,
    pub offset: Offset,
}

#[derive(Clone, Debug)]
pub struct Register {
    pub register: register::Register,
    pub size: usize,
    pub offset: Option<Offset>,
}

#[derive(Clone, Debug)]
pub enum MoveSource<'a> {
    Global(Global<'a>, bool),
    Local(Local, bool),
    Register(Register, bool),
    Lit(ExprLit),
}

impl<'a> MoveSource<'a> {
    pub fn signed(&self) -> bool {
        match self {
            Self::Global(_, signed) => *signed,
            Self::Local(_, signed) => *signed,
            Self::Register(_, signed) => *signed,
            Self::Lit(lit) => lit.signed(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Global(global, _) => global.size,
            Self::Local(local, _) => local.size,
            Self::Register(register, _) => register.size,
            Self::Lit(_lit) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MoveDestination<'a> {
    Global(Global<'a>),
    Local(Local),
    Register(Register),
}

impl<'a> MoveDestination<'a> {
    pub fn to_source(self, signed: bool) -> MoveSource<'a> {
        match self {
            MoveDestination::Global(global) => MoveSource::Global(global, signed),
            MoveDestination::Local(local) => MoveSource::Local(local, signed),
            MoveDestination::Register(register) => MoveSource::Register(register, signed),
        }
    }

    pub fn register(self) -> register::Register {
        match self {
            Self::Register(register) => register.register,
            _ => unreachable!(),
        }
    }

    pub fn offset(&self) -> Option<&Offset> {
        match self {
            Self::Local(local) => Some(&local.offset),
            Self::Register(register) => register.offset.as_ref(),
            Self::Global(global) => global.offset.as_ref(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            MoveDestination::Global(global) => global.size,
            MoveDestination::Local(local) => local.size,
            MoveDestination::Register(register) => register.size,
        }
    }

    pub fn set_size(&mut self, size: usize) {
        match self {
            MoveDestination::Global(global) => {
                global.size = size;
            }
            MoveDestination::Local(local) => {
                local.size = size;
            }
            MoveDestination::Register(register) => {
                register.size = size;
            }
        };
    }
}
