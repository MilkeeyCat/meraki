use crate::{
    archs::Arch,
    parser::ExprLit,
    register::{self, allocator::AllocatorError},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Offset(pub isize);

impl std::ops::Add<isize> for &Offset {
    type Output = Offset;

    fn add(self, rhs: isize) -> Self::Output {
        Offset(self.0 + rhs)
    }
}

impl std::ops::Sub<isize> for &Offset {
    type Output = Offset;

    fn sub(self, rhs: isize) -> Self::Output {
        Offset(self.0 - rhs)
    }
}

impl std::ops::Add<&Offset> for &Offset {
    type Output = Offset;

    fn add(self, rhs: &Offset) -> Self::Output {
        Offset(self.0 + rhs.0)
    }
}

impl std::ops::Sub<&Offset> for &Offset {
    type Output = Offset;

    fn sub(self, rhs: &Offset) -> Self::Output {
        Offset(self.0 - rhs.0)
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

#[derive(Clone, Debug, PartialEq)]
pub enum Base {
    Register(register::Register),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Register {
    pub register: register::Register,
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectiveAddress {
    pub base: Base,
    pub index: Option<register::Register>,
    pub scale: Option<usize>,
    pub displacement: Option<Offset>,
}

#[derive(Debug, Clone)]
pub enum Immediate {
    Int(i64),
    UInt(u64),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Memory {
    pub effective_address: EffectiveAddress,
    pub size: usize,
}

impl From<ExprLit> for Immediate {
    fn from(value: ExprLit) -> Self {
        match value {
            ExprLit::UInt(uint) => Self::UInt(uint.inner),
            ExprLit::Int(int) => Self::Int(int.inner),
            ExprLit::String(label) => Self::Label(label),
            ExprLit::Bool(bool) => Self::UInt(if bool { 1 } else { 0 }),
            ExprLit::Null => Self::UInt(0),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Source {
    Memory(Memory),
    Register(Register),
    Immediate(Immediate),
}

impl Source {
    pub fn size(&self) -> Option<usize> {
        match self {
            Self::Memory(memory) => Some(memory.size),
            Self::Register(register) => Some(register.size),
            Self::Immediate(_) => None,
        }
    }

    pub fn free(self, arch: &mut Arch) -> Result<(), AllocatorError> {
        match self {
            Self::Register(Register { register, .. }) => arch.free(register),
            Self::Memory(Memory {
                effective_address:
                    EffectiveAddress {
                        base: Base::Register(register),
                        ..
                    },
                ..
            }) => arch.free(register),
            _ => Ok(()),
        }
    }
}

impl TryInto<Destination> for Source {
    type Error = ();

    fn try_into(self) -> Result<Destination, Self::Error> {
        match self {
            Self::Memory(memory) => Ok(Destination::Memory(memory)),
            Self::Register(register) => Ok(Destination::Register(register)),
            // FIXME: return an error
            Self::Immediate(_) => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Destination {
    Memory(Memory),
    Register(Register),
}

impl Destination {
    pub fn with_size(mut self, size: usize) -> Self {
        match &mut self {
            Self::Memory(memory) => {
                memory.size = size;
            }
            Self::Register(register) => {
                register.size = size;
            }
        }

        self
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Memory(memory) => memory.size,
            Self::Register(register) => register.size,
        }
    }
}

impl Into<Source> for Destination {
    fn into(self) -> Source {
        match self {
            Self::Memory(memory) => Source::Memory(memory),
            Self::Register(register) => Source::Register(register),
        }
    }
}

impl Into<EffectiveAddress> for Destination {
    fn into(self) -> EffectiveAddress {
        match self {
            Destination::Register(register) => EffectiveAddress {
                base: Base::Register(register.register),
                index: None,
                scale: None,
                displacement: None,
            },
            Destination::Memory(memory) => memory.effective_address,
        }
    }
}

impl Into<EffectiveAddress> for Source {
    fn into(self) -> EffectiveAddress {
        match self {
            Source::Register(register) => EffectiveAddress {
                base: Base::Register(register.register),
                index: None,
                scale: None,
                displacement: None,
            },
            Source::Memory(memory) => memory.effective_address,
            Source::Immediate(_) => unreachable!(),
        }
    }
}

impl Into<Base> for Destination {
    fn into(self) -> Base {
        match self {
            Destination::Register(register) => Base::Register(register.register),
            Destination::Memory(memory) => memory.effective_address.base,
        }
    }
}

impl PartialEq<&Source> for &Destination {
    fn eq(&self, other: &&Source) -> bool {
        match (self, other) {
            (Destination::Memory(lhs), Source::Memory(rhs)) if lhs == rhs => true,
            (Destination::Register(lhs), Source::Register(rhs)) if lhs == rhs => true,
            _ => false,
        }
    }
}

impl PartialEq<&Destination> for &Source {
    fn eq(&self, other: &&Destination) -> bool {
        other == self
    }
}
