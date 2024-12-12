use super::{register::Register, OperandSize};
use derive_more::derive::Display;

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

#[derive(Clone, Debug, PartialEq, Display)]
pub enum Base {
    Register(Register),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EffectiveAddress {
    pub base: Base,
    pub index: Option<Register>,
    pub scale: Option<usize>,
    pub displacement: Option<Offset>,
}

impl std::fmt::Display for EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("[{}", self.base);

        if let Some(index) = &self.index {
            str.push_str(&format!(" + {}", index));
        }

        if let Some(scale) = self.scale {
            str.push_str(&format!("* {scale}"));
        }

        if let Some(displacement) = &self.displacement {
            str.push_str(&format!("{displacement}"));
        }

        write!(f, "{}]", str)
    }
}

#[derive(Debug, Clone, Display)]
pub enum Immediate {
    Int(i64),
    UInt(u64),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Memory {
    pub effective_address: EffectiveAddress,
    pub size: OperandSize,
}

impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.size, self.effective_address)
    }
}

#[derive(Debug, Clone)]
pub enum Source {
    Memory(Memory),
    Register(Register),
    Immediate(Immediate),
}

impl Source {
    pub fn size(&self) -> Option<OperandSize> {
        match self {
            Self::Memory(mem) => Some(mem.size.clone()),
            Self::Register(reg) => Some(reg.size()),
            Self::Immediate(_) => None,
        }
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(mem) => mem.fmt(f),
            Self::Register(r) => r.fmt(f),
            Self::Immediate(imm) => imm.fmt(f),
        }
    }
}

impl Into<Source> for i64 {
    fn into(self) -> Source {
        Source::Immediate(Immediate::Int(self))
    }
}

impl Into<Source> for u64 {
    fn into(self) -> Source {
        Source::Immediate(Immediate::UInt(self))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Destination {
    Memory(Memory),
    Register(Register),
}

impl Destination {
    pub fn size(&self) -> OperandSize {
        match self {
            Self::Memory(mem) => mem.size.clone(),
            Self::Register(reg) => reg.size(),
        }
    }
}

impl Into<Source> for Destination {
    fn into(self) -> Source {
        match self {
            Self::Memory(mem) => Source::Memory(mem),
            Self::Register(r) => Source::Register(r),
        }
    }
}

impl std::fmt::Display for Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(mem) => mem.fmt(f),
            Self::Register(r) => r.fmt(f),
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
