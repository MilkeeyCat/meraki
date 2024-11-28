use super::{register::Register, OperandSize};
use crate::ir::ExprLit;
use derive_more::derive::Display;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Default, Copy)]
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

impl EffectiveAddress {
    pub fn src(&self, size: OperandSize) -> Source {
        Source::Memory(Memory {
            effective_address: self.clone(),
            size,
        })
    }

    pub fn dest(&self, size: OperandSize) -> Destination {
        Destination::Memory(Memory {
            effective_address: self.clone(),
            size,
        })
    }
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

impl std::ops::Add<Offset> for EffectiveAddress {
    type Output = EffectiveAddress;

    fn add(mut self, rhs: Offset) -> Self::Output {
        self.displacement = Some(&self.displacement.unwrap_or_default() + &rhs);

        self
    }
}

impl From<Register> for EffectiveAddress {
    fn from(value: Register) -> Self {
        EffectiveAddress {
            base: Base::Register(value),
            displacement: None,
            index: None,
            scale: None,
        }
    }
}

#[derive(Debug, Clone, Display)]
pub enum Immediate {
    Int(i64),
    UInt(u64),
    Label(String),
}

impl Into<Immediate> for String {
    fn into(self) -> Immediate {
        Immediate::Label(self)
    }
}

#[derive(Debug, Error)]
#[error("String literal is not a valid assembly immediate")]
pub struct ImmediateStrLitError;

impl TryInto<Immediate> for ExprLit<'_> {
    type Error = ImmediateStrLitError;

    fn try_into(self) -> Result<Immediate, Self::Error> {
        Ok(match self {
            Self::Int(lit) => Immediate::Int(lit),
            Self::UInt(lit) => Immediate::UInt(lit),
            Self::Bool(lit) => Immediate::UInt(lit.into()),
            Self::Null => Immediate::UInt(0),
            Self::String(_) => return Err(ImmediateStrLitError),
        })
    }
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

impl Into<EffectiveAddress> for Destination {
    fn into(self) -> EffectiveAddress {
        match self {
            Self::Memory(mem) => mem.effective_address,
            Self::Register(r) => EffectiveAddress {
                base: Base::Register(r),
                index: None,
                scale: None,
                displacement: None,
            },
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
