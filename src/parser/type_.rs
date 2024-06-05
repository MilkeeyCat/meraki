use std::fmt::Display;

use crate::archs::Architecture;

#[derive(Debug)]
pub enum TypeError {
    Promotion(Type, Type),
    IdentNotFound(String),
    Assignment(Type, Type),
    Cast(Type, Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentNotFound(ident) => write!(f, "Ident {} not found", ident),
            Self::Promotion(lhs, rhs) => {
                write!(f, "Operation between {} and {} are not allowed", lhs, rhs)
            }
            Self::Assignment(lhs, rhs) => write!(f, "Can't assign {} to {}", lhs, rhs),
            Self::Cast(from, to) => write!(f, "Can't cast {} into {}", from, to),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    U8,
    I8,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::I8 => write!(f, "i8"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    fn int(&self) -> bool {
        match self {
            Self::U8 | Self::I8 => true,
            _ => false,
        }
    }

    fn bool(&self) -> bool {
        if self == &Self::Bool {
            true
        } else {
            false
        }
    }

    fn signed(&self) -> bool {
        match self {
            Self::I8 => true,
            _ => false,
        }
    }

    pub fn to_signed(&mut self) {
        match self {
            Self::U8 => {
                *self = Self::I8;
            }
            _ => {}
        }
    }

    pub fn promote(self, type_: Self) -> Result<Self, TypeError> {
        if self == type_ {
            return Ok(self);
        }

        if self.int() && type_.int() {
            return self.promote_ints(type_);
        }

        Err(TypeError::Promotion(self, type_))
    }

    fn promote_ints(mut self, mut type_: Self) -> Result<Self, TypeError> {
        if self.signed() || type_.signed() {
            self.to_signed();
            type_.to_signed();
        }

        if self > type_ {
            return Ok(self);
        }

        Ok(type_)
    }

    pub fn assign(self, type_: Self) -> Result<Self, TypeError> {
        if self == type_ {
            return Ok(self);
        }

        if self.int() && type_.int() {
            if (self.signed() && !type_.signed()) || (!self.signed() && type_.signed()) {
                return Err(TypeError::Assignment(type_, self));
            }

            if self >= type_ {
                return Ok(self);
            }
        }

        return Err(TypeError::Assignment(type_, self));
    }

    pub fn cast(self, type_: Self) -> Result<Self, TypeError> {
        if self.int() && type_.int() {
            return Ok(type_);
        }

        if self.bool() && type_.int() {
            return Ok(type_);
        }

        if type_.int() && self.bool() {
            return Ok(self);
        }

        return Err(TypeError::Cast(self, type_));
    }

    pub fn size<Arch: Architecture>(&self) -> usize {
        match self {
            Type::I8 | Type::U8 | Type::Bool => 1,
            _ => Arch::size(self),
        }
    }
}
