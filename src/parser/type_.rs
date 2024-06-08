use crate::archs::Architecture;
use std::fmt::Display;

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
    U16,
    I8,
    I16,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

impl Type {
    fn int(&self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::I8 | Self::I16 => true,
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

    pub fn signed(&self) -> bool {
        match self {
            Self::I8 | Self::I16 => true,
            _ => false,
        }
    }

    pub fn to_signed(&mut self) {
        match self {
            Self::U8 => {
                *self = Self::I8;
            }
            Self::U16 => {
                *self = Self::I16;
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
        if (self == type_) || (self.int() && type_.int()) {
            return Ok(type_);
        }

        if (self.bool() && type_.int()) || (self.int() && type_.bool()) {
            return Ok(type_);
        }

        return Err(TypeError::Cast(self, type_));
    }

    pub fn size<Arch: Architecture>(&self) -> usize {
        match self {
            Type::I8 | Type::U8 | Type::Bool => 1,
            Type::I16 | Type::U16 => 2,
            _ => Arch::size(self),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Type, TypeError};

    #[test]
    fn int_promotion() -> Result<(), TypeError> {
        let tests = [
            ((Type::U8, Type::U8), Type::U8),
            ((Type::U8, Type::I8), Type::I8),
            ((Type::I8, Type::U8), Type::I8),
            ((Type::I8, Type::I8), Type::I8),
            ((Type::U8, Type::U16), Type::U16),
            ((Type::U8, Type::I16), Type::I16),
            ((Type::I8, Type::U16), Type::I16),
            ((Type::I8, Type::I16), Type::I16),
        ];

        for (types, expected) in tests {
            assert_eq!(Type::promote(types.0, types.1)?, expected);
        }

        Ok(())
    }

    #[test]
    fn casting() -> Result<(), TypeError> {
        let tests = [
            //8
            (Type::U8, Type::U8),
            (Type::U8, Type::I8),
            (Type::I8, Type::U8),
            (Type::I8, Type::I8),
            //16
            (Type::U16, Type::U16),
            (Type::U16, Type::I16),
            (Type::I16, Type::U16),
            (Type::I16, Type::I16),
            //8-16
            (Type::U8, Type::U16),
            (Type::U8, Type::I16),
            (Type::I8, Type::U16),
            (Type::I8, Type::I16),
            //16-8
            (Type::U16, Type::U8),
            (Type::I16, Type::U8),
            (Type::U16, Type::I8),
            (Type::I16, Type::I8),
            //8-bool
            (Type::U8, Type::Bool),
            (Type::I8, Type::Bool),
            (Type::Bool, Type::U8),
            (Type::Bool, Type::I8),
            //bool-8
            (Type::Bool, Type::U8),
            (Type::Bool, Type::I8),
            (Type::U8, Type::Bool),
            (Type::I8, Type::Bool),
            //16-bool
            (Type::U16, Type::Bool),
            (Type::I16, Type::Bool),
            (Type::Bool, Type::U16),
            (Type::Bool, Type::I16),
            //bool-16
            (Type::Bool, Type::U16),
            (Type::Bool, Type::I16),
            (Type::U16, Type::Bool),
            (Type::I16, Type::Bool),
        ];

        for (type1, type2) in tests {
            assert!(
                Type::cast(type1.clone(), type2.clone()).is_ok(),
                "{}, {} big oops",
                type1,
                type2
            );
        }

        Ok(())
    }
}
