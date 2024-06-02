use std::fmt::Display;

#[derive(Debug)]
pub enum TypeError {
    Promotion(Type, Type),
    IdentNotFound(String),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentNotFound(ident) => write!(f, "Ident {} not found", ident),
            Self::Promotion(lhs, rhs) => {
                write!(f, "Operation between {} and {} are not allowed", lhs, rhs)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    U8,
    I8,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::I8 => write!(f, "i8"),
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

    pub fn assignable(&self, type_: &Self) -> bool {
        if self.int() && type_.int() {
            if (self.signed() && !type_.signed()) || (!self.signed() && type_.signed()) {
                return false;
            }

            if self >= type_ {
                return true;
            }
        }

        false
    }
}
