#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    U8,
    I8,
}

#[derive(Debug)]
pub enum TypeError {
    Promotion(Type, Type),
    IdentNotFound(String),
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
