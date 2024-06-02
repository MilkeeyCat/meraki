#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    U8,
    I8,
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

    pub fn resolve(left: Self, right: Self) -> Self {
        if left == right {
            return left;
        }

        if left.int() && right.int() {
            return Self::resolve_ints(left, right);
        }

        todo!();
    }

    fn resolve_ints(mut left: Self, mut right: Self) -> Self {
        if left.signed() || left.signed() {
            left.to_signed();
            right.to_signed();
        }

        if left > right {
            return left;
        }

        right
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
