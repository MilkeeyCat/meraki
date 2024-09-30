use super::TypeError;
use crate::{archs::Arch, scope::Scope};

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct TypeArray {
    pub type_: Box<Type>,
    pub length: usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Usize,
    Isize,
    Bool,
    Void,
    Struct(String),
    Ptr(Box<Type>),
    Array(TypeArray),
    Null,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Usize => write!(f, "usize"),
            Self::Isize => write!(f, "isize"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Ptr(type_) => write!(f, "*{type_}"),
            Self::Struct(name) => write!(f, "struct '{name}'"),
            Self::Array(array) => write!(f, "{}[{}]", array.type_, array.length),
            Self::Null => write!(f, "NULL"),
        }
    }
}

impl Type {
    pub fn int(&self) -> bool {
        match self {
            Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::I8
            | Self::I16
            | Self::I32
            | Self::I64
            | Self::Usize
            | Self::Isize => true,
            _ => false,
        }
    }

    pub fn ptr(&self) -> bool {
        matches!(self, Self::Ptr(..))
    }

    pub fn arr(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    pub fn signed(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::Isize => true,
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
            Self::U32 => {
                *self = Self::I32;
            }
            Self::U64 => {
                *self = Self::I64;
            }
            Self::Usize => {
                *self = Self::Isize;
            }
            _ => (),
        }
    }

    pub fn cast(from: Self, to: Self) -> Result<Self, TypeError> {
        match (from, to) {
            (from, to) if from.int() && to.int() => Ok(to),
            (from, to) if from == Self::Bool && to.int() || from.int() && to == Type::Bool => {
                Ok(to)
            }
            (from, to)
                if from.arr() && to.ptr() && from.inner().unwrap() == to.inner().unwrap() =>
            {
                Ok(to)
            }
            (from, to) if from.ptr() && to.ptr() => Ok(to),
            (from, to) => Err(TypeError::Cast(from, to)),
        }
    }

    pub fn size(&self, arch: &Arch, scope: &Scope) -> Result<usize, TypeError> {
        Ok(match self {
            Type::Void => 0,
            Type::I8 | Type::U8 | Type::Bool => 1,
            Type::I16 | Type::U16 => 2,
            Type::I32 | Type::U32 => 4,
            Type::I64 | Type::U64 => 8,
            Type::Struct(structure) => match scope
                .find_type(structure)
                .ok_or(TypeError::Nonexistent(structure.to_string()))?
            {
                crate::type_table::Type::Struct(structure) => structure.size(arch, scope)?,
            },
            Type::Array(array) => array.type_.size(arch, scope)? * array.length,
            type_ => arch.size(type_),
        })
    }

    pub fn inner(&self) -> Result<Type, TypeError> {
        match self {
            Self::Ptr(type_) => Ok(type_.as_ref().to_owned()),
            Self::Array(array) => Ok(*array.type_.clone()),
            type_ => Err(TypeError::Deref(type_.clone())),
        }
    }

    pub fn struct_unchecked(self) -> String {
        match self {
            Type::Struct(name) => name,
            _ => unreachable!(),
        }
    }

    pub fn promote(from: Self, to: Self) -> Result<Self, TypeError> {
        assert!(from.int());
        assert!(to.int());

        if from <= to {
            Ok(to)
        } else {
            Err(TypeError::Promotion(from, to))
        }
    }

    pub fn common(left: Self, right: Self) -> Result<Self, TypeError> {
        if let Ok(type_) = Self::promote(left.clone(), right.clone()) {
            return Ok(type_);
        }

        if let Ok(type_) = Self::promote(right.clone(), left.clone()) {
            return Ok(type_);
        }

        panic!("Couldn't find common type for {left} & {right}");
    }
}

#[cfg(test)]
mod test {
    use super::{Type, TypeError};

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

    #[test]
    fn casting_to_void() -> Result<(), TypeError> {
        let tests = [
            (Type::U8, Type::Void),
            (Type::Void, Type::U8),
            (Type::I8, Type::Void),
            (Type::Void, Type::I8),
            (Type::U16, Type::Void),
            (Type::Void, Type::U16),
            (Type::I16, Type::Void),
            (Type::Void, Type::I16),
            (Type::Bool, Type::Void),
            (Type::Void, Type::Bool),
        ];

        for (type1, type2) in tests {
            assert!(Type::cast(type1.clone(), type2.clone()).is_err());
        }

        Ok(())
    }
}
