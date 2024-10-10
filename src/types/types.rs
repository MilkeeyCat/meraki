use super::TypeError;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct TypeArray {
    pub type_: Box<Type>,
    pub length: usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl IntType {
    fn size(&self) -> Option<usize> {
        Some(match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::Isize => return None,
        })
    }
}

impl std::fmt::Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum UintType {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl UintType {
    fn size(&self) -> Option<usize> {
        Some(match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::Usize => return None,
        })
    }

    pub fn to_signed(self) -> IntType {
        match self {
            Self::U8 => IntType::I8,
            Self::U16 => IntType::I16,
            Self::U32 => IntType::I32,
            Self::U64 => IntType::I64,
            Self::Usize => IntType::Isize,
        }
    }
}

impl std::fmt::Display for UintType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::Usize => write!(f, "usize"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    Int(IntType),
    UInt(UintType),
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
            Self::Int(int) => int.fmt(f),
            Self::UInt(uint) => uint.fmt(f),
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
    pub fn ptr(&self) -> bool {
        matches!(self, Self::Ptr(..))
    }

    pub fn arr(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    pub fn signed(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    pub fn int(&self) -> bool {
        matches!(self, Type::UInt(_) | Type::Int(_))
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
            (from, to) if from.ptr() && to.int() => Ok(to),
            (from, to) => Err(TypeError::Cast(from, to)),
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Type::Void => Some(0),
            Type::Bool => Some(1),
            Type::Int(int) => int.size(),
            Type::UInt(uint) => uint.size(),
            _ => None,
        }
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

    pub fn common_type(lhs: Type, rhs: Type) -> Type {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => lhs,
            (type_ @ Type::Ptr(_), int) | (int, type_ @ Type::Ptr(_)) if int.int() => type_,
            (Type::UInt(lhs), Type::UInt(rhs)) => {
                if lhs > rhs {
                    Type::UInt(lhs)
                } else {
                    Type::UInt(rhs)
                }
            }
            (Type::Int(lhs), Type::Int(rhs)) => {
                if lhs > rhs {
                    Type::Int(lhs)
                } else {
                    Type::Int(rhs)
                }
            }
            (Type::UInt(uint), Type::Int(int)) | (Type::Int(int), Type::UInt(uint)) => {
                let uint_int = uint.to_signed();

                if uint_int <= int {
                    Type::Int(int)
                } else {
                    Type::Int(uint_int)
                }
            }
            (lhs, rhs) => unreachable!("Failed to get common type for {lhs} and {rhs}"),
        }
    }
}
