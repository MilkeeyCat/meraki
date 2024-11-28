use super::error::TyError;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct TyArray {
    pub ty: Box<Ty>,
    pub len: usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl IntTy {
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

impl std::fmt::Display for IntTy {
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
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl UintTy {
    fn size(&self) -> Option<usize> {
        Some(match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::Usize => return None,
        })
    }

    pub fn to_signed(self) -> IntTy {
        match self {
            Self::U8 => IntTy::I8,
            Self::U16 => IntTy::I16,
            Self::U32 => IntTy::I32,
            Self::U64 => IntTy::I64,
            Self::Usize => IntTy::Isize,
        }
    }
}

impl std::fmt::Display for UintTy {
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
pub enum Ty {
    Null,
    Void,
    Bool,
    Int(IntTy),
    UInt(UintTy),
    Ident(String),
    Ptr(Box<Ty>),
    Array(TyArray),
    Fn(Vec<Ty>, Box<Ty>),
    Infer,
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => int.fmt(f),
            Self::UInt(uint) => uint.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Ptr(type_) => write!(f, "*{type_}"),
            Self::Ident(name) => write!(f, "{name}"),
            Self::Array(array) => write!(f, "{}[{}]", array.ty, array.len),
            Self::Fn(params, return_type) => write!(
                f,
                "fn ({}) -> {return_type}",
                params
                    .iter()
                    .map(|type_| type_.to_string())
                    .collect::<String>()
            ),
            Self::Null => write!(f, "NULL"),
            Self::Infer => unreachable!(),
        }
    }
}

impl Ty {
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
        matches!(self, Ty::UInt(_) | Ty::Int(_))
    }

    pub fn cast(from: Self, to: Self) -> Result<Self, TyError> {
        match (from, to) {
            (from, to) if from.int() && to.int() => Ok(to),
            (from, to) if from == Self::Bool && to.int() || from.int() && to == Ty::Bool => Ok(to),
            (from, to)
                if from.arr() && to.ptr() && from.inner().unwrap() == to.inner().unwrap() =>
            {
                Ok(to)
            }
            (Ty::Array(_), Ty::Ptr(pointee)) if pointee.as_ref() == &Ty::Void => {
                Ok(Ty::Ptr(pointee))
            }
            (from, to) if from.ptr() && to.ptr() => Ok(to),
            (from, to) if from.ptr() && to.int() => Ok(to),
            (from, to) => Err(TyError::Cast(from, to)),
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Ty::Void => Some(0),
            Ty::Bool => Some(1),
            Ty::Int(int) => int.size(),
            Ty::UInt(uint) => uint.size(),
            _ => None,
        }
    }

    pub fn inner(&self) -> Result<Ty, TyError> {
        match self {
            Self::Ptr(type_) => Ok(type_.as_ref().to_owned()),
            Self::Array(array) => Ok(*array.ty.clone()),
            type_ => Err(TyError::Deref(type_.clone())),
        }
    }

    pub fn common_type(lhs: Ty, rhs: Ty) -> Ty {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => lhs,
            (type_ @ Ty::Ptr(_), int) | (int, type_ @ Ty::Ptr(_)) if int.int() => type_,
            (type_ @ Ty::Ptr(_), Ty::Null) | (Ty::Null, type_ @ Ty::Ptr(_)) => type_,
            (Ty::UInt(lhs), Ty::UInt(rhs)) => {
                if lhs > rhs {
                    Ty::UInt(lhs)
                } else {
                    Ty::UInt(rhs)
                }
            }
            (Ty::Int(lhs), Ty::Int(rhs)) => {
                if lhs > rhs {
                    Ty::Int(lhs)
                } else {
                    Ty::Int(rhs)
                }
            }
            (Ty::UInt(uint), Ty::Int(int)) | (Ty::Int(int), Ty::UInt(uint)) => {
                let uint_int = uint.to_signed();

                if uint_int <= int {
                    Ty::Int(int)
                } else {
                    Ty::Int(uint_int)
                }
            }
            (lhs, rhs) => unreachable!("Failed to get common type for {lhs} and {rhs}"),
        }
    }
}
