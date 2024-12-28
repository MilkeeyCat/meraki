use crate::{
    parser::{IntTy, UintTy},
    ty_problem,
};

#[derive(Debug, PartialEq)]
pub struct TyArray<'ir> {
    pub ty: &'ir Ty<'ir>,
    pub len: usize,
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

#[derive(Debug, PartialEq)]
pub enum Ty<'ir> {
    Void,
    Null,
    Bool,
    Int(IntTy),
    UInt(UintTy),
    Ptr(&'ir Ty<'ir>),
    Array(TyArray<'ir>),
    Fn(&'ir [&'ir Ty<'ir>], &'ir Ty<'ir>),
    Struct(super::Id),
    Infer(ty_problem::Id),
}

impl Ty<'_> {
    pub fn size<F>(&self, f: F) -> usize
    where
        F: Fn(&Ty) -> usize,
    {
        match self {
            Self::Void => 0,
            Self::Null | Self::Bool => 1,
            Self::Int(int) => int.size().unwrap_or_else(|| f(self)),
            Self::UInt(uint) => uint.size().unwrap_or_else(|| f(self)),
            Self::Array(ty_arr) => ty_arr.ty.size(f) * ty_arr.len,
            Self::Ptr(_) | Self::Fn(_, _) | Self::Struct(_) => f(self),
            Self::Infer(_) => unreachable!(),
        }
    }
}

impl std::fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => int.fmt(f),
            Self::UInt(uint) => uint.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Ptr(type_) => write!(f, "*{type_}"),
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
            Self::Struct(_) => write!(f, "owo"),
            Self::Infer(id) => write!(f, "infer({id:?})"),
        }
    }
}

impl<'ir> Ty<'ir> {
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
        matches!(self, Self::UInt(_) | Self::Int(_))
    }
}
