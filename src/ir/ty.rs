use crate::{
    ast::{IntTy, UintTy},
    ty_problem,
};

pub type AdtIdx = usize;
pub type FieldIdx = usize;

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

#[derive(Debug)]
pub struct AdtDef<'ir> {
    pub name: String,
    pub kind: AdtKind,
    pub variants: Vec<VariantDef<'ir>>,
}

#[derive(Debug, Clone, Copy)]
pub enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[derive(Debug)]
pub struct VariantDef<'ir> {
    pub name: String,
    pub fields: Vec<FieldDef<'ir>>,
}

impl<'ir> VariantDef<'ir> {
    pub fn get_field_by_name(&self, name: &str) -> Option<(FieldIdx, &FieldDef<'ir>)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
    }
}

#[derive(Debug)]
pub struct FieldDef<'ir> {
    pub name: String,
    pub ty: &'ir Ty<'ir>,
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
    Adt(AdtIdx),
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
            Self::Ptr(_) | Self::Fn(_, _) | Self::Adt(_) => f(self),
            Self::Infer(_) => unreachable!(),
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

    pub fn pointee(&self) -> &'ir Ty<'ir> {
        match self {
            Self::Ptr(ty) => ty,
            _ => unreachable!(),
        }
    }

    pub fn adt_idx(&self) -> AdtIdx {
        match self {
            Self::Adt(idx) => *idx,
            _ => unreachable!(),
        }
    }
}
