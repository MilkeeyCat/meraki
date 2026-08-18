use crate::{
    ast::{IntTy, UintTy},
    typecheck::ty_problem,
};

pub(crate) type AdtIdx = usize;
pub(crate) type FieldIdx = usize;

#[derive(Debug, PartialEq)]
pub(crate) struct TyArray<'ir> {
    pub(crate) ty: &'ir Ty<'ir>,
    pub(crate) len: usize,
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

    fn to_signed(self) -> IntTy {
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
pub(crate) struct AdtDef<'ir> {
    pub(crate) name: String,
    pub(crate) kind: AdtKind,
    pub(crate) variants: Vec<VariantDef<'ir>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[derive(Debug)]
pub(crate) struct VariantDef<'ir> {
    pub(crate) name: String,
    pub(crate) fields: Vec<FieldDef<'ir>>,
}

impl<'ir> VariantDef<'ir> {
    pub(crate) fn get_field_by_name(&self, name: &str) -> Option<(FieldIdx, &FieldDef<'ir>)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
    }
}

#[derive(Debug)]
pub(crate) struct FieldDef<'ir> {
    pub(crate) name: String,
    pub(crate) ty: &'ir Ty<'ir>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InferTy {
    TyVar(ty_problem::Id),
    IntVar(ty_problem::Id),
}

impl Into<ty_problem::Id> for InferTy {
    fn into(self) -> ty_problem::Id {
        match self {
            Self::TyVar(id) => id,
            Self::IntVar(id) => id,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Ty<'ir> {
    Void,
    Null,
    Bool,
    Int(IntTy),
    UInt(UintTy),
    Ptr(&'ir Ty<'ir>),
    Array(TyArray<'ir>),
    Fn(&'ir [&'ir Ty<'ir>], &'ir Ty<'ir>),
    Adt(AdtIdx),
    Infer(Option<InferTy>),
}

impl Ty<'_> {
    fn size<F>(&self, f: F) -> usize
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
    fn ptr(&self) -> bool {
        matches!(self, Self::Ptr(..))
    }

    fn arr(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    fn signed(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    fn int(&self) -> bool {
        matches!(self, Self::UInt(_) | Self::Int(_))
    }

    fn pointee(&self) -> &'ir Ty<'ir> {
        match self {
            Self::Ptr(ty) => ty,
            _ => unreachable!(),
        }
    }

    pub(crate) fn adt_idx(&self) -> AdtIdx {
        match self {
            Self::Adt(idx) => *idx,
            _ => unreachable!(),
        }
    }
}
