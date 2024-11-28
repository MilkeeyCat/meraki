use super::{Block, Expr, Ty};

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Var(ItemVar),
    Fn(ItemFn),
    Struct(ItemStruct),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemVar {
    pub ty: Ty,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemFn {
    pub ret_ty: Ty,
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemStruct {
    pub name: String,
    pub fields: Vec<(String, Ty)>,
}
