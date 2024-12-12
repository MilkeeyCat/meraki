use super::{Block, Ty, Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Global(Variable),
    Fn(ItemFn),
    Struct(ItemStruct),
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
