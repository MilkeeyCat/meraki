pub mod ty;
pub mod visitor;

use crate::ast::{BinOp, UnOp};
pub use ty::{Ty, TyArray};

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Id(pub usize);

#[derive(Debug, PartialEq)]
pub struct Expr<'ir> {
    pub id: Id,
    pub ty: &'ir Ty<'ir>,
    pub kind: Box<ExprKind<'ir>>,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind<'ir> {
    Binary(BinOp, Expr<'ir>, Expr<'ir>),
    Unary(UnOp, Expr<'ir>),
    Ident(Id),
    Lit(ExprLit),
    Struct(Vec<(String, Expr<'ir>)>),
    Field(Expr<'ir>, String),
    Cast(Expr<'ir>, &'ir Ty<'ir>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprLit {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, PartialEq)]
pub struct Stmt<'ir> {
    pub id: Id,
    pub kind: StmtKind<'ir>,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind<'ir> {
    Local(Variable<'ir>),
    Item(Item<'ir>),
    Expr(Expr<'ir>),
    Return(Option<Expr<'ir>>),
}

#[derive(Debug, PartialEq)]
pub struct Block<'ir>(pub Vec<Stmt<'ir>>);

#[derive(Debug, PartialEq)]
pub struct ItemFn<'ir> {
    pub name: String,
    pub params: Vec<(Id, &'ir Ty<'ir>)>,
    pub ret_ty: &'ir Ty<'ir>,
    pub block: Option<Block<'ir>>,
}

#[derive(Debug, PartialEq)]
pub struct Variable<'ir> {
    pub name: String,
    pub ty: &'ir Ty<'ir>,
    pub value: Option<Expr<'ir>>,
}

#[derive(Debug, PartialEq)]
pub struct Item<'ir> {
    pub id: Id,
    pub kind: ItemKind<'ir>,
}

#[derive(Debug, PartialEq)]
pub enum ItemKind<'ir> {
    Fn(ItemFn<'ir>),
    Global(Variable<'ir>),
}

#[derive(Debug)]
pub struct Package<'ir> {
    pub root_items: Vec<Item<'ir>>,
}

impl<'ir> Package<'ir> {
    pub fn new() -> Self {
        Package {
            root_items: Vec::new(),
        }
    }
}
