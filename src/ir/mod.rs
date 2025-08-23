pub mod ty;
pub mod visitor;

use crate::ast::{BinOp, UnOp};
use std::collections::HashMap;
pub use ty::{Ty, TyArray};

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Id(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct SymbolId(Id);

impl From<Id> for SymbolId {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr<'ir> {
    pub id: Id,
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
pub enum Symbol<'ir> {
    Fn {
        ty: &'ir Ty<'ir>,
        params: Vec<SymbolId>,
    },
    Variable(&'ir Ty<'ir>),
}

impl<'ir> Symbol<'ir> {
    pub fn ty(&self) -> &'ir Ty<'ir> {
        match self {
            Self::Fn { ty, .. } => ty,
            Self::Variable(ty) => ty,
        }
    }
}

#[derive(Debug)]
pub struct Package<'ir> {
    pub items: Vec<Item<'ir>>,
    pub symbols: HashMap<SymbolId, Symbol<'ir>>,
}

impl<'ir> Package<'ir> {
    pub fn new() -> Self {
        Package {
            items: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, id: SymbolId, symbol: Symbol<'ir>) {
        assert!(self.symbols.insert(id, symbol).is_none());
    }
}
