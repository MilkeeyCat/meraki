pub(crate) mod ty;
pub(crate) mod visitor;

pub(crate) use ty::{Ty, TyArray};

use crate::ast::{BinOp, UnOp};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub(crate) struct Id(pub(crate) usize);

impl From<SymbolId> for Id {
    fn from(value: SymbolId) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub(crate) struct SymbolId(Id);

impl From<Id> for SymbolId {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub(crate) struct ExprId(Id);

impl From<Id> for ExprId {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Expr<'ir> {
    pub(crate) id: Id,
    pub(crate) kind: Box<ExprKind<'ir>>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ExprKind<'ir> {
    Binary(BinOp, Expr<'ir>, Expr<'ir>),
    Unary(UnOp, Expr<'ir>),
    Ident(SymbolId),
    Lit(ExprLit),
    Struct(&'ir Ty<'ir>, Vec<(String, Expr<'ir>)>),
    Field(Expr<'ir>, String),
    Call(Expr<'ir>, Vec<Expr<'ir>>),
    Cast(Expr<'ir>, &'ir Ty<'ir>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ExprLit {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Stmt<'ir> {
    pub(crate) id: Id,
    pub(crate) kind: StmtKind<'ir>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum StmtKind<'ir> {
    Local(Variable<'ir>),
    Item(Item<'ir>),
    Expr(Expr<'ir>),
    Return(Option<Expr<'ir>>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Block<'ir>(pub(crate) Vec<Stmt<'ir>>);

#[derive(Debug, PartialEq)]
pub(crate) struct ItemFn<'ir> {
    pub(crate) name: String,
    pub(crate) params: Vec<(Id, &'ir Ty<'ir>)>,
    pub(crate) ret_ty: &'ir Ty<'ir>,
    pub(crate) block: Option<Block<'ir>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Variable<'ir> {
    pub(crate) name: String,
    pub(crate) ty: &'ir Ty<'ir>,
    pub(crate) value: Option<Expr<'ir>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Item<'ir> {
    pub(crate) id: Id,
    pub(crate) kind: ItemKind<'ir>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum ItemKind<'ir> {
    Fn(ItemFn<'ir>),
    Global(Variable<'ir>),
}

#[derive(Debug)]
pub(crate) enum Symbol<'ir> {
    Fn {
        ty: &'ir Ty<'ir>,
        params: Vec<SymbolId>,
    },
    Variable(&'ir Ty<'ir>),
}

impl<'ir> Symbol<'ir> {
    pub(crate) fn ty(&self) -> &'ir Ty<'ir> {
        match self {
            Self::Fn { ty, .. } => ty,
            Self::Variable(ty) => ty,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Package<'ir> {
    pub(crate) items: Vec<Item<'ir>>,
    pub(crate) symbols: HashMap<SymbolId, Symbol<'ir>>,
    pub(crate) expr_tys: HashMap<ExprId, &'ir Ty<'ir>>,
}

impl<'ir> Package<'ir> {
    pub(crate) fn new() -> Self {
        Package {
            items: Vec::new(),
            symbols: HashMap::new(),
            expr_tys: HashMap::new(),
        }
    }

    pub(crate) fn add_symbol(&mut self, id: SymbolId, symbol: Symbol<'ir>) {
        assert!(self.symbols.insert(id, symbol).is_none());
    }
}
