mod ordered_map;
mod types;

use crate::ast::{BinOp, UnOp};
use bumpalo::Bump;

pub use ordered_map::OrderedMap;
pub use types::{Ty, TyArray};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Hash)]
pub struct Id {
    pub global_id: usize,
    pub node_id: usize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Expr<'ir> {
    pub ty: &'ir Ty<'ir>,
    pub kind: ExprKind<'ir>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ExprKind<'ir> {
    // It's a reference only because it doesn't work without indirection
    Binary(BinOp, &'ir Expr<'ir>, &'ir Expr<'ir>),
    Unary(UnOp, &'ir Expr<'ir>),
    Ident(Id),
    Lit(ExprLit<'ir>),
    Struct(&'ir [(&'ir str, Expr<'ir>)]),
    Field(&'ir Expr<'ir>, &'ir str),
    Cast(&'ir Expr<'ir>, &'ir Ty<'ir>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExprLit<'ir> {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(&'ir str),
    Null,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Stmt<'ir> {
    Local(&'ir Variable<'ir>),
    Item(Item<'ir>),
    Expr(Expr<'ir>),
    Return(Option<Expr<'ir>>),
}

#[derive(Debug, PartialEq)]
pub struct Block<'ir>(pub &'ir [Stmt<'ir>]);

#[derive(Debug, PartialEq)]
pub struct Signature<'ir> {
    pub params: &'ir [&'ir Ty<'ir>],
    pub ret_ty: &'ir Ty<'ir>,
}

#[derive(Debug, PartialEq)]
pub struct ItemFn<'ir> {
    pub id: Id,
    pub name: &'ir str,
    pub signature: Signature<'ir>,
    pub block: Block<'ir>,
}

#[derive(Debug, PartialEq)]
pub struct Variable<'ir> {
    pub id: Id,
    pub name: &'ir str,
    pub ty: &'ir Ty<'ir>,
    pub initializer: Option<Expr<'ir>>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Item<'ir> {
    Fn(&'ir ItemFn<'ir>),
    Global(&'ir Variable<'ir>),
    Struct(&'ir [(&'ir str, &'ir Ty<'ir>)]),
}

#[derive(Debug, Clone, Copy)]
pub enum Node<'ir> {
    Item(Item<'ir>),
    Stmt(Stmt<'ir>),
    Expr(Expr<'ir>),
}

#[derive(Debug, Clone, Copy)]
pub struct Global<'ir>(pub &'ir [Node<'ir>]);

#[derive(Debug)]
pub struct Ir<'ir> {
    allocator: &'ir Bump,
    globals: &'ir [Global<'ir>],
}

impl<'ir> Ir<'ir> {
    pub fn new(allocator: &'ir Bump) -> Self {
        Ir {
            allocator,
            globals: &[],
        }
    }

    pub fn set_globals(&mut self, globals: &'ir [Global<'ir>]) {
        self.globals = globals;
    }

    pub fn iter_items(&self) -> impl Iterator<Item = Item<'ir>> {
        self.globals
            .iter()
            .map(|global| match global.0[0] {
                Node::Item(item) => item,
                _ => unreachable!(),
            })
            .into_iter()
    }

    pub fn get_node(&self, id: Id) -> &'ir Node<'ir> {
        &self.globals[id.global_id].0[id.node_id]
    }
}
