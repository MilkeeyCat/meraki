mod ordered_map;
mod types;

use bumpalo::Bump;
pub use types::{IntTy, Ty, TyArray, UintTy};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Hash)]
pub struct Id {
    pub global_id: usize,
    pub node_id: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Expr<'ir> {
    pub ty: &'ir Ty<'ir>,
    pub kind: ExprKind<'ir>,
}

#[derive(Debug, Copy, Clone)]
pub enum ExprKind<'ir> {
    Ident(Id),
    Lit(ExprLit<'ir>),
}

#[derive(Debug, Copy, Clone)]
pub enum ExprLit<'ir> {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(&'ir str),
    Null,
}

#[derive(Debug, Copy, Clone)]
pub enum Stmt<'ir> {
    Item(Item<'ir>),
    Expr(Expr<'ir>),
    Return(Option<Expr<'ir>>),
}

#[derive(Debug)]
pub struct Block<'ir>(pub &'ir [Stmt<'ir>]);

#[derive(Debug)]
pub struct Signature<'ir> {
    pub params: &'ir [&'ir Ty<'ir>],
    pub ret_ty: &'ir Ty<'ir>,
}

#[derive(Debug)]
pub struct ItemFn<'ir> {
    pub id: Id,
    pub name: &'ir str,
    pub signature: Signature<'ir>,
    pub block: Block<'ir>,
}

#[derive(Debug)]
pub struct ItemVar<'ir> {
    pub id: Id,
    pub name: &'ir str,
    pub ty: &'ir Ty<'ir>,
    pub initializer: Option<Expr<'ir>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Item<'ir> {
    Fn(&'ir ItemFn<'ir>),
    Var(&'ir ItemVar<'ir>),
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

    pub fn iter_items(&self) -> impl Iterator<Item = Item> {
        self.globals
            .iter()
            .map(|global| match global.0[0] {
                Node::Item(item) => item,
                _ => unreachable!(),
            })
            .into_iter()
    }
}
