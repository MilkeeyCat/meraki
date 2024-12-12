use super::{item::Item, Block, Expr, Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Local(Variable),
    Item(Item),
    Expr(Expr),
    Return(StmtReturn),
    If(StmtIf),
    While(StmtWhile),
    For(StmtFor),
    Continue,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtReturn {
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtIf {
    pub condition: Expr,
    pub consequence: Block,
    pub alternative: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtWhile {
    pub condition: Expr,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StmtFor {
    pub initializer: Option<Box<Stmt>>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub block: Block,
}
