use super::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Local(StmtLocal),
    Expr(Expr),
    Return(StmtReturn),
}

#[derive(Debug, Clone)]
pub struct StmtLocal {
    name: String,
    value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct StmtReturn(pub Box<Expr>);

impl StmtReturn {
    pub fn new(value: Box<Expr>) -> Self {
        Self(value)
    }
}
