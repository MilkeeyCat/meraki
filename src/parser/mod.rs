mod error;
mod item;
mod op;
mod parser;
mod precedence;
mod stmt;
mod types;

pub mod expr;

pub use error::{ParserError, TyError};
pub use expr::*;
pub use item::{Item, ItemFn, ItemStruct};
pub use op::{BinOp, BitwiseOp, CmpOp, OpParseError, UnOp};
pub use parser::Parser;
pub use precedence::Precedence;
pub use stmt::{Stmt, StmtFor, StmtIf, StmtReturn, StmtWhile};
pub use types::{IntTy, Ty, TyArray, UintTy};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: Ty,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Stmt>);
