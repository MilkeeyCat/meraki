mod error;
pub mod expr;
mod item;
mod op;
mod parser;
mod precedence;
mod stmt;
mod types;

pub use error::{ParserError, TyError};
pub use expr::*;
pub use item::{Item, ItemFn, ItemStruct, ItemVar};
pub use op::{BinOp, BitwiseOp, CmpOp, OpParseError, UnOp};
pub use parser::Parser;
pub use precedence::Precedence;
pub use stmt::{Stmt, StmtFor, StmtIf, StmtReturn, StmtWhile};
pub use types::{IntTy, Ty, TyArray, UintTy};

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Stmt>);
