mod error;
mod symbol_table;

pub use error::SymbolTableError;
pub use symbol_table::{
    Symbol, SymbolFunction, SymbolGlobal, SymbolLocal, SymbolParam, SymbolTable,
};
