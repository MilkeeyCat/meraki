use super::{ExprError, IntLitReprError, OpParseError};
use crate::{
    lexer::{LexerError, Token},
    symbol_table::SymbolTableError,
    types::{Type, TypeError},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error(transparent)]
    Expr(#[from] ExprError),
    #[error(transparent)]
    Lexer(#[from] LexerError),
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    Operator(#[from] OpParseError),
    #[error(transparent)]
    Int(#[from] IntLitReprError),
    #[error(transparent)]
    SymbolTable(#[from] SymbolTableError),
    #[error("Expected token {0}, got {1}")]
    UnexpectedToken(Token, Token),
    #[error("Expected {0}")]
    Expected(Token),
    #[error("Failed to parse type, found {0}")]
    ParseType(Token),
    #[error("Failed to parse prefix token {0}")]
    Prefix(Token),
    #[error("Failed to parse infix token {0}")]
    Infix(Token),
    #[error("Call to undeclared function {0}")]
    UndeclaredFunction(String),
    #[error("Function has signature ({}), got called with ({})",
        .0
            .iter()
            .map(|type_| type_.to_string())
            .collect::<Vec<String>>()
            .join(", "),
        .1
            .iter()
            .map(|type_| type_.to_string())
            .collect::<Vec<String>>()
            .join(", ")
    )]
    FunctionArguments(Vec<Type>, Vec<Type>),
}
