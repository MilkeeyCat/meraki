use crate::{
    lexer::{LexerError, Token},
    symbol_table::SymbolTableError,
    types::{Type, TypeError},
};

use super::{ExprError, IntLitReprError, OpParseError};

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, Token),
    ParseType(Token),
    Prefix(Token),
    Infix(Token),
    Lexer(LexerError),
    Type(TypeError),
    Operator(OpParseError),
    Int(IntLitReprError),
    SymbolTable(SymbolTableError),
    UndeclaredFunction(String),
    FunctionArguments(String, Vec<Type>, Vec<Type>),
}

impl std::error::Error for ParserError {}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(expected, actual) => {
                write!(f, "Expected token {expected}, got {actual}")
            }
            Self::ParseType(token) => write!(f, "Failed to parse type, found {token}"),
            Self::Prefix(token) => write!(f, "Failed to parse prefix token {token}"),
            Self::Infix(token) => write!(f, "Failed to parse infix token {token}"),
            Self::Lexer(e) => e.fmt(f),
            Self::Type(e) => e.fmt(f),
            Self::Operator(e) => e.fmt(f),
            Self::Int(e) => e.fmt(f),
            Self::SymbolTable(e) => e.fmt(f),
            Self::UndeclaredFunction(name) => write!(f, "Call to undeclared function {name}"),
            Self::FunctionArguments(name, expected, actual) => write!(
                f,
                "Function {name} has signature ({}), got called with ({})",
                expected
                    .iter()
                    .map(|type_| type_.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                actual
                    .iter()
                    .map(|type_| type_.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl From<TypeError> for ParserError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<SymbolTableError> for ParserError {
    fn from(value: SymbolTableError) -> Self {
        Self::SymbolTable(value)
    }
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        Self::Lexer(value)
    }
}

impl From<ExprError> for ParserError {
    fn from(value: ExprError) -> Self {
        match value {
            ExprError::Type(e) => Self::Type(e),
            ExprError::SymbolTable(e) => Self::SymbolTable(e),
        }
    }
}
