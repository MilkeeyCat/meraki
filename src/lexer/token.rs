use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    Eof,

    Ident(String),
    Int(String),
    String(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Arrow,
    Period,
    Ampersand,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Comma,
    Semicolon,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Const,
    True,
    Enum,
    Struct,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Self {
        return Self { token_type, span };
    }
}
