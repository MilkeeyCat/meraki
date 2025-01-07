use derive_more::derive::Display;
use std::{
    hash::{Hash, Hasher},
    mem::discriminant,
};

#[derive(Debug, Clone, Eq, Display)]
pub enum TokenKind {
    #[display("ident")]
    Ident(String),
    #[display("integer literal")]
    Integer(String),
    #[display("string literal")]
    String(String),

    #[display("=")]
    Assign,
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("!")]
    Bang,
    #[display("*")]
    Asterisk,
    #[display("/")]
    Slash,
    #[display("->")]
    Arrow,
    #[display(".")]
    Period,
    #[display("~")]
    Tilde,
    #[display("&")]
    Ampersand,
    #[display("|")]
    Bar,
    #[display("==")]
    Equal,
    #[display("!=")]
    NotEqual,
    #[display("<")]
    LessThan,
    #[display(">")]
    GreaterThan,
    #[display("<=")]
    LessEqual,
    #[display(">=")]
    GreaterEqual,
    #[display("&&")]
    And,
    #[display("||")]
    Or,
    #[display("<<")]
    Shl,
    #[display(">>")]
    Shr,
    #[display(",")]
    Comma,
    #[display(";")]
    Semicolon,
    #[display(":")]
    Colon,
    #[display("(")]
    LParen,
    #[display(")")]
    RParen,
    #[display("{{")]
    LBrace,
    #[display("}}")]
    RBrace,
    #[display("[")]
    LBracket,
    #[display("]")]
    RBracket,

    #[display("const")]
    Const,
    #[display("true")]
    True,
    #[display("false")]
    False,
    #[display("let")]
    Let,
    #[display("fn")]
    Fn,
    #[display("enum")]
    Enum,
    #[display("struct")]
    Struct,
    #[display("if")]
    If,
    #[display("while")]
    While,
    #[display("for")]
    For,
    #[display("else")]
    Else,
    #[display("return")]
    Return,
    #[display("as")]
    As,
    #[display("continue")]
    Continue,
    #[display("break")]
    Break,

    #[display("u8")]
    U8,
    #[display("u16")]
    U16,
    #[display("u32")]
    U32,
    #[display("u64")]
    U64,
    #[display("i8")]
    I8,
    #[display("i16")]
    I16,
    #[display("i32")]
    I32,
    #[display("i64")]
    I64,
    #[display("usize")]
    Usize,
    #[display("isize")]
    Isize,
    #[display("bool")]
    Bool,
    #[display("void")]
    Void,
    #[display("null")]
    Null,
}

impl PartialEq<TokenKind> for TokenKind {
    fn eq(&self, other: &TokenKind) -> bool {
        discriminant(self) == discriminant(other)
    }
}

impl Hash for TokenKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state)
    }
}
