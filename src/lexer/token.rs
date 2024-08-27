use crate::scope::Scope;
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Token {
    Eof,

    Ident(String),
    Integer(String),
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
    LessEqual,
    GreaterEqual,
    And,
    Or,
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
    False,
    Enum,
    Struct,
    If,
    Else,
    Return,

    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Usize,
    Isize,
    Bool,
    Void,
}

impl Token {
    pub fn is_type(&self, scope: &Scope) -> bool {
        match &self {
            Token::U8
            | Token::U16
            | Token::U32
            | Token::U64
            | Token::I8
            | Token::I16
            | Token::I32
            | Token::I64
            | Token::Usize
            | Token::Isize
            | Token::Bool
            | Token::Void => true,
            Token::Ident(ident) => scope.find_type(ident).is_some(),
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;

        match self {
            Eof => write!(f, "EOF"),
            Ident(ident) => write!(f, "ident({})", ident),
            Integer(integer) => write!(f, "int({})", integer),
            String(string) => write!(f, "string({})", string),
            Assign => write!(f, "="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Bang => write!(f, "!"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Arrow => write!(f, "->"),
            Period => write!(f, "."),
            Ampersand => write!(f, "&"),
            Equal => write!(f, "=="),
            NotEqual => write!(f, "!="),
            LessThan => write!(f, "<"),
            GreaterThan => write!(f, ">"),
            LessEqual => write!(f, "<="),
            GreaterEqual => write!(f, "=>"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Colon => write!(f, ":"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Const => write!(f, "const"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Enum => write!(f, "enum"),
            Struct => write!(f, "struct"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Return => write!(f, "return"),
            U8 => write!(f, "u8"),
            U16 => write!(f, "u16"),
            U32 => write!(f, "u32"),
            U64 => write!(f, "u64"),
            I8 => write!(f, "i8"),
            I16 => write!(f, "i16"),
            I32 => write!(f, "i32"),
            I64 => write!(f, "i64"),
            Usize => write!(f, "usize"),
            Isize => write!(f, "isize"),
            Bool => write!(f, "bool"),
            Void => write!(f, "void"),
        }
    }
}
