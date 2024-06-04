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

    I8,
    U8,
    Bool,
}

impl Token {
    pub fn precedence(&self) -> u8 {
        match self {
            Token::Bang => 3,
            Token::Asterisk | Token::Slash => 3,
            Token::Plus | Token::Minus => 2,
            Token::Assign => 1,
            _ => 0,
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
            I8 => write!(f, "i8"),
            U8 => write!(f, "u8"),
            Bool => write!(f, "bool"),
        }
    }
}
