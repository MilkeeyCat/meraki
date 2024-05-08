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
    Enum,
    Struct,
    False,
    If,
    Else,
    Return,

    I8,
    U8,
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
