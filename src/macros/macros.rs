use crate::lexer;
use libloading::Symbol;
use std::ffi::{CStr, CString, c_char};

#[repr(C)]
#[derive(Debug)]
pub enum Token {
    Ident(*mut c_char),
    Integer(*mut c_char),
    String(*mut c_char),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Arrow,
    Period,
    Tilde,
    Ampersand,
    Bar,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Shl,
    Shr,
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
    Let,
    Fn,
    Enum,
    Struct,
    If,
    While,
    For,
    Else,
    Return,
    As,
    Continue,
    Break,

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

    Null,
}

#[repr(C)]
#[derive(Debug)]
pub struct Slice<T> {
    pub ptr: *const T,
    pub len: usize,
}

pub type MacroFn = extern "C" fn(Slice<Token>) -> Slice<Token>;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Macro {
    name: *const c_char,
    func: MacroFn,
}

impl Macro {
    pub fn name(&self) -> &str {
        unsafe {
            CStr::from_ptr(self.name)
                .to_str()
                .expect("Failed to convert CStr to str")
        }
    }

    pub fn func(&self) -> &MacroFn {
        &self.func
    }
}

pub fn symbol_to_macros<'lib>(symbol: Symbol<'lib, *const Slice<Macro>>) -> &'lib [Macro] {
    unsafe {
        let slice = symbol.read();
        std::slice::from_raw_parts(slice.ptr, slice.len)
    }
}

impl Into<Token> for lexer::TokenKind {
    fn into(self) -> Token {
        match self {
            Self::Ident(ident) => {
                let ident = Box::leak(Box::new(CString::new(ident).unwrap()));

                Token::Ident(ident.as_ptr() as *mut c_char)
            }
            Self::String(string) => {
                let string = Box::leak(Box::new(CString::new(string).unwrap()));

                Token::String(string.as_ptr() as *mut c_char)
            }
            Self::Integer(integer) => {
                let integer = Box::leak(Box::new(CString::new(integer).unwrap()));

                Token::Integer(integer.as_ptr() as *mut c_char)
            }

            Self::Assign => Token::Assign,
            Self::Plus => Token::Plus,
            Self::Minus => Token::Minus,
            Self::Bang => Token::Bang,
            Self::Asterisk => Token::Asterisk,
            Self::Slash => Token::Slash,
            Self::Arrow => Token::Arrow,
            Self::Period => Token::Period,
            Self::Tilde => Token::Tilde,
            Self::Ampersand => Token::Ampersand,
            Self::Bar => Token::Bar,
            Self::Equal => Token::Equal,
            Self::NotEqual => Token::NotEqual,
            Self::LessThan => Token::LessThan,
            Self::GreaterThan => Token::GreaterThan,
            Self::LessEqual => Token::LessEqual,
            Self::GreaterEqual => Token::GreaterEqual,
            Self::And => Token::And,
            Self::Or => Token::Or,
            Self::Shl => Token::Shl,
            Self::Shr => Token::Shr,
            Self::Comma => Token::Comma,
            Self::Semicolon => Token::Semicolon,
            Self::Colon => Token::Colon,
            Self::LParen => Token::LParen,
            Self::RParen => Token::RParen,
            Self::LBrace => Token::LBrace,
            Self::RBrace => Token::RBrace,
            Self::LBracket => Token::LBracket,
            Self::RBracket => Token::RBracket,

            Self::Const => Token::Const,
            Self::True => Token::True,
            Self::False => Token::False,
            Self::Let => Token::Let,
            Self::Fn => Token::Fn,
            Self::Enum => Token::Enum,
            Self::Struct => Token::Struct,
            Self::If => Token::If,
            Self::While => Token::While,
            Self::For => Token::For,
            Self::Else => Token::Else,
            Self::Return => Token::Return,
            Self::As => Token::As,
            Self::Continue => Token::Continue,
            Self::Break => Token::Break,

            Self::U8 => Token::U8,
            Self::U16 => Token::U16,
            Self::U32 => Token::U32,
            Self::U64 => Token::U64,
            Self::I8 => Token::I8,
            Self::I16 => Token::I16,
            Self::I32 => Token::I32,
            Self::I64 => Token::I64,
            Self::Usize => Token::Usize,
            Self::Isize => Token::Isize,
            Self::Bool => Token::Bool,
            Self::Void => Token::Void,

            Self::Null => Token::Null,
            _ => todo!(),
        }
    }
}

impl Into<lexer::TokenKind> for Token {
    fn into(self) -> lexer::TokenKind {
        match self {
            Self::Ident(ptr) => {
                let ident = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::TokenKind::Ident(ident)
            }
            Self::String(ptr) => {
                let string = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::TokenKind::String(string)
            }
            Self::Integer(ptr) => {
                let integer = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::TokenKind::Integer(integer)
            }

            Self::Assign => lexer::TokenKind::Assign,
            Self::Plus => lexer::TokenKind::Plus,
            Self::Minus => lexer::TokenKind::Minus,
            Self::Bang => lexer::TokenKind::Bang,
            Self::Asterisk => lexer::TokenKind::Asterisk,
            Self::Slash => lexer::TokenKind::Slash,
            Self::Arrow => lexer::TokenKind::Arrow,
            Self::Period => lexer::TokenKind::Period,
            Self::Tilde => lexer::TokenKind::Tilde,
            Self::Ampersand => lexer::TokenKind::Ampersand,
            Self::Bar => lexer::TokenKind::Bar,
            Self::Equal => lexer::TokenKind::Equal,
            Self::NotEqual => lexer::TokenKind::NotEqual,
            Self::LessThan => lexer::TokenKind::LessThan,
            Self::GreaterThan => lexer::TokenKind::GreaterThan,
            Self::LessEqual => lexer::TokenKind::LessEqual,
            Self::GreaterEqual => lexer::TokenKind::GreaterEqual,
            Self::And => lexer::TokenKind::And,
            Self::Or => lexer::TokenKind::Or,
            Self::Shl => lexer::TokenKind::Shl,
            Self::Shr => lexer::TokenKind::Shr,
            Self::Comma => lexer::TokenKind::Comma,
            Self::Semicolon => lexer::TokenKind::Semicolon,
            Self::Colon => lexer::TokenKind::Colon,
            Self::LParen => lexer::TokenKind::LParen,
            Self::RParen => lexer::TokenKind::RParen,
            Self::LBrace => lexer::TokenKind::LBrace,
            Self::RBrace => lexer::TokenKind::RBrace,
            Self::LBracket => lexer::TokenKind::LBracket,
            Self::RBracket => lexer::TokenKind::RBracket,

            Self::Const => lexer::TokenKind::Const,
            Self::True => lexer::TokenKind::True,
            Self::False => lexer::TokenKind::False,
            Self::Let => lexer::TokenKind::Let,
            Self::Fn => lexer::TokenKind::Fn,
            Self::Enum => lexer::TokenKind::Enum,
            Self::Struct => lexer::TokenKind::Struct,
            Self::If => lexer::TokenKind::If,
            Self::While => lexer::TokenKind::While,
            Self::For => lexer::TokenKind::For,
            Self::Else => lexer::TokenKind::Else,
            Self::Return => lexer::TokenKind::Return,
            Self::As => lexer::TokenKind::As,
            Self::Continue => lexer::TokenKind::Continue,
            Self::Break => lexer::TokenKind::Break,

            Self::U8 => lexer::TokenKind::U8,
            Self::U16 => lexer::TokenKind::U16,
            Self::U32 => lexer::TokenKind::U32,
            Self::U64 => lexer::TokenKind::U64,
            Self::I8 => lexer::TokenKind::I8,
            Self::I16 => lexer::TokenKind::I16,
            Self::I32 => lexer::TokenKind::I32,
            Self::I64 => lexer::TokenKind::I64,
            Self::Usize => lexer::TokenKind::Usize,
            Self::Isize => lexer::TokenKind::Isize,
            Self::Bool => lexer::TokenKind::Bool,
            Self::Void => lexer::TokenKind::Void,

            Self::Null => lexer::TokenKind::Null,
        }
    }
}
