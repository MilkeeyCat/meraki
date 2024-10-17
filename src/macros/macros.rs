use crate::lexer;
use libloading::Symbol;
use std::ffi::{c_char, CStr, CString};

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

impl Into<Token> for lexer::Token {
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
        }
    }
}

impl Into<lexer::Token> for Token {
    fn into(self) -> lexer::Token {
        match self {
            Self::Ident(ptr) => {
                let ident = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::Token::Ident(ident)
            }
            Self::String(ptr) => {
                let string = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::Token::String(string)
            }
            Self::Integer(ptr) => {
                let integer = unsafe { CString::from_raw(ptr) }.into_string().unwrap();

                lexer::Token::Integer(integer)
            }

            Self::Assign => lexer::Token::Assign,
            Self::Plus => lexer::Token::Plus,
            Self::Minus => lexer::Token::Minus,
            Self::Bang => lexer::Token::Bang,
            Self::Asterisk => lexer::Token::Asterisk,
            Self::Slash => lexer::Token::Slash,
            Self::Arrow => lexer::Token::Arrow,
            Self::Period => lexer::Token::Period,
            Self::Tilde => lexer::Token::Tilde,
            Self::Ampersand => lexer::Token::Ampersand,
            Self::Bar => lexer::Token::Bar,
            Self::Equal => lexer::Token::Equal,
            Self::NotEqual => lexer::Token::NotEqual,
            Self::LessThan => lexer::Token::LessThan,
            Self::GreaterThan => lexer::Token::GreaterThan,
            Self::LessEqual => lexer::Token::LessEqual,
            Self::GreaterEqual => lexer::Token::GreaterEqual,
            Self::And => lexer::Token::And,
            Self::Or => lexer::Token::Or,
            Self::Shl => lexer::Token::Shl,
            Self::Shr => lexer::Token::Shr,
            Self::Comma => lexer::Token::Comma,
            Self::Semicolon => lexer::Token::Semicolon,
            Self::Colon => lexer::Token::Colon,
            Self::LParen => lexer::Token::LParen,
            Self::RParen => lexer::Token::RParen,
            Self::LBrace => lexer::Token::LBrace,
            Self::RBrace => lexer::Token::RBrace,
            Self::LBracket => lexer::Token::LBracket,
            Self::RBracket => lexer::Token::RBracket,

            Self::Const => lexer::Token::Const,
            Self::True => lexer::Token::True,
            Self::False => lexer::Token::False,
            Self::Let => lexer::Token::Let,
            Self::Fn => lexer::Token::Fn,
            Self::Enum => lexer::Token::Enum,
            Self::Struct => lexer::Token::Struct,
            Self::If => lexer::Token::If,
            Self::While => lexer::Token::While,
            Self::For => lexer::Token::For,
            Self::Else => lexer::Token::Else,
            Self::Return => lexer::Token::Return,
            Self::As => lexer::Token::As,
            Self::Continue => lexer::Token::Continue,
            Self::Break => lexer::Token::Break,

            Self::U8 => lexer::Token::U8,
            Self::U16 => lexer::Token::U16,
            Self::U32 => lexer::Token::U32,
            Self::U64 => lexer::Token::U64,
            Self::I8 => lexer::Token::I8,
            Self::I16 => lexer::Token::I16,
            Self::I32 => lexer::Token::I32,
            Self::I64 => lexer::Token::I64,
            Self::Usize => lexer::Token::Usize,
            Self::Isize => lexer::Token::Isize,
            Self::Bool => lexer::Token::Bool,
            Self::Void => lexer::Token::Void,

            Self::Null => lexer::Token::Null,
        }
    }
}
