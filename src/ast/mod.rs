use crate::lexer::{span::Span, Token, TokenKind};
use derive_more::derive::Display;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: Ty,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub open_brace: Span,
    pub stmts: Vec<Stmt>,
    pub close_brace: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Global(Variable),
    Fn {
        ret_ty: Ty,
        name: String,
        params: Vec<(String, Ty)>,
        block: Option<Block>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Ty)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Local(Variable),
    Item(Item),
    Expr(Expr),
    Return(Option<Expr>),
    If {
        condition: Expr,
        consequence: Block,
        alternative: Option<Block>,
    },
    While {
        condition: Expr,
        block: Block,
    },
    For {
        initializer: Option<Box<Stmt>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        block: Block,
    },
    Continue,
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        ty: Ty,
    },
    Lit(ExprLit),
    Ident(String),
    Struct {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Array(Vec<Expr>),
    Field {
        expr: Box<Expr>,
        field: String,
    },
    StructMethod {
        expr: Box<Expr>,
        method: String,
        arguments: Vec<Expr>,
    },
    ArrayAccess {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    FunctionCall {
        expr: Box<Expr>,
        arguments: Vec<Expr>,
    },
    MacroCall {
        name: String,
        tokens: Vec<Token>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(i64),
    UInt(u64),
    Bool(bool),
    String(String),
    Null,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum IntTy {
    #[display("i8")]
    I8,
    #[display("i16")]
    I16,
    #[display("i32")]
    I32,
    #[display("i64")]
    I64,
    #[display("isize")]
    Isize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum UintTy {
    #[display("u8")]
    U8,
    #[display("u16")]
    U16,
    #[display("u32")]
    U32,
    #[display("u64")]
    U64,
    #[display("usize")]
    Usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum Ty {
    #[display("null")]
    Null,
    #[display("void")]
    Void,
    #[display("bool")]
    Bool,
    Int(IntTy),
    UInt(UintTy),
    Ident(String),
    #[display("*{_0}")]
    Ptr(Box<Ty>),
    #[display("{ty}[{len}]")]
    Array {
        ty: Box<Ty>,
        len: usize,
    },
    #[display("fn ({}) -> {_1}",
        _0
            .iter()
            .map(|type_| type_.to_string())
            .collect::<String>()
    )]
    Fn(Vec<Ty>, Box<Ty>),
    #[display("infer")]
    Infer,
}

#[derive(Error, Debug)]
pub enum OpParseError {
    #[error("Failed to parse binary operator from {0}")]
    Bin(TokenKind),
    #[error("Failed to parse unary operator from {0}")]
    Un(TokenKind),
    #[error("Failed to parse comparison operator from binary operator {0:?}")]
    Cmp(BinOp),
    #[error("Failed to parse bitwise operator from binary operator {0:?}")]
    Bitwise(BinOp),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Assign,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    Shl,
    Shr,
}

impl TryFrom<&TokenKind> for BinOp {
    type Error = OpParseError;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Asterisk => Ok(Self::Mul),
            TokenKind::Plus => Ok(Self::Add),
            TokenKind::Minus => Ok(Self::Sub),
            TokenKind::Slash => Ok(Self::Div),
            TokenKind::Equal => Ok(Self::Equal),
            TokenKind::NotEqual => Ok(Self::NotEqual),
            TokenKind::LessThan => Ok(Self::LessThan),
            TokenKind::GreaterThan => Ok(Self::GreaterThan),
            TokenKind::LessEqual => Ok(Self::LessEqual),
            TokenKind::GreaterEqual => Ok(Self::GreaterEqual),
            TokenKind::Assign => Ok(Self::Assign),
            TokenKind::And => Ok(Self::LogicalAnd),
            TokenKind::Or => Ok(Self::LogicalOr),
            TokenKind::Ampersand => Ok(Self::BitwiseAnd),
            TokenKind::Bar => Ok(Self::BitwiseOr),
            TokenKind::Shl => Ok(Self::Shl),
            TokenKind::Shr => Ok(Self::Shr),
            token => Err(OpParseError::Bin(token.to_owned())),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    LogicalNot,
    Negative,
    Address,
    Deref,
    BitwiseNot,
}

impl TryFrom<&TokenKind> for UnOp {
    type Error = OpParseError;

    fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Bang => Ok(Self::LogicalNot),
            TokenKind::Minus => Ok(Self::Negative),
            TokenKind::Ampersand => Ok(Self::Address),
            TokenKind::Asterisk => Ok(Self::Deref),
            TokenKind::Tilde => Ok(Self::BitwiseNot),
            token => Err(OpParseError::Un(token.to_owned())),
        }
    }
}

pub enum CmpOp {
    LessEqual,
    LessThan,
    GreaterEqual,
    GreaterThan,
    Equal,
    NotEqual,
}

impl TryFrom<&BinOp> for CmpOp {
    type Error = OpParseError;

    fn try_from(value: &BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::LessThan => Ok(Self::LessThan),
            BinOp::LessEqual => Ok(Self::LessEqual),
            BinOp::GreaterThan => Ok(Self::GreaterThan),
            BinOp::GreaterEqual => Ok(Self::GreaterEqual),
            BinOp::Equal => Ok(Self::Equal),
            BinOp::NotEqual => Ok(Self::NotEqual),
            _ => Err(OpParseError::Cmp(value.to_owned())),
        }
    }
}

pub enum BitwiseOp {
    And,
    Or,
}

impl TryFrom<&BinOp> for BitwiseOp {
    type Error = OpParseError;

    fn try_from(value: &BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::BitwiseAnd => Ok(Self::And),
            BinOp::BitwiseOr => Ok(Self::Or),
            _ => Err(OpParseError::Bitwise(value.to_owned())),
        }
    }
}
