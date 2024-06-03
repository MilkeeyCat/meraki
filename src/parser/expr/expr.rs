use std::fmt::Display;

use super::IntLitRepr;
use crate::{
    lexer::Token,
    parser::{type_::TypeError, Type},
    symtable::{Symbol, SymbolTable},
};

#[derive(Debug)]
pub enum OpParseError {
    Bin(Token),
    Un(Token),
}

impl Display for OpParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Un(token) => {
                write!(f, "Failed to parse unary operator from {}", token)
            }
            Self::Bin(token) => {
                write!(f, "Failed to parse binary operator from {}", token)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
}

impl TryFrom<&Token> for BinOp {
    type Error = OpParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Asterisk => Ok(Self::Mul),
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Slash => Ok(Self::Div),
            Token::Equal => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            Token::LessThan => Ok(Self::LessThan),
            Token::GreaterThan => Ok(Self::GreaterThan),
            Token::LessEqual => Ok(Self::LessEqual),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::Assign => Ok(Self::Assign),
            token => Err(OpParseError::Bin(token.to_owned())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(String),
}

impl Expr {
    pub fn type_(&self, symtable: &SymbolTable) -> Result<Type, TypeError> {
        match self {
            Self::Binary(expr) => {
                let left_type = expr.left.as_ref().type_(symtable)?;
                let right_type = expr.right.as_ref().type_(symtable)?;

                Type::promote(left_type, right_type)
            }
            Self::Unary(expr) => expr.type_(symtable),
            Self::Lit(literal) => match literal {
                ExprLit::Int(int) => Ok(int.type_()),
            },
            Self::Ident(ident) => match symtable
                .find(ident)
                .ok_or(TypeError::IdentNotFound(ident.to_owned()))?
            {
                Symbol::GlobalVar(global_var) => Ok(global_var.type_.clone()),
            },
            Self::Cast(cast) => cast.type_(symtable),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl ExprBinary {
    pub fn new(op: BinOp, left: Box<Expr>, right: Box<Expr>) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(IntLitRepr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Negative,
}

impl TryFrom<&Token> for UnOp {
    type Error = OpParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Bang => Ok(Self::Not),
            Token::Minus => Ok(Self::Negative),
            token => Err(OpParseError::Un(token.to_owned())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

impl ExprUnary {
    pub fn new(op: UnOp, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }

    pub fn type_(&self, symtable: &SymbolTable) -> Result<Type, TypeError> {
        let mut expr_type = self.expr.type_(symtable)?;

        if let Expr::Lit(ExprLit::Int(int)) = self.expr.as_ref() {
            if self.op == UnOp::Negative {
                if int.first_bit_set() {
                    expr_type = int.widen_type().unwrap();
                } else {
                    expr_type.to_signed();
                }
            }
        }

        Ok(expr_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCast {
    expr: Box<Expr>,
    type_: Type,
}

impl ExprCast {
    pub fn new(type_: Type, expr: Box<Expr>) -> Self {
        Self { type_, expr }
    }

    pub fn type_(&self, symbtable: &SymbolTable) -> Result<Type, TypeError> {
        let expr_type = self.expr.type_(symbtable)?;

        expr_type.cast(self.type_.clone())
    }
}
