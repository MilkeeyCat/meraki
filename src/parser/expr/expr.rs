use std::collections::HashMap;

use super::IntLitRepr;
use crate::{
    parser::op::{BinOp, UnOp},
    symtable::{Symbol, SymbolTable},
    type_::{Type, TypeError},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(String),
    Struct(ExprStruct),
}

impl Expr {
    pub fn type_(&self, symtable: &SymbolTable) -> Result<Type, TypeError> {
        match self {
            Self::Binary(expr) => expr.type_(symtable),
            Self::Unary(expr) => expr.type_(symtable),
            Self::Lit(literal) => literal.type_(),
            Self::Ident(ident) => match symtable
                .find(ident)
                .ok_or(TypeError::IdentNotFound(ident.to_owned()))?
            {
                Symbol::GlobalVar(global_var) => Ok(global_var.type_.clone()),
                Symbol::LocalVar(local) => Ok(local.type_.clone()),
            },
            Self::Cast(cast) => cast.type_(symtable),
            Self::Struct(expr_struct) => Ok(expr_struct.type_()),
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

    pub fn type_(&self, symtable: &SymbolTable) -> Result<Type, TypeError> {
        match &self.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let left_type = self.left.as_ref().type_(symtable)?;
                let right_type = self.right.as_ref().type_(symtable)?;

                Type::promote(left_type, right_type)
            }
            BinOp::Assign => self
                .left
                .type_(symtable)?
                .assign(self.right.type_(symtable)?),
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessEqual
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => Ok(Type::Bool),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(IntLitRepr),
    Bool(bool),
}

impl ExprLit {
    pub fn type_(&self) -> Result<Type, TypeError> {
        match self {
            ExprLit::Int(int) => Ok(int.type_()),
            ExprLit::Bool(_) => Ok(Type::Bool),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct {
    name: String,
    fields: HashMap<String, Expr>,
}

impl ExprStruct {
    pub fn new(name: String, fields: HashMap<String, Expr>) -> Self {
        Self { name, fields }
    }

    pub fn type_(&self) -> Type {
        Type::Struct(self.name.clone())
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
        match &self.op {
            UnOp::Negative => {
                let mut expr_type = self.expr.type_(symtable)?;
                expr_type.to_signed();

                Ok(expr_type)
            }
            UnOp::Not => Ok(Type::Bool),
        }
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

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}
