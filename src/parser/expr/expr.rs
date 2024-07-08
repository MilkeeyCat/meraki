use super::{int_repr::UIntLitRepr, IntLitRepr};
use crate::{
    parser::op::{BinOp, UnOp},
    scope::Scope,
    symbol_table::Symbol,
    type_::{Type, TypeError},
};
use std::collections::HashMap;

pub trait Expression {
    fn type_(&self, scope: &Scope) -> Result<Type, TypeError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(String),
    Struct(ExprStruct),
    FunctionCall(ExprFunctionCall),
}

impl Expression for Expr {
    fn type_(&self, symtable: &Scope) -> Result<Type, TypeError> {
        match self {
            Self::Binary(expr) => expr.type_(symtable),
            Self::Unary(expr) => expr.type_(symtable),
            Self::Lit(literal) => literal.type_(symtable),
            Self::Ident(ident) => match symtable
                .find_symbol(ident)
                .ok_or(TypeError::IdentNotFound(ident.to_owned()))?
            {
                Symbol::Global(global_var) => Ok(global_var.type_.clone()),
                Symbol::Local(local) => Ok(local.type_.clone()),
                Symbol::Param(param) => Ok(param.type_.clone()),
                _ => unreachable!(),
            },
            Self::Cast(cast) => cast.type_(symtable),
            Self::Struct(expr_struct) => expr_struct.type_(symtable),
            Self::FunctionCall(function_call) => {
                match symtable.find_symbol(&function_call.name).unwrap() {
                    Symbol::Function(function) => Ok(function.return_type.to_owned()),
                    _ => unreachable!(),
                }
            }
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

impl Expression for ExprBinary {
    fn type_(&self, symtable: &Scope) -> Result<Type, TypeError> {
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
    UInt(UIntLitRepr),
    Bool(bool),
}

impl Expression for ExprLit {
    fn type_(&self, _: &Scope) -> Result<Type, TypeError> {
        match self {
            ExprLit::Int(int) => Ok(int.type_()),
            ExprLit::UInt(uint) => Ok(uint.type_()),
            ExprLit::Bool(_) => Ok(Type::Bool),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct {
    name: String,
    fields: HashMap<String, Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprFunctionCall {
    pub name: String,
    pub arguments: Vec<Expr>,
}

impl ExprFunctionCall {
    pub fn new(name: String, arguments: Vec<Expr>) -> Self {
        Self { name, arguments }
    }
}

impl ExprStruct {
    pub fn new(name: String, fields: HashMap<String, Expr>) -> Self {
        Self { name, fields }
    }
}

impl Expression for ExprStruct {
    fn type_(&self, _: &Scope) -> Result<Type, TypeError> {
        Ok(Type::Struct(self.name.clone()))
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
}

impl Expression for ExprUnary {
    fn type_(&self, symtable: &Scope) -> Result<Type, TypeError> {
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
    pub expr: Box<Expr>,
    pub type_: Type,
}

impl ExprCast {
    pub fn new(type_: Type, expr: Box<Expr>) -> Self {
        Self { type_, expr }
    }
}

impl Expression for ExprCast {
    fn type_(&self, symbtable: &Scope) -> Result<Type, TypeError> {
        let expr_type = self.expr.type_(symbtable)?;

        expr_type.cast(self.type_.clone())
    }
}
