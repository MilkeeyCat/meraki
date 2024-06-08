use super::IntLitRepr;
use crate::{
    parser::{
        op::{BinOp, UnOp},
        type_::TypeError,
        Type,
    },
    symtable::{Symbol, SymbolTable},
};

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
            Self::Binary(expr) => expr.type_(symtable),
            Self::Unary(expr) => expr.type_(symtable),
            Self::Lit(literal) => literal.type_(),
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

                if let Expr::Lit(ExprLit::Int(int)) = self.expr.as_ref() {
                    if int.first_bit_set() {
                        expr_type = int.widen_type().unwrap();
                    }
                }

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
