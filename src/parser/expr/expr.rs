use super::{int_repr::UIntLitRepr, ExprError, IntLitRepr};
use crate::{
    parser::op::{BinOp, UnOp},
    passes::TypeChecker,
    scope::Scope,
    symbol_table::{Symbol, SymbolTableError},
    type_table,
    types::{IntType, Type, TypeArray, TypeError, UintType},
};

pub trait Expression {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(ExprIdent),
    Struct(ExprStruct),
    Array(ExprArray),
    StructAccess(ExprStructAccess),
    StructMethod(ExprStructMethod),
    ArrayAccess(ExprArrayAccess),
    FunctionCall(ExprFunctionCall),
}

impl Expression for Expr {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match self {
            Self::Binary(expr) => expr.type_(scope),
            Self::Unary(expr) => expr.type_(scope),
            Self::Lit(literal) => literal.type_(scope),
            Self::Ident(ident) => ident.type_(scope),
            Self::Cast(cast) => cast.type_(scope),
            Self::Struct(expr_struct) => expr_struct.type_(scope),
            Self::Array(expr) => expr.type_(scope),
            Self::StructAccess(expr_struct_access) => expr_struct_access.type_(scope),
            Self::StructMethod(expr) => expr.type_(scope),
            Self::ArrayAccess(expr) => expr.type_(scope),
            Self::FunctionCall(function_call) => {
                match scope
                    .find_symbol(&function_call.name)
                    .ok_or(SymbolTableError::NotFound(function_call.name.clone()))?
                {
                    Symbol::Function(function) => Ok(function.return_type.to_owned()),
                    _ => unreachable!(),
                }
            }
        }
    }
}

impl Expr {
    pub fn lvalue(&self) -> bool {
        match self {
            Self::StructAccess(_)
            | Self::ArrayAccess(_)
            | Self::Ident(_)
            | Self::Unary(ExprUnary {
                op: UnOp::Deref, ..
            }) => true,
            _ => false,
        }
    }

    pub fn int_lit_only(expr: &Expr) -> bool {
        match expr {
            Expr::Binary(expr) => {
                Expr::int_lit_only(expr.left.as_ref()) && Expr::int_lit_only(expr.right.as_ref())
            }
            Expr::Unary(expr) => Expr::int_lit_only(expr.expr.as_ref()),
            Expr::Cast(expr) => {
                if expr.type_.int() {
                    Expr::int_lit_only(expr.expr.as_ref())
                } else {
                    false
                }
            }
            Expr::Lit(expr) => match expr {
                ExprLit::Int(_) | ExprLit::UInt(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Expression for ExprBinary {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match &self.op {
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Assign
            | BinOp::BitwiseAnd
            | BinOp::BitwiseOr
            | BinOp::Shl
            | BinOp::Shr => {
                Ok(TypeChecker::check_bin(&self.op, &self.left, &self.right, scope).unwrap())
            }
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessEqual
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual
            | BinOp::LogicalAnd
            | BinOp::LogicalOr => Ok(Type::Bool),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprLit {
    Int(IntLitRepr),
    UInt(UIntLitRepr),
    Bool(bool),
    String(String),
    Null,
}

impl Expression for ExprLit {
    fn type_(&self, _: &Scope) -> Result<Type, ExprError> {
        match self {
            ExprLit::Int(int) => Ok(int.type_()),
            ExprLit::UInt(uint) => Ok(uint.type_()),
            ExprLit::Bool(_) => Ok(Type::Bool),
            ExprLit::String(_) => Ok(Type::Ptr(Box::new(Type::Int(IntType::I8)))),
            ExprLit::Null => Ok(Type::Null),
        }
    }
}

impl ExprLit {
    pub fn signed(&self) -> bool {
        match self {
            ExprLit::Int(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIdent(pub String);

impl Expression for ExprIdent {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match scope
            .find_symbol(&self.0)
            .ok_or(TypeError::IdentNotFound(self.0.to_owned()))?
        {
            Symbol::Global(global_var) => Ok(global_var.type_.clone()),
            Symbol::Local(local) => Ok(local.type_.clone()),
            Symbol::Param(param) => Ok(param.type_.clone()),
            Symbol::Function(func) => Ok(func.return_type.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprArray(pub Vec<Expr>);

impl Expression for ExprArray {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        Ok(Type::Array(TypeArray {
            type_: Box::new(self.0.get(0).unwrap().type_(scope)?),
            length: self.0.len(),
        }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStructAccess {
    pub expr: Box<Expr>,
    pub field: String,
}

impl Expression for ExprStructAccess {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match self.expr.type_(scope)? {
            Type::Struct(struct_name) => {
                match scope
                    .find_type(&struct_name)
                    .ok_or(TypeError::Nonexistent(struct_name.to_owned()))?
                {
                    type_table::Type::Struct(type_struct) => {
                        Ok(type_struct.get_field_type(&self.field).unwrap().to_owned())
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStructMethod {
    pub expr: Box<Expr>,
    pub method: String,
    pub arguments: Vec<Expr>,
}

impl Expression for ExprStructMethod {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match self.expr.type_(scope)? {
            Type::Struct(struct_name) => {
                match scope
                    .find_type(&struct_name)
                    .ok_or(TypeError::Nonexistent(struct_name.to_owned()))?
                {
                    type_table::Type::Struct(type_struct) => Ok(type_struct
                        .find_method(&self.method)
                        .unwrap()
                        .return_type
                        .to_owned()),
                }
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprFunctionCall {
    pub name: String,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprArrayAccess {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

impl Expression for ExprArrayAccess {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        Ok(self.expr.type_(scope)?.inner()?)
    }
}

impl Expression for ExprStruct {
    fn type_(&self, _: &Scope) -> Result<Type, ExprError> {
        Ok(Type::Struct(self.name.clone()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnOp,
    pub expr: Box<Expr>,
}

impl Expression for ExprUnary {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        Ok(match &self.op {
            UnOp::Negative => match self.expr.type_(scope)? {
                Type::UInt(uint) => Type::Int(uint.to_signed()),
                type_ => type_,
            },
            UnOp::LogicalNot => Type::Bool,
            UnOp::Address => Type::Ptr(Box::new(self.expr.type_(scope)?)),
            UnOp::Deref => self.expr.type_(scope)?.inner()?,
            UnOp::BitwiseNot => self.expr.type_(scope)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprCast {
    pub expr: Box<Expr>,
    pub type_: Type,
}

impl Expression for ExprCast {
    fn type_(&self, _: &Scope) -> Result<Type, ExprError> {
        Ok(self.type_.to_owned())
    }
}
