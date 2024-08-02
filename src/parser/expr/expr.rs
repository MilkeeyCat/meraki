use super::{int_repr::UIntLitRepr, IntLitRepr};
use crate::{
    archs::Architecture,
    codegen::locations::MoveDestination,
    parser::op::{BinOp, UnOp},
    scope::Scope,
    symbol_table::Symbol,
    type_::{Type, TypeError},
    type_table,
};

pub trait Expression {
    fn type_(&self, scope: &Scope) -> Result<Type, TypeError>;
}

pub trait LValue {
    fn dest<'a>(&self, arch: &dyn Architecture, scope: &'a Scope) -> MoveDestination<'a>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary(ExprBinary),
    Unary(ExprUnary),
    Cast(ExprCast),
    Lit(ExprLit),
    Ident(ExprIdent),
    Struct(ExprStruct),
    StructAccess(ExprStructAccess),
    FunctionCall(ExprFunctionCall),
}

impl Expression for Expr {
    fn type_(&self, scope: &Scope) -> Result<Type, TypeError> {
        match self {
            Self::Binary(expr) => expr.type_(scope),
            Self::Unary(expr) => expr.type_(scope),
            Self::Lit(literal) => literal.type_(scope),
            Self::Ident(ident) => ident.type_(scope),
            Self::Cast(cast) => cast.type_(scope),
            Self::Struct(expr_struct) => expr_struct.type_(scope),
            Self::StructAccess(expr_struct_access) => expr_struct_access.type_(scope),
            Self::FunctionCall(function_call) => {
                match scope.find_symbol(&function_call.name).unwrap() {
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

impl std::fmt::Display for ExprLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{}", int.to_string()),
            Self::UInt(uint) => write!(f, "{}", uint.to_string()),
            Self::Bool(boolean) => {
                if boolean == &true {
                    write!(f, "1")
                } else {
                    write!(f, "1")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIdent(pub String);

impl Expression for ExprIdent {
    fn type_(&self, scope: &Scope) -> Result<Type, TypeError> {
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

impl LValue for ExprIdent {
    fn dest<'a>(&self, arch: &dyn Architecture, scope: &'a Scope) -> MoveDestination<'a> {
        scope.find_symbol(&self.0).unwrap().to_dest(arch, scope)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStruct {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

impl ExprStruct {
    pub fn new(name: String, fields: Vec<(String, Expr)>) -> Self {
        Self { name, fields }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStructAccess {
    pub name: String,
    pub field: String,
}

impl Expression for ExprStructAccess {
    fn type_(&self, scope: &Scope) -> Result<Type, TypeError> {
        match scope.find_symbol(&self.name).unwrap() {
            Symbol::Local(local) => match &local.type_ {
                Type::Struct(s) => match scope.find_type(&s).unwrap() {
                    type_table::Type::Struct(sl) => {
                        Ok(sl.get_field_type(&self.field).unwrap().to_owned())
                    }
                },
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl LValue for ExprStructAccess {
    fn dest<'a>(&self, arch: &dyn Architecture, scope: &'a Scope) -> MoveDestination<'a> {
        let symbol = scope.find_symbol(&self.name).unwrap();
        let (field_offset, struct_size) = match symbol.type_() {
            Type::Struct(s) => match scope.find_type(&s).unwrap() {
                type_table::Type::Struct(type_struct) => (
                    type_struct.offset(arch, &self.field, scope),
                    type_struct.size(arch, scope),
                ),
            },
            _ => panic!(),
        };

        let mut dest = symbol.to_dest(arch, scope);
        // NOTE: local variable use 1-based offset but struct offsets are 0-based, so to plumb it correctly gotta slap that -1
        let new_offset = struct_size + dest.local_offset() - field_offset - 1;

        match &mut dest {
            MoveDestination::Local(local) => {
                local.offset = new_offset;
            }
            MoveDestination::Global(global) => match &mut global.offset {
                Some(offset) => *offset += new_offset,
                None => global.offset = Some(new_offset),
            },
            _ => unreachable!(),
        };

        dest
    }
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
