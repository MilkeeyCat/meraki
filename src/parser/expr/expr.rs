use super::{int_repr::UIntLitRepr, ExprError, IntLitRepr};
use crate::{
    archs::ArchError,
    codegen::{
        locations::{self, MoveDestination, Offset},
        CodeGen,
    },
    parser::op::{BinOp, UnOp},
    scope::Scope,
    symbol_table::{Symbol, SymbolTableError},
    type_table,
    types::{Type, TypeError},
};

pub trait Expression {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError>;
}

pub trait LValue {
    fn dest(&self, codegen: &mut CodeGen) -> Result<MoveDestination, ArchError>;
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
            Self::StructAccess(expr_struct_access) => expr_struct_access.type_(scope),
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
            Self::StructAccess(_) | Self::ArrayAccess(_) | Self::Ident(_) => true,
            _ => false,
        }
    }

    pub fn dest(&self, codegen: &mut CodeGen) -> Result<Option<MoveDestination>, ArchError> {
        match self {
            Expr::Ident(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::StructAccess(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::Unary(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::ArrayAccess(expr) => expr.dest(codegen).map(|d| Some(d)),
            _ => Ok(None),
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
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match &self.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let left = self.left.as_ref().type_(scope)?;
                let right = self.right.as_ref().type_(scope)?;

                Ok(Type::promote(left, right)?)
            }
            BinOp::Assign => Ok(self.left.type_(scope)?.assign(self.right.type_(scope)?)?),
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
    String(String),
}

impl Expression for ExprLit {
    fn type_(&self, _: &Scope) -> Result<Type, ExprError> {
        match self {
            ExprLit::Int(int) => Ok(int.type_()),
            ExprLit::UInt(uint) => Ok(uint.type_()),
            ExprLit::Bool(_) => Ok(Type::Bool),
            ExprLit::String(_) => Ok(Type::Ptr(Box::new(Type::U8))),
        }
    }
}

impl ExprLit {
    pub fn signed(&self) -> bool {
        match self {
            ExprLit::Int(_) => true,
            ExprLit::UInt(_) | ExprLit::Bool(_) | ExprLit::String(_) => false,
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
                    write!(f, "0")
                }
            }
            Self::String(literal) => write!(f, "{literal}"),
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

impl LValue for ExprIdent {
    fn dest(&self, codegen: &mut CodeGen) -> Result<MoveDestination, ArchError> {
        Ok(codegen
            .scope
            .find_symbol(&self.0)
            .ok_or(SymbolTableError::NotFound(self.0.clone()))?
            .to_dest(&codegen.arch, &codegen.scope)?)
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
    pub expr: Box<Expr>,
    pub field: String,
}

impl Expression for ExprStructAccess {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match self.expr.as_ref() {
            Expr::Ident(expr) => match expr.type_(&scope)? {
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
                _ => todo!(),
            },
            Expr::StructAccess(expr) => match expr.type_(&scope)? {
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
                _ => todo!(),
            },
            Expr::Unary(expr) => match expr.type_(&scope)? {
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
                _ => todo!(),
            },

            expr => panic!("Expression {expr:?} can't be used to access struct field value"),
        }
    }
}

impl LValue for ExprStructAccess {
    fn dest(&self, codegen: &mut CodeGen) -> Result<MoveDestination, ArchError> {
        let mut dest = match self.expr.as_ref() {
            Expr::Ident(expr) => expr.dest(codegen)?,
            Expr::StructAccess(expr) => expr.dest(codegen)?,
            Expr::Unary(expr) => expr.dest(codegen)?,
            _ => unreachable!(),
        };
        let (field_offset, field_size) = match self.expr.type_(&codegen.scope)? {
            Type::Struct(s) => match codegen
                .scope
                .find_type(&s)
                .ok_or(TypeError::Nonexistent(s))?
            {
                type_table::Type::Struct(type_struct) => (
                    type_struct.offset(&codegen.arch, &self.field, &codegen.scope)?,
                    type_struct
                        .get_field_type(&self.field)
                        .unwrap()
                        .size(&codegen.arch, &codegen.scope)?,
                ),
            },
            type_ => panic!("{type_:?}"),
        };

        let new_offset = dest.offset().unwrap_or(&Offset(0)) + &field_offset;

        match &mut dest {
            MoveDestination::Local(local) => {
                local.offset = new_offset;
                local.size = field_size;
            }
            MoveDestination::Global(global) => {
                global.size = field_size;
                match &mut global.offset {
                    Some(offset) => *offset = &*offset + &new_offset,
                    None => global.offset = Some(new_offset),
                }
            }
            MoveDestination::Register(register) => {
                register.size = field_size;
                match &mut register.offset {
                    Some(offset) => *offset = &*offset + &new_offset,
                    None => register.offset = Some(new_offset),
                }
            }
        };

        Ok(dest)
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

impl LValue for ExprArrayAccess {
    fn dest(&self, codegen: &mut CodeGen) -> Result<MoveDestination, ArchError> {
        let pointed_type_size = self
            .expr
            .type_(&codegen.scope)?
            .inner()?
            .size(&codegen.arch, &codegen.scope)?;

        let base = self.expr.dest(codegen)?.expect("Everything went tits up");
        let r = codegen.arch.alloc()?;
        let r2 = codegen.arch.alloc()?;
        let index = r.to_dest(codegen.arch.word_size());
        let mut dest = r2.to_dest(codegen.arch.word_size());

        codegen
            .expr(*self.index.clone(), Some(index.clone()))
            .unwrap();
        codegen
            .arch
            .array_offset(&dest, &base, &index, pointed_type_size);
        dest.set_size(pointed_type_size);
        dest.set_offset(Offset::default());

        Ok(dest)
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

impl LValue for ExprUnary {
    fn dest(&self, codegen: &mut CodeGen) -> Result<MoveDestination, ArchError> {
        let expr_dest = match self.expr.as_ref() {
            Expr::Ident(expr) => expr.dest(codegen)?,
            Expr::StructAccess(expr) => expr.dest(codegen)?,
            _ => panic!(),
        };
        let r = codegen.arch.alloc().unwrap();

        codegen
            .arch
            .mov(
                expr_dest.to_source(self.type_(&codegen.scope)?.signed()),
                MoveDestination::Register(locations::Register {
                    register: r,
                    offset: None,
                    size: 8,
                }),
                &codegen.scope,
            )
            .unwrap();

        Ok(MoveDestination::Register(locations::Register {
            register: r,
            offset: Some(Offset(0)),
            size: self
                .expr
                .type_(&codegen.scope)?
                .inner()?
                .size(&codegen.arch, &codegen.scope)?,
        }))
    }
}

impl ExprUnary {
    pub fn new(op: UnOp, expr: Box<Expr>) -> Self {
        Self { op, expr }
    }
}

impl Expression for ExprUnary {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        Ok(match &self.op {
            UnOp::Negative => {
                let mut expr_type = self.expr.type_(scope)?;
                expr_type.to_signed();

                expr_type
            }
            UnOp::Not => Type::Bool,
            UnOp::Address => Type::Ptr(Box::new(self.expr.type_(scope)?)),
            UnOp::Deref => self.expr.type_(scope)?.inner()?,
        })
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
    fn type_(&self, symbtable: &Scope) -> Result<Type, ExprError> {
        let expr_type = self.expr.type_(symbtable)?;

        Ok(expr_type.cast(self.type_.clone())?)
    }
}
