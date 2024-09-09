use super::{int_repr::UIntLitRepr, ExprError, IntLitRepr};
use crate::{
    archs::ArchError,
    codegen::{operands, CodeGen, Destination},
    parser::op::{BinOp, UnOp},
    scope::Scope,
    symbol_table::{Symbol, SymbolTableError},
    type_table,
    types::{Type, TypeArray, TypeError},
};
use operands::{Base, EffectiveAddress, Memory};

pub trait Expression {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError>;
}

pub trait LValue {
    fn dest(&self, codegen: &mut CodeGen) -> Result<Destination, ArchError>;
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
            Self::StructAccess(_) | Self::ArrayAccess(_) | Self::Ident(_) => true,
            _ => false,
        }
    }

    pub fn dest(&self, codegen: &mut CodeGen) -> Result<Option<Destination>, ArchError> {
        match self {
            Expr::Ident(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::StructAccess(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::Unary(expr) => expr.dest(codegen).map(|d| Some(d)),
            Expr::ArrayAccess(expr) => expr.dest(codegen).map(|d| Some(d)),
            _ => Ok(None),
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

impl ExprBinary {
    pub fn new(op: BinOp, left: Expr, right: Expr, scope: &Scope) -> Result<Self, ExprError> {
        let modify = |expr: Expr, prev: Type, new: Type| -> Expr {
            if prev != new && Expr::int_lit_only(&expr) {
                Expr::Cast(ExprCast {
                    expr: Box::new(expr),
                    type_: new,
                })
            } else {
                expr
            }
        };
        let promote = |mut from: Type, mut to: Type, signed: bool| -> Type {
            if from > to {
                if signed {
                    from.to_signed();
                }

                from
            } else {
                if signed {
                    to.to_signed();
                }

                to
            }
        };

        match &op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let left_type = left.type_(scope)?;
                let right_type = right.type_(scope)?;

                if left_type.int() && right_type.int() {
                    if left_type != right_type {
                        let signed = left_type.signed() || right_type.signed();
                        let new_left_type = promote(left_type.clone(), right_type.clone(), signed);
                        let new_right_type = promote(right_type.clone(), left_type.clone(), signed);
                        let left = modify(left, left_type, new_left_type);
                        let right = modify(right, right_type, new_right_type);

                        if left.type_(scope)? != right.type_(scope)? {
                            return Err(ExprError::Type(TypeError::Mismatched(
                                left.type_(scope)?,
                                right.type_(scope)?,
                            )));
                        }

                        return Ok(Self {
                            op,
                            left: Box::new(left),
                            right: Box::new(right),
                        });
                    }
                } else {
                    panic!("Math operations are allowed only on integers");
                }
            }
            BinOp::Assign => {
                let left_type = left.type_(scope)?;
                let right_type = right.type_(scope)?;

                if left_type != right_type {
                    let signed = left_type.signed();
                    let new_right_type = promote(right_type.clone(), left_type.clone(), signed);
                    let right = modify(right, right_type, new_right_type);

                    if left.type_(scope)? != right.type_(scope)? {
                        return Err(ExprError::Type(TypeError::Mismatched(
                            left.type_(scope)?,
                            right.type_(scope)?,
                        )));
                    }

                    return Ok(Self {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    });
                }
            }
            _ => {}
        };

        Ok(Self {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl Expression for ExprBinary {
    fn type_(&self, scope: &Scope) -> Result<Type, ExprError> {
        match &self.op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Assign => {
                let left = self.left.as_ref().type_(scope)?;
                let right = self.right.as_ref().type_(scope)?;

                assert_eq!(left, right);

                Ok(left)
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
}

impl Expression for ExprLit {
    fn type_(&self, _: &Scope) -> Result<Type, ExprError> {
        match self {
            ExprLit::Int(int) => Ok(int.type_()),
            ExprLit::UInt(uint) => Ok(uint.type_()),
            ExprLit::Bool(_) => Ok(Type::Bool),
            ExprLit::String(_) => Ok(Type::Ptr(Box::new(Type::I8))),
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
    fn dest(&self, codegen: &mut CodeGen) -> Result<Destination, ArchError> {
        Ok(codegen
            .scope
            .find_symbol(&self.0)
            .ok_or(SymbolTableError::NotFound(self.0.clone()))?
            .dest(&codegen.arch, &codegen.scope)?)
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

impl LValue for ExprStructAccess {
    fn dest(&self, codegen: &mut CodeGen) -> Result<Destination, ArchError> {
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

        Ok(match self.expr.dest(codegen)?.expect("Uh oh") {
            Destination::Memory(mut memory) => {
                memory.effective_address.displacement =
                    if let Some(displacement) = memory.effective_address.displacement {
                        Some(&displacement + &field_offset)
                    } else {
                        Some(field_offset)
                    };
                memory.size = field_size;

                Destination::Memory(memory)
            }
            Destination::Register(register) => Destination::Memory(Memory {
                effective_address: EffectiveAddress {
                    base: Base::Register(register.register),
                    index: None,
                    scale: None,
                    displacement: Some(field_offset),
                },
                size: field_size,
            }),
        })
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

impl LValue for ExprArrayAccess {
    fn dest(&self, codegen: &mut CodeGen) -> Result<Destination, ArchError> {
        let base = self.expr.dest(codegen)?.unwrap();
        let index = codegen.arch.alloc()?;
        let r = codegen.arch.alloc()?;
        let r_loc = r.dest(codegen.arch.word_size());

        codegen
            .expr(
                *self.index.clone(),
                Some(index.dest(codegen.arch.word_size())),
                None,
            )
            .unwrap();
        codegen.arch.lea(
            &Destination::Register(operands::Register {
                register: r,
                size: codegen.arch.word_size(),
            }),
            &EffectiveAddress {
                base: base.into(),
                index: Some(index),
                scale: None,
                displacement: None,
            },
        );
        codegen.arch.array_offset(
            &r_loc,
            &index.dest(codegen.arch.word_size()),
            self.type_(&codegen.scope)?
                .size(&codegen.arch, &codegen.scope)?,
        )?;

        Ok(Destination::Memory(Memory {
            effective_address: EffectiveAddress {
                base: Base::Register(r),
                index: None,
                scale: None,
                displacement: None,
            },
            size: self
                .type_(&codegen.scope)?
                .size(&codegen.arch, &codegen.scope)?,
        }))
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
    fn dest(&self, codegen: &mut CodeGen) -> Result<Destination, ArchError> {
        let expr_dest = self.expr.dest(codegen)?.unwrap();
        let r = codegen.arch.alloc().unwrap();

        codegen
            .arch
            .mov(
                &expr_dest.into(),
                &Destination::Register(operands::Register {
                    register: r,
                    size: 8,
                }),
                self.type_(&codegen.scope)?.signed(),
            )
            .unwrap();

        Ok(Destination::Memory(Memory {
            effective_address: EffectiveAddress {
                base: Base::Register(r),
                index: None,
                scale: None,
                displacement: None,
            },
            size: self
                .expr
                .type_(&codegen.scope)?
                .inner()?
                .size(&codegen.arch, &codegen.scope)?,
        }))
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

impl Expression for ExprCast {
    fn type_(&self, symbtable: &Scope) -> Result<Type, ExprError> {
        let expr_type = self.expr.type_(symbtable)?;

        Ok(expr_type.cast(self.type_.clone())?)
    }
}
