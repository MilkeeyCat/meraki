use super::locations::{DestinationLocal, MoveDestination, MoveSource, SourceRegister};
use crate::{
    archs::Architecture,
    parser::{
        BinOp, CmpOp, Expr, ExprBinary, ExprFunctionCall, ExprLit, ExprStruct, ExprStructAccess,
        ExprUnary, Expression, OpParseError, Stmt, StmtFunction, StmtReturn, StmtVarDecl, UnOp,
    },
    register_allocator::AllocatorError,
    scope::Scope,
    symbol_table::Symbol,
    type_::{Type, TypeError},
    type_table,
};
use std::fs::File;
use std::io::Write;

#[derive(Debug)]
pub enum CodeGenError {
    OpParse(OpParseError),
    Type(TypeError),
    Allocator(AllocatorError),
    Assign(Expr),
}

impl std::error::Error for CodeGenError {}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OpParse(e) => write!(f, "{}", e),
            Self::Type(e) => write!(f, "{}", e),
            Self::Allocator(e) => write!(f, "{}", e),
            Self::Assign(e) => write!(f, "Can't assign to non ident {:?}", e),
        }
    }
}

impl From<TypeError> for CodeGenError {
    fn from(value: TypeError) -> Self {
        Self::Type(value)
    }
}

impl From<AllocatorError> for CodeGenError {
    fn from(value: AllocatorError) -> Self {
        Self::Allocator(value)
    }
}

impl From<OpParseError> for CodeGenError {
    fn from(value: OpParseError) -> Self {
        Self::OpParse(value)
    }
}

pub struct CodeGen<'a> {
    arch: &'a mut dyn Architecture,
    scope: Scope,
}

impl<'a> CodeGen<'a> {
    pub fn new(arch: &'a mut dyn Architecture, scope: Scope) -> Self {
        Self { arch, scope }
    }

    fn declare(&mut self, variable: StmtVarDecl) {
        if !self.scope.local() {
            self.arch
                .declare(&variable.name, variable.type_.size(self.arch, &self.scope))
        }
    }

    fn function(&mut self, func: StmtFunction) -> Result<(), CodeGenError> {
        self.scope.enter(*func.scope);
        let offset = self.populate_offsets(&func.body);
        self.arch.fn_preamble(&func.name, offset);

        for stmt in func.body {
            self.stmt(stmt)?;
        }

        self.arch.fn_postamble(&func.name, offset);
        self.scope.leave();

        Ok(())
    }

    fn ret(&mut self, ret: StmtReturn) -> Result<(), CodeGenError> {
        let label = &ret.label;
        if let Some(expr) = ret.expr {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;
            self.expr(expr, Some((&r).into()))?;
            self.arch.ret(r, type_, &self.scope);
        }

        self.arch.jmp(label);

        Ok(())
    }

    fn expr(&mut self, expr: Expr, dest: Option<MoveDestination>) -> Result<(), CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr, dest)?,
            Expr::Lit(lit) => {
                if let Some(dest) = dest {
                    self.arch.mov(MoveSource::Lit(lit), dest, &self.scope)
                }
            }
            Expr::Unary(unary_expr) => {
                if let Some(dest) = dest {
                    self.unary_expr(unary_expr, dest)?
                }
            }
            Expr::Ident(ident) => {
                let symbol = self.scope.find_symbol(&ident.0).unwrap();

                if let Some(dest) = dest {
                    self.arch
                        .mov(symbol.to_source(self.arch, &self.scope), dest, &self.scope);
                }
            }
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let type_size = cast_expr.type_(&self.scope)?.size(self.arch, &self.scope);
                let expr = match *cast_expr.expr {
                    Expr::Lit(ExprLit::Int(mut int)) => {
                        int.zero_except_n_bytes(type_size);

                        Expr::Lit(ExprLit::Int(int))
                    }
                    Expr::Lit(ExprLit::UInt(mut uint)) => {
                        uint.zero_except_n_bytes(type_size);

                        Expr::Lit(ExprLit::UInt(uint))
                    }
                    expr => expr,
                };
                self.expr(expr, dest)?;
            }
            Expr::FunctionCall(func_call) => self.call_function(func_call, dest)?,
            Expr::Struct(expr) => {
                if let Some(dest) = dest {
                    self.struct_expr(expr, dest)?
                }
            }
            Expr::StructAccess(expr) => {
                if let Some(dest) = dest {
                    self.struct_access(expr, dest)?;
                }
            }
        };

        Ok(())
    }

    fn bin_expr(
        &mut self,
        expr: ExprBinary,
        dest: Option<MoveDestination>,
    ) -> Result<(), CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let left = *expr.left;
                if let Expr::Ident(name) = &left {
                    let symbol = self.scope.find_symbol(&name.0).unwrap().clone();
                    let symbol_dest: MoveDestination = (&symbol).into();

                    // NOTE: clone bad
                    self.expr(*expr.right, Some(symbol_dest.clone()))?;

                    if let Some(dest) = dest {
                        let r = self.arch.alloc()?;

                        self.arch.mov(
                            symbol_dest.to_source(symbol.type_().size(self.arch, &self.scope)),
                            (&r).into(),
                            &self.scope,
                        );
                        self.arch.mov(
                            MoveSource::Register(SourceRegister {
                                register: &r,
                                size: symbol.type_().size(self.arch, &self.scope),
                                signed: symbol.type_().signed(),
                                offset: None,
                            }),
                            dest,
                            &self.scope,
                        );
                        self.arch.free(r)?;
                    }
                } else {
                    return Err(CodeGenError::Assign(left));
                }
            }
            BinOp::Add => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    self.expr(*expr.right, Some((&r).into()))?;

                    self.arch.add(dest.register(), &r);
                    self.arch.free(r)?;
                }
            }
            BinOp::Sub => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    self.expr(*expr.right, Some((&r).into()))?;

                    self.arch.sub(dest.register(), &r);
                    self.arch.free(r)?;
                }
            }
            BinOp::Mul => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    self.expr(*expr.right, Some((&r).into()))?;

                    self.arch.mul(dest.register(), &r);
                    self.arch.free(r)?;
                }
            }
            BinOp::Div => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    self.expr(*expr.right, Some((&r).into()))?;

                    self.arch.div(dest.register(), &r);
                    self.arch.free(r)?;
                }
            }
            BinOp::LessThan
            | BinOp::LessEqual
            | BinOp::GreaterThan
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    self.expr(*expr.right, Some((&r).into()))?;

                    self.arch
                        .cmp(dest.register(), &r, CmpOp::try_from(&expr.op)?);
                    self.arch.free(r)?;
                }
            }
        };

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodeGenError> {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr, None).map(|_| ()),
            Stmt::VarDecl(var_decl) => Ok(self.declare(var_decl)),
            Stmt::Function(func) => self.function(func),
            Stmt::Return(ret) => self.ret(ret),
        }
    }

    fn unary_expr(
        &mut self,
        unary_expr: ExprUnary,
        dest: MoveDestination,
    ) -> Result<(), CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                self.expr(*unary_expr.expr, Some(dest.clone()))?;
                self.arch.negate(dest.register());
            }
            UnOp::Not => {
                let r = self.arch.alloc()?;

                self.expr(*unary_expr.expr, Some((&r).into()))?;
                self.arch.not(&r, dest.register());
                self.arch.free(r)?;
            }
        };

        Ok(())
    }

    fn call_function(
        &mut self,
        call: ExprFunctionCall,
        dest: Option<MoveDestination>,
    ) -> Result<(), CodeGenError> {
        if call.arguments.len() > 6 {
            panic!("Can't call function with more than 6 arguments");
        }

        for (i, argument) in call.arguments.into_iter().enumerate() {
            let r = self.arch.alloc()?;
            self.expr(argument, Some((&r).into()))?;

            //NOTE: should the register be freed?
            self.arch.move_function_argument(r, i);
        }

        if let Some(dest) = dest {
            self.arch.call_fn(&call.name, Some(dest.register()));
        } else {
            self.arch.call_fn(&call.name, None);
        }

        Ok(())
    }

    fn struct_expr(&mut self, expr: ExprStruct, dest: MoveDestination) -> Result<(), CodeGenError> {
        let type_struct = match self.scope.find_type(&expr.name).unwrap() {
            type_table::Type::Struct(type_) => type_,
            _ => panic!("Expected type to be struct"),
        }
        .clone();
        //NOTE: clone bad ^
        let struct_size = type_struct.size(self.arch, &self.scope);

        for (name, expr) in expr.fields.into_iter() {
            let offset = type_struct.offset(self.arch, &name, &self.scope);
            self.expr(
                expr,
                Some(MoveDestination::Local(DestinationLocal {
                    // NOTE: local variable use 1-based offset but struct offsets are 0-based, so to plumb it correctly gotta slap that -1
                    offset: struct_size + dest.local_offset() - offset - 1,
                })),
            )?;
        }

        Ok(())
    }

    fn struct_access(
        &mut self,
        expr: ExprStructAccess,
        dest: MoveDestination,
    ) -> Result<(), CodeGenError> {
        let symbol = self.scope.find_symbol(&expr.name).unwrap();
        let field_offset = match symbol.type_() {
            Type::Struct(s) => match self.scope.find_type(&s).unwrap() {
                type_table::Type::Struct(type_struct) => {
                    type_struct.offset(self.arch, &expr.field, &self.scope)
                }
            },
            _ => panic!(),
        };

        let mut src = symbol.to_source(self.arch, &self.scope);
        match &mut src {
            MoveSource::Local(local) => {
                local.offset += field_offset;
            }
            MoveSource::Global(global) => match &mut global.offset {
                Some(offset) => *offset += field_offset,
                None => global.offset = Some(field_offset),
            },
            _ => unreachable!(),
        };

        self.arch.mov(src, dest, &self.scope);

        Ok(())
    }

    pub fn compile(&mut self, program: Vec<Stmt>, path: &str) -> Result<(), CodeGenError> {
        let mut file = File::create(path).expect(&format!("Failed to open a file {}", path));

        for stmt in program {
            self.stmt(stmt)?;
        }

        file.write_all(&self.arch.finish())
            .expect("Failed to write generated code to output file");

        Ok(())
    }

    fn populate_offsets(&mut self, stmts: &Vec<Stmt>) -> usize {
        let mut offset = 1;

        for stmt in stmts {
            if let Stmt::VarDecl(var_decl) = stmt {
                if let Symbol::Local(local) = self.scope.find_symbol_mut(&var_decl.name).unwrap() {
                    local.offset = offset;
                    offset += var_decl.type_.size(self.arch, &self.scope);
                }
            }
        }

        let alignment = self.arch.alignment();

        if offset < alignment {
            alignment
        } else if offset % alignment == 0 {
            offset
        } else {
            ((offset / alignment) + 1) * alignment
        }
    }
}
