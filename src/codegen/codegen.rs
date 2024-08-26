use super::{operands, CodeGenError, Destination, EffectiveAddress, Immediate, Offset, Source};
use crate::{
    archs::Arch,
    parser::{
        BinOp, CmpOp, Expr, ExprArrayAccess, ExprBinary, ExprFunctionCall, ExprIdent, ExprLit,
        ExprStruct, ExprStructAccess, ExprUnary, Expression, LValue, Stmt, StmtFunction,
        StmtReturn, StmtVarDecl, UnOp,
    },
    scope::Scope,
    symbol_table::SymbolTableError,
    type_table,
    types::{Type, TypeError},
};

pub struct CodeGen {
    pub arch: Arch,
    pub scope: Scope,
}

impl CodeGen {
    pub fn new(arch: Arch, scope: Scope) -> Self {
        Self { arch, scope }
    }

    fn declare(&mut self, variable: StmtVarDecl) -> Result<(), CodeGenError> {
        if !self.scope.local() {
            self.arch.declare(
                &variable.name,
                variable.type_.size(&self.arch, &self.scope)?,
            );
        }

        if let Some(expr) = variable.value {
            self.expr(
                Expr::Binary(ExprBinary::new(
                    BinOp::Assign,
                    Box::new(Expr::Ident(ExprIdent(variable.name))),
                    Box::new(expr),
                )),
                None,
            )?;
        }

        Ok(())
    }

    fn function(&mut self, mut func: StmtFunction) -> Result<(), CodeGenError> {
        let offset = self
            .arch
            .populate_offsets(&mut func.scope.symbol_table, &self.scope)?;

        self.scope.enter(*func.scope);
        self.arch.fn_preamble(
            &func.name,
            &func
                .params
                .iter()
                .map(|(_, type_)| type_.to_owned())
                .collect::<Vec<Type>>(),
            offset,
            &self.scope,
        )?;

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

            self.expr(expr, Some(r.dest(type_.size(&self.arch, &self.scope)?)))?;
            self.arch.ret(
                &Source::Register(operands::Register {
                    register: r,
                    size: type_.size(&self.arch, &self.scope)?,
                }),
                type_.signed(),
            )?;
        }

        self.arch.jmp(label);

        Ok(())
    }

    pub fn expr(&mut self, expr: Expr, mut dest: Option<Destination>) -> Result<(), CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr, dest)?,
            Expr::Lit(lit) => {
                if let Some(dest) = dest {
                    if let ExprLit::String(literal) = &lit {
                        let label = self.arch.define_literal(literal.to_owned());

                        self.arch
                            .mov(&Source::Immediate(Immediate::Label(label)), &dest, false)?;
                    } else {
                        self.arch.mov(
                            &Source::Immediate(lit.clone().into()),
                            &dest,
                            lit.signed(),
                        )?
                    }
                }
            }
            Expr::Unary(unary_expr) => {
                if let Some(dest) = dest {
                    self.unary_expr(unary_expr, dest)?
                }
            }
            Expr::Ident(ident) => {
                if let Some(dest) = dest {
                    let symbol = self
                        .scope
                        .find_symbol(&ident.0)
                        .ok_or(SymbolTableError::NotFound(ident.0.clone()))?;
                    let dst = symbol.dest(&self.arch, &self.scope)?;

                    // If the ident is of type pointer, the address of variable has to be moved, not the value
                    if let Type::Array(_) = ident.type_(&self.scope)? {
                        let r = self.arch.alloc()?;

                        self.arch.lea(
                            &Destination::Register(operands::Register {
                                register: r,
                                size: self.arch.word_size(),
                            }),
                            &EffectiveAddress {
                                base: dst.into(),
                                index: None,
                                scale: None,
                                displacement: None,
                            },
                        );
                        self.arch.mov(
                            &Source::Register(operands::Register {
                                register: r,
                                size: self.arch.word_size(),
                            }),
                            &dest,
                            false,
                        )?;
                        self.arch.free(r)?;
                    } else {
                        self.arch.mov(&dst.into(), &dest, false)?;
                    }
                }
            }
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let type_size = cast_expr
                    .type_(&self.scope)?
                    .size(&self.arch, &self.scope)?;
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

                if let Some(dest) = &mut dest {
                    let size = expr.type_(&self.scope)?.size(&self.arch, &self.scope)?;

                    match dest {
                        Destination::Memory(memory) => {
                            memory.size = size;
                        }
                        Destination::Register(register) => {
                            register.size = size;
                        }
                    };
                }

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
            Expr::ArrayAccess(expr) => {
                if let Some(dest) = dest {
                    self.array_access(expr, dest)?;
                }
            }
        };

        Ok(())
    }

    fn bin_expr(
        &mut self,
        expr: ExprBinary,
        dest: Option<Destination>,
    ) -> Result<(), CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let lvalue_dest = expr
                    .left
                    .dest(self)?
                    .ok_or(CodeGenError::Assign(*expr.left))?;
                let type_ = expr.right.type_(&self.scope)?;

                self.expr(*expr.right, Some(lvalue_dest.clone()))?;

                if let Some(dest) = dest {
                    self.arch.mov(&lvalue_dest.into(), &dest, type_.signed())?;
                }
            }
            BinOp::Add => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let type_ = expr.right.type_(&self.scope)?;
                    let r = self.arch.alloc()?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    )?;
                    self.arch.add(
                        &dest,
                        &Source::Register(operands::Register {
                            register: r,
                            size: type_.size(&self.arch, &self.scope)?,
                        }),
                    );
                    self.arch.free(r)?;
                }
            }
            BinOp::Sub => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    )?;
                    self.arch.sub(
                        &dest,
                        &Source::Register(operands::Register {
                            register: r,
                            size: type_.size(&self.arch, &self.scope)?,
                        }),
                    );
                    self.arch.free(r)?;
                }
            }
            BinOp::Mul => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    )?;
                    self.arch.mul(
                        &dest,
                        &Source::Register(operands::Register {
                            register: r,
                            size: type_.size(&self.arch, &self.scope)?,
                        }),
                        type_.signed(),
                    )?;
                    self.arch.free(r)?;
                }
            }
            BinOp::Div => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()))?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    )?;
                    self.arch.div(
                        &dest,
                        &Source::Register(operands::Register {
                            register: r,
                            size: type_.size(&self.arch, &self.scope)?,
                        }),
                        type_.signed(),
                    )?;
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
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    )?;
                    self.arch.cmp(
                        &dest,
                        &Source::Register(operands::Register {
                            register: r,
                            size: type_.size(&self.arch, &self.scope)?,
                        }),
                        CmpOp::try_from(&expr.op)?,
                    );
                    self.arch.free(r)?;
                }
            }
        };

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodeGenError> {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr, None).map(|_| ()),
            Stmt::VarDecl(var_decl) => self.declare(var_decl),
            Stmt::Function(func) => self.function(func),
            Stmt::Return(ret) => self.ret(ret),
        }
    }

    fn unary_expr(&mut self, unary_expr: ExprUnary, dest: Destination) -> Result<(), CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                self.expr(*unary_expr.expr, Some(dest.clone()))?;
                self.arch.negate(&dest);
            }
            UnOp::Not => {
                let type_ = unary_expr.type_(&self.scope)?;
                let r = self.arch.alloc()?;

                self.expr(
                    *unary_expr.expr,
                    Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                )?;
                self.arch
                    .not(&r.dest(type_.size(&self.arch, &self.scope)?), &dest);
                self.arch.free(r)?;
            }
            UnOp::Address => {
                let expr_dest = unary_expr.expr.dest(self)?.unwrap();
                let r = self.arch.alloc()?;

                self.arch.lea(
                    &Destination::Register(operands::Register {
                        register: r,
                        size: self.arch.word_size(),
                    }),
                    &expr_dest.into(),
                );
                self.arch.mov(
                    &Source::Register(operands::Register {
                        register: r,
                        size: self.arch.word_size(),
                    }),
                    &dest,
                    unary_expr.expr.type_(&self.scope)?.signed(),
                )?;
            }
            UnOp::Deref => {
                let dest2 = unary_expr.expr.dest(self)?.unwrap();
                let r = self.arch.alloc()?;

                self.arch.mov(
                    &dest2.into(),
                    &Destination::Register(operands::Register {
                        register: r,
                        size: self.arch.word_size(),
                    }),
                    false,
                )?;
                self.arch.mov(
                    &Source::Register(operands::Register {
                        register: r,
                        size: unary_expr
                            .type_(&self.scope)?
                            .size(&self.arch, &self.scope)?,
                    }),
                    &dest,
                    unary_expr.expr.type_(&self.scope)?.signed(),
                )?;
            }
        };

        Ok(())
    }

    fn call_function(
        &mut self,
        call: ExprFunctionCall,
        dest: Option<Destination>,
    ) -> Result<(), CodeGenError> {
        let mut preceding = Vec::new();
        let mut stack_size = 0;

        for expr in call.arguments.into_iter() {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;

            self.expr(expr, Some(r.dest(self.arch.word_size())))?;
            stack_size += self.arch.push_arg(
                Source::Register(operands::Register {
                    register: r,
                    size: self.arch.word_size(),
                }),
                &type_,
                &preceding,
            );
            self.arch.free(r)?;
            preceding.push(type_);
        }

        self.arch.call_fn(&call.name, dest.as_ref());
        if stack_size > 0 {
            self.arch.shrink_stack(stack_size);
        }

        Ok(())
    }

    fn struct_expr(&mut self, expr: ExprStruct, dest: Destination) -> Result<(), CodeGenError> {
        let type_struct = match self
            .scope
            .find_type(&expr.name)
            .ok_or(TypeError::Nonexistent(expr.name))?
        {
            type_table::Type::Struct(type_) => type_,
        }
        .clone();
        //NOTE: clone bad ^

        for (name, expr) in expr.fields.into_iter() {
            let offset = type_struct.offset(&self.arch, &name, &self.scope)?;
            let field_size = type_struct
                .get_field_type(&name)
                .unwrap()
                .size(&self.arch, &self.scope)?;

            self.expr(
                expr,
                Some(match dest.clone() {
                    Destination::Memory(mut memory) => {
                        memory.effective_address.displacement = Some(
                            &memory
                                .effective_address
                                .displacement
                                .unwrap_or(Offset::default())
                                + &offset,
                        );
                        memory.size = field_size;

                        Destination::Memory(memory)
                    }
                    _ => todo!(),
                }),
            )?;
        }

        Ok(())
    }

    fn struct_access(
        &mut self,
        expr: ExprStructAccess,
        dest: Destination,
    ) -> Result<(), CodeGenError> {
        let expr_dest = expr.dest(self)?;

        Ok(self
            .arch
            .mov(&expr_dest.into(), &dest, expr.type_(&self.scope)?.signed())?)
    }

    fn array_access(
        &mut self,
        expr: ExprArrayAccess,
        dest: Destination,
    ) -> Result<(), CodeGenError> {
        let expr_dest = expr.dest(self)?;

        Ok(self
            .arch
            .mov(&expr_dest.into(), &dest, expr.type_(&self.scope)?.signed())?)
    }

    pub fn compile(&mut self, program: Vec<Stmt>) -> Result<Vec<u8>, CodeGenError> {
        for stmt in program {
            self.stmt(stmt)?;
        }

        Ok(self.arch.finish())
    }
}
