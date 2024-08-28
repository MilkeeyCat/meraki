use super::{operands, CodeGenError, Destination, EffectiveAddress, Immediate, Offset, Source};
use crate::{
    archs::{Arch, Jump},
    parser::{
        BinOp, CmpOp, Expr, ExprArrayAccess, ExprBinary, ExprFunctionCall, ExprIdent, ExprLit,
        ExprStruct, ExprStructAccess, ExprUnary, Expression, LValue, Stmt, StmtFunction, StmtIf,
        StmtReturn, StmtVarDecl, UnOp,
    },
    scope::Scope,
    symbol_table::SymbolTableError,
    type_table,
    types::{Type, TypeError},
};

#[derive(Debug, Clone)]
pub struct State {
    false_label: Option<String>,
    end_label: Option<String>,
}

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
                None,
            )?;
        }

        Ok(())
    }

    fn function(&mut self, mut func: StmtFunction) -> Result<(), CodeGenError> {
        let offset = self
            .arch
            .populate_offsets(&mut func.block.scope.symbol_table, &self.scope)?;

        self.scope.enter(func.block.scope);
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

        for stmt in func.block.statements {
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

            self.expr(
                expr,
                Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                None,
            )?;
            self.arch.ret(
                &Source::Register(operands::Register {
                    register: r,
                    size: type_.size(&self.arch, &self.scope)?,
                }),
                type_.signed(),
            )?;
        }

        self.arch.jmp(label, Jump::Unconditional);

        Ok(())
    }

    fn if_stmt(&mut self, if_stmt: StmtIf) -> Result<(), CodeGenError> {
        let r = self.arch.alloc()?;
        let expr_size = if_stmt
            .condition
            .type_(&self.scope)?
            .size(&self.arch, &self.scope)?;

        self.expr(
            if_stmt.condition,
            Some(Destination::Register(operands::Register {
                register: r,
                size: expr_size,
            })),
            None,
        )?;
        self.arch.cmp(
            &Destination::Register(operands::Register {
                register: r,
                size: expr_size,
            }),
            &Source::Immediate(Immediate::UInt(0)),
            CmpOp::Equal,
        );

        let consequence_label = self.arch.generate_label();
        let alternative_label = self.arch.generate_label();

        self.arch.jmp(&alternative_label, Jump::Equal);
        self.scope.enter(if_stmt.consequence.scope);
        for stmt in if_stmt.consequence.statements {
            self.stmt(stmt)?;
        }
        self.scope.leave();

        if if_stmt.alternative.is_some() {
            self.arch.jmp(&consequence_label, Jump::Unconditional);
        }

        if let Some(block) = if_stmt.alternative {
            self.arch.write_label(&alternative_label);
            self.scope.enter(block.scope);

            for stmt in block.statements {
                self.stmt(stmt)?;
            }

            self.scope.leave();
            self.arch.write_label(&consequence_label);
        }

        Ok(())
    }

    pub fn expr(
        &mut self,
        expr: Expr,
        mut dest: Option<Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr, dest, state)?,
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
                    self.unary_expr(unary_expr, dest, state)?
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

                self.expr(expr, dest, state)?;
            }
            Expr::FunctionCall(func_call) => self.call_function(func_call, dest, state)?,
            Expr::Struct(expr) => {
                if let Some(dest) = dest {
                    self.struct_expr(expr, dest, state)?
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
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let lvalue_dest = expr
                    .left
                    .dest(self)?
                    .ok_or(CodeGenError::Assign(*expr.left))?;
                let type_ = expr.right.type_(&self.scope)?;

                self.expr(*expr.right, Some(lvalue_dest.clone()), state)?;

                if let Some(dest) = dest {
                    self.arch.mov(&lvalue_dest.into(), &dest, type_.signed())?;
                }
            }
            BinOp::Add => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let type_ = expr.right.type_(&self.scope)?;
                    let r = self.arch.alloc()?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
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
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
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
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
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
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
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
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let r = self.arch.alloc()?;
                    let type_ = expr.right.type_(&self.scope)?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
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
            BinOp::LogicalAnd | BinOp::LogicalOr => {
                if let Some(dest) = dest {
                    let mut parent = false;
                    let state = state.map(|state| state.to_owned()).unwrap_or_else(|| {
                        parent = true;

                        State {
                            false_label: Some(self.arch.generate_label()),
                            end_label: Some(self.arch.generate_label()),
                        }
                    });
                    let eval =
                        |codegen: &mut Self, expr: Expr, op: &BinOp| -> Result<(), CodeGenError> {
                            let cmp = match &expr {
                                Expr::Binary(ExprBinary {
                                    op: BinOp::LogicalAnd,
                                    ..
                                })
                                | Expr::Binary(ExprBinary {
                                    op: BinOp::LogicalOr,
                                    ..
                                }) => false,
                                _ => true,
                            };

                            let opposite = if op == &BinOp::LogicalAnd {
                                &BinOp::LogicalOr
                            } else {
                                &BinOp::LogicalAnd
                            };

                            match &expr {
                                Expr::Binary(ExprBinary { op, .. }) if op == opposite => {
                                    codegen.expr(expr, Some(dest.clone()), None)?;
                                }
                                _ => {
                                    codegen.expr(expr, Some(dest.clone()), Some(&state))?;
                                }
                            }

                            if cmp {
                                codegen.arch.cmp(
                                    &dest,
                                    &Source::Immediate(Immediate::UInt(0)),
                                    CmpOp::Equal,
                                );

                                if op == &BinOp::LogicalAnd {
                                    codegen
                                        .arch
                                        .jmp(&state.false_label.as_ref().unwrap(), Jump::Equal);
                                } else {
                                    codegen
                                        .arch
                                        .jmp(&state.false_label.as_ref().unwrap(), Jump::NotEqual);
                                }
                            }

                            Ok(())
                        };

                    eval(self, *expr.left, &expr.op)?;
                    eval(self, *expr.right, &expr.op)?;

                    if parent {
                        if expr.op == BinOp::LogicalAnd {
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(1)), &dest, false)?;
                            self.arch
                                .jmp(state.end_label.as_ref().unwrap(), Jump::Unconditional);
                            self.arch.write_label(state.false_label.as_ref().unwrap());
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
                        } else {
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
                            self.arch
                                .jmp(state.end_label.as_ref().unwrap(), Jump::Unconditional);
                            self.arch.write_label(state.false_label.as_ref().unwrap());
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(1)), &dest, false)?;
                        }

                        self.arch.write_label(state.end_label.as_ref().unwrap());
                    }
                }
            }
        };

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodeGenError> {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr, None, None).map(|_| ()),
            Stmt::VarDecl(var_decl) => self.declare(var_decl),
            Stmt::Function(func) => self.function(func),
            Stmt::Return(ret) => self.ret(ret),
            Stmt::If(if_stmt) => self.if_stmt(if_stmt),
        }
    }

    fn unary_expr(
        &mut self,
        unary_expr: ExprUnary,
        dest: Destination,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                self.expr(*unary_expr.expr, Some(dest.clone()), state)?;
                self.arch.negate(&dest);
            }
            UnOp::Not => {
                let type_ = unary_expr.type_(&self.scope)?;
                let r = self.arch.alloc()?;

                self.expr(
                    *unary_expr.expr,
                    Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                    state,
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
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        let mut preceding = Vec::new();
        let mut stack_size = 0;

        for expr in call.arguments.into_iter() {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;

            self.expr(expr, Some(r.dest(self.arch.word_size())), state)?;
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

    fn struct_expr(
        &mut self,
        expr: ExprStruct,
        dest: Destination,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
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
                state,
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
