use super::{operands, CodeGenError, Destination, EffectiveAddress, Immediate, Offset, Source};
use crate::{
    archs::{Arch, Jump},
    parser::{
        BinOp, BitwiseOp, CmpOp, Expr, ExprArray, ExprArrayAccess, ExprBinary, ExprFunctionCall,
        ExprIdent, ExprLit, ExprStruct, ExprStructAccess, ExprStructMethod, ExprUnary, Expression,
        LValue, Stmt, StmtFor, StmtFunction, StmtIf, StmtReturn, StmtVarDecl, StmtWhile, UnOp,
    },
    scope::Scope,
    symbol_table::{Symbol, SymbolTableError},
    type_table as tt,
    types::{Type, TypeError},
};
use operands::{Base, Memory};

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
                Expr::Binary(ExprBinary {
                    op: BinOp::Assign,
                    left: Box::new(Expr::Ident(ExprIdent(variable.name))),
                    right: Box::new(expr),
                }),
                None,
                None,
            )?;
        }

        Ok(())
    }

    fn function(&mut self, mut func: StmtFunction) -> Result<(), CodeGenError> {
        let offset = self
            .arch
            .populate_offsets(&mut func.block, &self.scope, Default::default())?
            .unsigned_abs()
            .next_multiple_of(self.arch.stack_alignment());

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

        self.arch.jcc(label, Jump::Unconditional);

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
        );

        let consequence_label = self.arch.generate_label();
        let alternative_label = self.arch.generate_label();

        self.arch.jcc(&alternative_label, Jump::Equal);
        self.scope.enter(if_stmt.consequence.scope);
        for stmt in if_stmt.consequence.statements {
            self.stmt(stmt)?;
        }
        self.scope.leave();

        if if_stmt.alternative.is_some() {
            self.arch.jcc(&consequence_label, Jump::Unconditional);
        }

        self.arch.write_label(&alternative_label);
        if let Some(block) = if_stmt.alternative {
            self.scope.enter(block.scope);

            for stmt in block.statements {
                self.stmt(stmt)?;
            }

            self.scope.leave();
            self.arch.write_label(&consequence_label);
        }

        Ok(())
    }

    fn while_stmt(&mut self, stmt: StmtWhile) -> Result<(), CodeGenError> {
        let start_label = self.arch.generate_label();
        let end_label = self.arch.generate_label();
        let r = self.arch.alloc()?;

        self.arch.write_label(&start_label);
        self.expr(stmt.condition, Some(r.dest(1)), None)?;
        self.arch.cmp(
            &Destination::Register(operands::Register {
                register: r,
                size: 1,
            }),
            &Source::Immediate(Immediate::UInt(0)),
        );
        self.arch.jcc(&end_label, Jump::Equal);

        self.scope.enter(stmt.block.scope);
        for stmt in stmt.block.statements {
            self.stmt(stmt)?;
        }
        self.scope.leave();

        self.arch.jcc(&start_label, Jump::Unconditional);
        self.arch.write_label(&end_label);

        self.arch.free(r)?;

        Ok(())
    }

    fn for_stmt(&mut self, stmt: StmtFor) -> Result<(), CodeGenError> {
        let start_label = self.arch.generate_label();
        let end_label = self.arch.generate_label();
        let r = self.arch.alloc()?;

        self.scope.enter(stmt.block.scope);

        if let Some(initializer) = stmt.initializer {
            self.stmt(*initializer)?;
        }

        self.arch.write_label(&start_label);
        if let Some(condition) = stmt.condition {
            self.expr(condition, Some(r.dest(1)), None)?;
            self.arch.cmp(
                &Destination::Register(operands::Register {
                    register: r,
                    size: 1,
                }),
                &Source::Immediate(Immediate::UInt(0)),
            );
            self.arch.jcc(&end_label, Jump::Equal);
        }

        for stmt in stmt.block.statements {
            self.stmt(stmt)?;
        }

        if let Some(increment) = stmt.increment {
            self.expr(increment, None, None)?;
        }

        self.scope.leave();

        self.arch.jcc(&start_label, Jump::Unconditional);
        self.arch.write_label(&end_label);

        self.arch.free(r)?;

        Ok(())
    }

    pub fn expr(
        &mut self,
        expr: Expr,
        dest: Option<Destination>,
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

                    // If the ident is of type array, the address of variable has to be moved, not the value
                    if let Type::Array(_) = ident.type_(&self.scope)? {
                        let r = self.arch.alloc()?;
                        let r_op = operands::Register {
                            register: r,
                            size: self.arch.word_size(),
                        };

                        self.arch
                            .lea(&Destination::Register(r_op.clone()), &dst.into());
                        self.arch.mov(&Source::Register(r_op), &dest, false)?;
                        self.arch.free(r)?;
                    } else {
                        self.arch.mov(&dst.into(), &dest, false)?;
                    }
                }
            }
            Expr::Cast(cast_expr) => {
                if let Some(dest) = dest {
                    let type_ = cast_expr.expr.type_(&self.scope)?;
                    let casted_size = cast_expr.type_.size(&self.arch, &self.scope)?;
                    let r = self.arch.alloc()?;

                    self.expr(*cast_expr.expr, Some(r.dest(self.arch.word_size())), state)?;
                    self.arch
                        .mov(&r.source(casted_size), &dest, type_.signed())?;
                    self.arch.free(r)?;
                }
            }
            Expr::FunctionCall(func_call) => self.call_function(func_call, dest, state)?,
            Expr::Struct(expr) => {
                if let Some(dest) = dest {
                    self.struct_expr(expr, dest, state)?
                }
            }
            Expr::Array(expr) => {
                if let Some(dest) = dest {
                    self.array_expr(expr, dest, state)?
                }
            }
            Expr::StructAccess(expr) => {
                if let Some(dest) = dest {
                    self.struct_access(expr, dest)?;
                }
            }
            Expr::StructMethod(expr) => {
                self.struct_call_method(expr, dest, state)?;
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
                    self.arch
                        .mov(&lvalue_dest.clone().into(), &dest, type_.signed())?;
                }

                self.free(lvalue_dest)?;
            }
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::BitwiseAnd
            | BinOp::BitwiseOr => {
                if let Some(dest) = dest {
                    self.expr(*expr.left, Some(dest.clone()), state)?;

                    let type_ = expr.right.type_(&self.scope)?;
                    let r = self.arch.alloc()?;

                    self.expr(
                        *expr.right,
                        Some(r.dest(type_.size(&self.arch, &self.scope)?)),
                        state,
                    )?;

                    match &expr.op {
                        BinOp::Add => {
                            self.arch.add(
                                &dest,
                                &Source::Register(operands::Register {
                                    register: r,
                                    size: type_.size(&self.arch, &self.scope)?,
                                }),
                            );
                        }
                        BinOp::Sub => {
                            self.arch.sub(
                                &dest,
                                &Source::Register(operands::Register {
                                    register: r,
                                    size: type_.size(&self.arch, &self.scope)?,
                                }),
                            );
                        }
                        BinOp::Mul => {
                            self.arch.mul(
                                &dest,
                                &Source::Register(operands::Register {
                                    register: r,
                                    size: type_.size(&self.arch, &self.scope)?,
                                }),
                                type_.signed(),
                            )?;
                        }
                        BinOp::Div => {
                            self.arch.div(
                                &dest,
                                &Source::Register(operands::Register {
                                    register: r,
                                    size: type_.size(&self.arch, &self.scope)?,
                                }),
                                type_.signed(),
                            )?;
                        }
                        BinOp::BitwiseAnd | BinOp::BitwiseOr => {
                            self.arch.bitwise(
                                &dest,
                                &Source::Register(operands::Register {
                                    register: r,
                                    size: type_.size(&self.arch, &self.scope)?,
                                }),
                                BitwiseOp::try_from(&expr.op).unwrap(),
                            );
                        }
                        _ => unreachable!(),
                    };

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
                    let size = std::cmp::max(
                        expr.left
                            .type_(&self.scope)?
                            .size(&self.arch, &self.scope)?,
                        expr.right
                            .type_(&self.scope)?
                            .size(&self.arch, &self.scope)?,
                    );
                    let left = self.arch.alloc()?;
                    let right = self.arch.alloc()?;

                    self.expr(*expr.left, Some(left.dest(size)), state)?;
                    self.expr(*expr.right, Some(right.dest(size)), state)?;
                    self.arch.cmp(&left.dest(size), &right.source(size));
                    self.arch.setcc(&dest, CmpOp::try_from(&expr.op)?);
                    self.arch.free(left)?;
                    self.arch.free(right)?;
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
                                codegen
                                    .arch
                                    .cmp(&dest, &Source::Immediate(Immediate::UInt(0)));

                                if op == &BinOp::LogicalAnd {
                                    codegen
                                        .arch
                                        .jcc(&state.false_label.as_ref().unwrap(), Jump::Equal);
                                } else {
                                    codegen
                                        .arch
                                        .jcc(&state.false_label.as_ref().unwrap(), Jump::NotEqual);
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
                                .jcc(state.end_label.as_ref().unwrap(), Jump::Unconditional);
                            self.arch.write_label(state.false_label.as_ref().unwrap());
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
                        } else {
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), &dest, false)?;
                            self.arch
                                .jcc(state.end_label.as_ref().unwrap(), Jump::Unconditional);
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
            Stmt::If(stmt) => self.if_stmt(stmt),
            Stmt::While(stmt) => self.while_stmt(stmt),
            Stmt::For(stmt) => self.for_stmt(stmt),
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
            UnOp::LogicalNot => {
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
                    &expr_dest.clone().into(),
                );
                self.arch.mov(
                    &Source::Register(operands::Register {
                        register: r,
                        size: self.arch.word_size(),
                    }),
                    &dest,
                    unary_expr.expr.type_(&self.scope)?.signed(),
                )?;
                self.free(expr_dest)?;
            }
            UnOp::Deref => {
                let expr_dest = unary_expr.dest(self)?;

                self.arch.mov(
                    &expr_dest.clone().into(),
                    &dest,
                    unary_expr.expr.type_(&self.scope)?.signed(),
                )?;
                self.free(expr_dest)?;
            }
            UnOp::BitwiseNot => {
                self.expr(*unary_expr.expr, Some(dest.clone()), state)?;

                self.arch.bitwise_not(&dest);
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

        let function = match self
            .scope
            .find_symbol(&call.name)
            .ok_or(SymbolTableError::NotFound(call.name.clone()))
            .unwrap()
        {
            Symbol::Function(func) => func,
            _ => unreachable!(),
        };
        self.arch.call_fn(
            &call.name,
            dest.as_ref(),
            function.return_type.signed(),
            function.return_type.size(&self.arch, &self.scope)?,
        )?;
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
            tt::Type::Struct(type_) => type_,
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

    fn array_expr(
        &mut self,
        expr: ExprArray,
        dest: Destination,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        for (i, expr) in expr.0.into_iter().enumerate() {
            let size = expr.type_(&self.scope)?.size(&self.arch, &self.scope)?;
            let index = self.arch.alloc()?;
            let r = self.arch.alloc()?;
            let r_loc = r.dest(self.arch.word_size());

            self.arch.mov(
                &Source::Immediate(Immediate::UInt(i as u64)),
                &index.dest(self.arch.word_size()),
                false,
            )?;

            match &dest {
                Destination::Memory(memory) => {
                    self.arch.lea(&r_loc, &memory.effective_address);
                }
                Destination::Register(_) => unreachable!(),
            }

            self.arch
                .array_offset(&r_loc, &index.dest(self.arch.word_size()), size)?;

            self.expr(
                expr,
                Some(Destination::Memory(Memory {
                    effective_address: EffectiveAddress {
                        base: Base::Register(r),
                        index: None,
                        scale: None,
                        displacement: None,
                    },
                    size,
                })),
                state,
            )?;
            self.arch.free(index)?;
            self.arch.free(r)?;
        }

        Ok(())
    }

    fn struct_access(
        &mut self,
        expr: ExprStructAccess,
        dest: Destination,
    ) -> Result<(), CodeGenError> {
        let expr_dest = expr.dest(self)?;

        self.arch.mov(
            &expr_dest.clone().into(),
            &dest,
            expr.type_(&self.scope)?.signed(),
        )?;
        self.free(expr_dest)
    }

    fn struct_call_method(
        &mut self,
        expr: ExprStructMethod,
        dest: Option<Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        let struct_name = expr.expr.type_(&self.scope)?.struct_unchecked();
        let expr_dest = expr.expr.dest(self)?.unwrap();
        let r = self.arch.alloc()?;
        let effective_address = match expr_dest.clone() {
            Destination::Memory(memory) => memory.effective_address,
            _ => unreachable!(),
        };

        self.arch
            .lea(&r.dest(self.arch.word_size()), &effective_address);

        let this = Type::Ptr(Box::new(Type::Struct(struct_name.clone())));
        let mut preceding = Vec::new();
        let mut stack_size = 0;

        self.arch
            .push_arg(r.source(self.arch.word_size()), &this, &preceding);
        preceding.push(this);

        for expr in expr.arguments.into_iter() {
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

        let method = match self.scope.find_type(&struct_name).unwrap() {
            tt::Type::Struct(type_) => type_.find_method(&expr.method).unwrap(),
        };
        self.arch.call_fn(
            &format!("{struct_name}__{}", expr.method),
            dest.as_ref(),
            method.return_type.signed(),
            method.return_type.size(&self.arch, &self.scope)?,
        )?;
        if stack_size > 0 {
            self.arch.shrink_stack(stack_size);
        }
        self.free(expr_dest)?;
        self.arch.free(r)?;

        Ok(())
    }

    fn array_access(
        &mut self,
        expr: ExprArrayAccess,
        dest: Destination,
    ) -> Result<(), CodeGenError> {
        let expr_dest = expr.dest(self)?;

        self.arch.mov(
            &expr_dest.clone().into(),
            &dest,
            expr.type_(&self.scope)?.signed(),
        )?;
        self.free(expr_dest)
    }

    fn free(&mut self, dest: Destination) -> Result<(), CodeGenError> {
        Ok(match dest {
            Destination::Memory(memory) => {
                match memory.effective_address.base {
                    Base::Register(register) => self.arch.free(register)?,
                    Base::Label(_) => {}
                }
                if let Some(index) = memory.effective_address.index {
                    self.arch.free(index)?;
                }
            }
            Destination::Register(register) => {
                self.arch.free(register.register)?;
            }
        })
    }

    pub fn compile(&mut self, program: Vec<Stmt>) -> Result<Vec<u8>, CodeGenError> {
        for stmt in program {
            self.stmt(stmt)?;
        }

        Ok(self.arch.finish())
    }
}
