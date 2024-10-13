use super::{
    operands, Argument, CodeGenError, Destination, EffectiveAddress, Immediate, Offset,
    SethiUllman, Source,
};
use crate::{
    archs::{Arch, Jump},
    parser::{
        BinOp, BitwiseOp, CmpOp, Expr, ExprArray, ExprArrayAccess, ExprBinary, ExprFunctionCall,
        ExprIdent, ExprLit, ExprStruct, ExprStructAccess, ExprStructMethod, ExprUnary, Expression,
        Stmt, StmtFor, StmtFunction, StmtIf, StmtReturn, StmtVarDecl, StmtWhile, UnOp,
    },
    register::Register,
    scope::Scope,
    symbol_table::{Symbol, SymbolTableError},
    type_table as tt,
    types::{Type, TypeError},
};
use operands::{Base, Memory};

enum ScopeInfo {
    Function { label: String },
    Loop { start: String, end: String },
}

#[derive(Debug, Clone)]
pub struct State {
    false_label: Option<String>,
    end_label: Option<String>,
}

pub struct CodeGen {
    pub arch: Arch,
    pub scope: Scope,
    scope_infos: Vec<ScopeInfo>,
}

impl CodeGen {
    pub fn new(arch: Arch, scope: Scope) -> Self {
        Self {
            arch,
            scope,
            scope_infos: Vec::new(),
        }
    }

    fn declare(&mut self, variable: StmtVarDecl) -> Result<(), CodeGenError> {
        if !self.scope.local() {
            self.arch
                .declare(&variable.name, self.arch.size(&variable.type_, &self.scope));
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
        self.scope_infos.push(ScopeInfo::Function {
            label: func.name.clone(),
        });

        for stmt in func.block.statements {
            self.stmt(stmt)?;
        }

        self.scope_infos.pop();
        self.arch.fn_postamble(&func.name, offset);
        self.scope.leave();

        Ok(())
    }

    fn ret(&mut self, ret: StmtReturn) -> Result<(), CodeGenError> {
        if let Some(expr) = ret.expr {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;

            self.expr(
                expr,
                Some(&r.dest(self.arch.size(&type_, &self.scope))),
                None,
            )?;
            self.arch.ret(
                &Source::Register(operands::Register {
                    register: r,
                    size: self.arch.size(&type_, &self.scope),
                }),
                type_.signed(),
            )?;
            self.arch.free(r)?;
        }

        if let Some(label) = self.function_scope_info() {
            self.arch.jcc(&format!("{label}_ret"), Jump::Unconditional);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn if_stmt(&mut self, if_stmt: StmtIf) -> Result<(), CodeGenError> {
        let r = self.arch.alloc()?;
        let expr_size = self
            .arch
            .size(&if_stmt.condition.type_(&self.scope)?, &self.scope);

        self.expr(if_stmt.condition, Some(&r.dest(expr_size)), None)?;
        self.arch.cmp(
            &Destination::Register(operands::Register {
                register: r,
                size: expr_size,
            }),
            &Source::Immediate(Immediate::UInt(0)),
        );
        self.arch.free(r)?;

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
        self.expr(stmt.condition, Some(&r.dest(1)), None)?;
        self.arch.cmp(
            &Destination::Register(operands::Register {
                register: r,
                size: 1,
            }),
            &Source::Immediate(Immediate::UInt(0)),
        );
        self.arch.jcc(&end_label, Jump::Equal);
        self.scope_infos.push(ScopeInfo::Loop {
            start: start_label.clone(),
            end: end_label.clone(),
        });

        self.scope.enter(stmt.block.scope);
        for stmt in stmt.block.statements {
            self.stmt(stmt)?;
        }
        self.scope.leave();

        self.scope_infos.pop();
        self.arch.jcc(&start_label, Jump::Unconditional);
        self.arch.write_label(&end_label);

        self.arch.free(r)?;

        Ok(())
    }

    fn for_stmt(&mut self, stmt: StmtFor) -> Result<(), CodeGenError> {
        let start_label = self.arch.generate_label();
        let end_label = self.arch.generate_label();
        let increment_label = self.arch.generate_label();
        let r = self.arch.alloc()?;

        self.scope.enter(stmt.block.scope);

        if let Some(initializer) = stmt.initializer {
            self.stmt(*initializer)?;
        }

        self.arch.write_label(&start_label);
        if let Some(condition) = stmt.condition {
            self.expr(condition, Some(&r.dest(1)), None)?;
            self.arch.cmp(
                &Destination::Register(operands::Register {
                    register: r,
                    size: 1,
                }),
                &Source::Immediate(Immediate::UInt(0)),
            );
            self.arch.jcc(&end_label, Jump::Equal);
        }

        self.scope_infos.push(ScopeInfo::Loop {
            start: increment_label.clone(),
            end: end_label.clone(),
        });

        for stmt in stmt.block.statements {
            self.stmt(stmt)?;
        }

        self.scope_infos.pop();

        self.arch.write_label(&increment_label);
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
        dest: Option<&Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr, dest, state)?,
            Expr::Lit(lit) => {
                if let Some(dest) = dest {
                    if let ExprLit::String(literal) = &lit {
                        let label = self.arch.define_literal(literal.to_owned());

                        self.arch
                            .mov(&Source::Immediate(Immediate::Label(label)), dest, false)?;
                    } else {
                        let signed = lit.signed();
                        self.arch
                            .mov(&Source::Immediate(lit.into()), dest, signed)?
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
                    let source = symbol.source(&self.arch, &self.scope)?;

                    // If the ident is of type array, the address of variable has to be moved, not the value
                    if let Type::Array(_) = ident.type_(&self.scope)? {
                        let r = self.arch.alloc()?;
                        let r_op = operands::Register {
                            register: r,
                            size: self.arch.word_size(),
                        };

                        self.arch
                            .lea(&Destination::Register(r_op.clone()), &source.clone().into());
                        self.arch.mov(&Source::Register(r_op), dest, false)?;
                        self.arch.free(r)?;
                    } else {
                        self.arch.mov(&source, dest, false)?;
                    }

                    source.free(&mut self.arch)?;
                }
            }
            Expr::Cast(cast_expr) => {
                if let Some(dest) = dest {
                    let type_ = cast_expr.expr.type_(&self.scope)?;
                    let og_size = self
                        .arch
                        .size(&type_, &self.scope)
                        .clamp(0, self.arch.word_size());
                    let casted_size = self.arch.size(&cast_expr.type_, &self.scope);

                    if casted_size != og_size {
                        let (r, new) = match dest {
                            Destination::Memory(_) => (self.arch.alloc()?, true),
                            Destination::Register(register) => (register.register, false),
                        };

                        self.expr(*cast_expr.expr, Some(&r.dest(og_size)), state)?;

                        if new {
                            if casted_size > og_size {
                                self.arch.mov(
                                    &r.source(og_size),
                                    &r.dest(casted_size),
                                    type_.signed(),
                                )?;
                            }
                            self.arch
                                .mov(&r.source(casted_size), dest, type_.signed())?;
                            self.arch.free(r)?;
                        }
                    } else {
                        self.expr(*cast_expr.expr, Some(dest), state)?;
                    }
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
        mut expr: ExprBinary,
        dest: Option<&Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        let type_ = expr.type_(&self.scope)?;
        let left_type = expr.left.type_(&self.scope)?;
        let right_type = expr.right.type_(&self.scope)?;
        let size = self.arch.size(&type_, &self.scope);
        let signed = type_.signed();

        match &expr.op {
            BinOp::Assign => {
                let lvalue_dest = self.expr_dest(*expr.left)?;

                self.expr(*expr.right, Some(&lvalue_dest), state)?;

                if let Some(dest) = dest {
                    self.arch.mov(&lvalue_dest.clone().into(), dest, signed)?;
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
                    let expr_src = |codegen: &mut Self,
                                  expr: Expr,
                                  size: usize,
                                  dest: Option<Destination>,
                                  state: Option<&State>|
                     -> Result<(Source, Option<Register>), CodeGenError> {
                        let (dest, r) = match dest{
                            Some(dest) => (dest, None),
                            None => {
                                let r =codegen.arch.alloc()?;

                                (r.dest(size), Some(r))
                            }
                        };

                        Ok(match expr {
                            Expr::Lit(lit) => (Source::Immediate(lit.into()),r),
                            _ => {
                                codegen.expr(expr, Some(&dest), state)?;

                                (dest.into(), r)
                            }
                        })
                    };

                    if expr.op == BinOp::Add {
                        // Canonicalize foo + T* to T* + foo
                        if type_.ptr() && expr.right.type_(&self.scope).unwrap().ptr() {
                            std::mem::swap(expr.left.as_mut(), expr.right.as_mut());
                        }
                    }

                    let (expr_dest, dest_r) = match dest {
                        Destination::Memory(_) => {
                            let r = self.arch.alloc()?;

                            (r.dest(size), Some(r))
                        }
                        Destination::Register(register) if register.size != size => {
                            (dest.to_owned().with_size(size), None)
                        }
                        _ => (dest.to_owned(), None),
                    };

                    let (lhs, rhs, expr_r) = if expr.left.r_num() > expr.right.r_num() {
                        let (lhs, _) =
                            expr_src(self, *expr.left, size, Some(expr_dest.clone()), state)?;
                        let (rhs, r) = expr_src(self, *expr.right, size, None, state)?;

                        (lhs, rhs, r)
                    } else {
                        let (rhs, _) =
                            expr_src(self, *expr.right, size, Some(expr_dest.clone()), state)?;
                        let (lhs, r) = expr_src(self, *expr.left, size, None, state)?;

                        (lhs, rhs, r)
                    };

                    match &expr.op {
                        BinOp::Add => match (left_type, right_type) {
                            (Type::Ptr(type_), _) => {
                                let r = self.arch.alloc()?;

                                self.arch.mul(
                                    &Source::Immediate(Immediate::UInt(
                                        self.arch.size(&type_, &self.scope) as u64,
                                    )),
                                    &rhs,
                                    &r.dest(size),
                                    signed,
                                )?;
                                self.arch.add(&lhs, &r.source(size), &expr_dest, false)?;

                                self.arch.free(r)?;
                            }
                            _ => self.arch.add(&lhs, &rhs, &expr_dest, signed)?,
                        },
                        BinOp::Sub => match (left_type, right_type) {
                            // type_ and _ are the same
                            (Type::Ptr(type_), Type::Ptr(_)) => {
                                let r = self.arch.alloc()?;

                                self.arch.sub(&lhs, &rhs, &expr_dest, signed)?;
                                self.arch.div(
                                    &expr_dest.clone().into(),
                                    &Source::Immediate(Immediate::UInt(
                                        self.arch.size(&type_, &self.scope) as u64,
                                    )),
                                    &r.dest(size),
                                    true,
                                )?;
                                self.arch.mov(&r.source(size), &expr_dest, true)?;

                                self.arch.free(r)?;
                            }
                            (Type::Ptr(type_), _) => {
                                let r = self.arch.alloc()?;

                                self.arch.mul(
                                    &Source::Immediate(Immediate::UInt(
                                        self.arch.size(&type_, &self.scope) as u64,
                                    )),
                                    &rhs,
                                    &r.dest(size),
                                    signed,
                                )?;
                                self.arch.sub(&lhs, &r.source(size), &expr_dest, false)?;

                                self.arch.free(r)?;
                            }
                            _ => self.arch.sub(&lhs, &rhs, &expr_dest, signed)?,
                        },
                        BinOp::Mul => {
                            self.arch.mul(&lhs, &rhs, &expr_dest, signed)?;
                        }
                        BinOp::Div => {
                            self.arch.div(&lhs, &rhs, &expr_dest, signed)?;
                        }
                        BinOp::BitwiseAnd | BinOp::BitwiseOr => {
                            self.arch.bitwise(
                                &lhs,
                                &rhs,
                                &expr_dest,
                                BitwiseOp::try_from(&expr.op).unwrap(),
                                signed,
                            )?;
                        }
                        _ => unreachable!(),
                    };

                    if dest.size() != expr_dest.size() {
                        self.arch.mov(
                            &expr_dest.clone().into(),
                            &expr_dest.clone().with_size(dest.size()),
                            signed,
                        )?;
                    }
                    if let Destination::Memory(_) = dest {
                        self.arch.mov(
                            &expr_dest.clone().with_size(dest.size()).into(),
                            dest,
                            signed,
                        )?;
                    }
                    if let Some(r) = dest_r {
                        self.arch.free(r)?;
                    }
                    if let Some(r) = expr_r {
                        self.arch.free(r)?;
                    }
                }
            }
            BinOp::LessThan
            | BinOp::LessEqual
            | BinOp::GreaterThan
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => {
                if let Some(dest) = dest {
                    let size = self
                        .arch
                        .size(&Type::common_type(left_type, right_type), &self.scope);
                    let left = self.arch.alloc()?;
                    let right = self.arch.alloc()?;

                    self.expr(*expr.left, Some(&left.dest(size)), state)?;
                    self.expr(*expr.right, Some(&right.dest(size)), state)?;
                    self.arch.cmp(&left.dest(size), &right.source(size));
                    self.arch.setcc(dest, CmpOp::try_from(&expr.op)?);
                    self.arch.free(left)?;
                    self.arch.free(right)?;
                }
            }
            BinOp::Shl => {
                if let Some(dest) = dest {
                    let r = self.arch.alloc()?;

                    self.expr(*expr.left, Some(dest), state)?;
                    self.expr(*expr.right, Some(&r.dest(size)), state)?;

                    self.arch.shl(
                        dest,
                        &Source::Register(operands::Register { register: r, size }),
                    )?;

                    self.arch.free(r)?;
                }
            }
            BinOp::Shr => {
                if let Some(dest) = dest {
                    let r = self.arch.alloc()?;

                    self.expr(*expr.left, Some(dest), state)?;
                    self.expr(*expr.right, Some(&r.dest(size)), state)?;

                    self.arch.shr(
                        dest,
                        &Source::Register(operands::Register { register: r, size }),
                    )?;

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
                                    codegen.expr(expr, Some(dest), None)?;
                                }
                                _ => {
                                    codegen.expr(expr, Some(dest), Some(&state))?;
                                }
                            }

                            if cmp {
                                codegen
                                    .arch
                                    .cmp(dest, &Source::Immediate(Immediate::UInt(0)));

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
                                .mov(&Source::Immediate(Immediate::UInt(1)), dest, false)?;
                            self.arch
                                .jcc(state.end_label.as_ref().unwrap(), Jump::Unconditional);
                            self.arch.write_label(state.false_label.as_ref().unwrap());
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), dest, false)?;
                        } else {
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(0)), dest, false)?;
                            self.arch
                                .jcc(state.end_label.as_ref().unwrap(), Jump::Unconditional);
                            self.arch.write_label(state.false_label.as_ref().unwrap());
                            self.arch
                                .mov(&Source::Immediate(Immediate::UInt(1)), dest, false)?;
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
            Stmt::Continue => {
                if let Some((start, _)) = self.loop_scope_info() {
                    self.arch.jcc(&start, Jump::Unconditional);

                    Ok(())
                } else {
                    unreachable!();
                }
            }
            Stmt::Break => {
                if let Some((_, end)) = self.loop_scope_info() {
                    self.arch.jcc(&end, Jump::Unconditional);

                    Ok(())
                } else {
                    unreachable!();
                }
            }
        }
    }

    fn unary_expr(
        &mut self,
        unary_expr: ExprUnary,
        dest: &Destination,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                self.expr(*unary_expr.expr, Some(dest), state)?;
                self.arch.negate(dest);
            }
            UnOp::LogicalNot => {
                let type_ = unary_expr.type_(&self.scope)?;
                let r = self.arch.alloc()?;

                self.expr(
                    *unary_expr.expr,
                    Some(&r.dest(self.arch.size(&type_, &self.scope))),
                    state,
                )?;
                self.arch
                    .not(&r.dest(self.arch.size(&type_, &self.scope)), dest);
                self.arch.free(r)?;
            }
            UnOp::Address => {
                let signed = unary_expr.expr.type_(&self.scope)?.signed();
                let expr_dest = self.expr_dest(*unary_expr.expr)?;
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
                    dest,
                    signed,
                )?;

                self.arch.free(r)?;
                self.free(expr_dest)?;
            }
            UnOp::Deref => {
                let signed = unary_expr.expr.type_(&self.scope)?.signed();
                let expr_dest = self.expr_dest(Expr::Unary(unary_expr))?;

                self.arch.mov(&expr_dest.clone().into(), dest, signed)?;
                self.free(expr_dest)?;
            }
            UnOp::BitwiseNot => {
                self.expr(*unary_expr.expr, Some(dest), state)?;

                self.arch.bitwise_not(dest);
            }
        };

        Ok(())
    }

    fn call_function(
        &mut self,
        call: ExprFunctionCall,
        dest: Option<&Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        let mut preceding = Vec::new();
        let mut stack_size = 0;
        let mut arg_registers = Vec::new();

        for expr in call.arguments.clone().into_iter() {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;

            self.expr(expr, Some(&r.dest(self.arch.word_size())), state)?;
            let arg = self.arch.push_arg(
                Source::Register(operands::Register {
                    register: r,
                    size: self.arch.word_size(),
                }),
                &type_,
                &preceding,
            );

            match arg {
                Argument::Stack(size) => stack_size += size,
                Argument::Register(r) => arg_registers.push(r),
            };

            self.arch.free(r)?;
            preceding.push(type_);
        }

        let (_, return_type) = match call.expr.type_(&self.scope)? {
            Type::Fn(params, return_type) => (params, return_type),
            _ => unreachable!(),
        };

        match *call.expr {
            Expr::Ident(expr)
                if self.scope.find_symbol(&expr.0).is_some_and(|symbol| {
                    if let Symbol::Function(_) = symbol {
                        true
                    } else {
                        false
                    }
                }) =>
            {
                self.arch.call(
                    &Source::Immediate(Immediate::Label(expr.0)),
                    dest,
                    return_type.signed(),
                    self.arch.size(&return_type, &self.scope),
                )?;
            }
            _ => {
                let r = self.arch.alloc()?;

                self.expr(*call.expr, Some(&r.dest(self.arch.word_size())), state)?;
                self.arch.call(
                    &r.source(self.arch.word_size()),
                    dest,
                    return_type.signed(),
                    self.arch.size(&return_type, &self.scope),
                )?;
                self.arch.free(r)?;
            }
        };

        for r in arg_registers {
            self.arch.free(r)?;
        }
        if stack_size > 0 {
            self.arch.shrink_stack(stack_size);
        }

        Ok(())
    }

    fn struct_expr(
        &mut self,
        expr: ExprStruct,
        dest: &Destination,
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
            let field_size = self
                .arch
                .size(&type_struct.get_field_type(&name).unwrap(), &self.scope);

            self.expr(
                expr,
                Some(&match dest.clone() {
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
        dest: &Destination,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        for (i, expr) in expr.0.into_iter().enumerate() {
            let size = self.arch.size(&expr.type_(&self.scope)?, &self.scope);
            let index = self.arch.alloc()?;
            let r = self.arch.alloc()?;
            let r_loc = r.dest(self.arch.word_size());

            self.arch.mov(
                &Source::Immediate(Immediate::UInt(i as u64)),
                &index.dest(self.arch.word_size()),
                false,
            )?;

            match dest {
                Destination::Memory(memory) => {
                    self.arch.lea(&r_loc, &memory.effective_address);
                }
                Destination::Register(_) => unreachable!(),
            }

            self.arch.array_offset(
                &r_loc.clone().into(),
                &index.source(self.arch.word_size()),
                size,
                &r_loc,
            )?;

            self.expr(
                expr,
                Some(&Destination::Memory(Memory {
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
        dest: &Destination,
    ) -> Result<(), CodeGenError> {
        let signed = expr.type_(&self.scope)?.signed();
        let expr_dest = self.expr_dest(Expr::StructAccess(expr))?;

        self.arch.mov(&expr_dest.clone().into(), dest, signed)?;
        self.free(expr_dest)
    }

    fn struct_call_method(
        &mut self,
        expr: ExprStructMethod,
        dest: Option<&Destination>,
        state: Option<&State>,
    ) -> Result<(), CodeGenError> {
        let struct_name = match expr.expr.type_(&self.scope)? {
            Type::Custom(name) => name,
            _ => unreachable!(),
        };
        let expr_dest = self.expr_dest(*expr.expr)?;
        let r = self.arch.alloc()?;
        let effective_address = match expr_dest.clone() {
            Destination::Memory(memory) => memory.effective_address,
            _ => unreachable!(),
        };

        self.arch
            .lea(&r.dest(self.arch.word_size()), &effective_address);

        let this = Type::Ptr(Box::new(Type::Custom(struct_name.clone())));
        let mut preceding = Vec::new();
        let mut stack_size = 0;
        let mut arg_registers = Vec::new();

        let arg = self
            .arch
            .push_arg(r.source(self.arch.word_size()), &this, &preceding);
        preceding.push(this);

        match arg {
            Argument::Stack(size) => stack_size += size,
            Argument::Register(r) => arg_registers.push(r),
        };

        for expr in expr.arguments.into_iter() {
            let type_ = expr.type_(&self.scope)?;
            let r = self.arch.alloc()?;

            self.expr(expr, Some(&r.dest(self.arch.word_size())), state)?;
            let arg = self.arch.push_arg(
                Source::Register(operands::Register {
                    register: r,
                    size: self.arch.word_size(),
                }),
                &type_,
                &preceding,
            );

            match arg {
                Argument::Stack(size) => stack_size += size,
                Argument::Register(r) => arg_registers.push(r),
            };

            self.arch.free(r)?;
            preceding.push(type_);
        }

        let method = match self.scope.find_type(&struct_name).unwrap() {
            tt::Type::Struct(type_) => type_.find_method(&expr.method).unwrap(),
        };
        self.arch.call(
            &Source::Immediate(Immediate::Label(format!("{struct_name}__{}", expr.method))),
            dest,
            method.return_type.signed(),
            self.arch.size(&method.return_type, &self.scope),
        )?;
        for r in arg_registers {
            self.arch.free(r)?;
        }
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
        dest: &Destination,
    ) -> Result<(), CodeGenError> {
        let signed = expr.type_(&self.scope)?.signed();
        let expr_dest = self.expr_dest(Expr::ArrayAccess(expr))?;

        self.arch.mov(&expr_dest.clone().into(), dest, signed)?;
        self.free(expr_dest)
    }

    fn free(&mut self, dest: Destination) -> Result<(), CodeGenError> {
        Ok(match dest {
            Destination::Memory(memory) => {
                match memory.effective_address.base {
                    Base::Register(register) => self.arch.free(register)?,
                    Base::Label(_) => (),
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

    fn function_scope_info(&self) -> Option<String> {
        self.scope_infos.iter().rev().find_map(|info| {
            if let ScopeInfo::Function { label } = info {
                Some(label.to_owned())
            } else {
                None
            }
        })
    }

    fn loop_scope_info(&self) -> Option<(String, String)> {
        self.scope_infos.iter().rev().find_map(|info| {
            if let ScopeInfo::Loop { start, end } = info {
                Some((start.to_owned(), end.to_owned()))
            } else {
                None
            }
        })
    }

    fn expr_dest(&mut self, expr: Expr) -> Result<Destination, CodeGenError> {
        match expr {
            Expr::Ident(expr) => Ok(self
                .scope
                .find_symbol(&expr.0)
                .ok_or(SymbolTableError::NotFound(expr.0.clone()))?
                .source(&self.arch, &self.scope)
                .unwrap()
                .try_into()
                .unwrap()),
            Expr::StructAccess(expr) => {
                let type_ = expr.type_(&self.scope)?;
                let (field_offset, mut field_size) = match expr.expr.type_(&self.scope)? {
                    Type::Custom(c) => {
                        match self.scope.find_type(&c).ok_or(TypeError::Nonexistent(c))? {
                            tt::Type::Struct(type_struct) => (
                                type_struct.offset(&self.arch, &expr.field, &self.scope)?,
                                self.arch.size(
                                    &type_struct.get_field_type(&expr.field).unwrap(),
                                    &self.scope,
                                ),
                            ),
                        }
                    }
                    type_ => panic!("{type_:?}"),
                };

                if let Type::Array(_) = expr.type_(&self.scope)? {
                    field_size = self.arch.word_size();
                }

                let dest = match self.expr_dest(*expr.expr)? {
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
                };

                //TODO: dis looks ugly, refactor it plz
                if let Type::Array(_) = type_ {
                    let r = self.arch.alloc()?;

                    self.arch
                        .lea(&r.dest(self.arch.word_size()), &dest.clone().into());
                    self.free(dest)?;

                    Ok(r.dest(self.arch.word_size()))
                } else {
                    Ok(dest)
                }
            }
            Expr::ArrayAccess(expr) => {
                let type_ = expr.type_(&self.scope)?;
                let base = self.expr_dest(*expr.expr)?;
                let index = self.arch.alloc()?;
                let r = self.arch.alloc()?;
                let r_loc = r.dest(self.arch.word_size());

                self.expr(
                    *expr.index.clone(),
                    Some(&index.dest(self.arch.word_size())),
                    None,
                )?;
                match &base {
                    Destination::Memory(memory) => {
                        self.arch.lea(&r_loc, &memory.effective_address);
                    }
                    Destination::Register(register) => {
                        self.arch
                            .mov(&Source::Register(register.to_owned()), &r_loc, false)?;
                    }
                }
                self.arch.array_offset(
                    &r_loc.clone().into(),
                    &index.source(self.arch.word_size()),
                    self.arch.size(&type_, &self.scope),
                    &r_loc,
                )?;

                self.free(base)?;
                self.arch.free(index)?;

                Ok(Destination::Memory(Memory {
                    effective_address: EffectiveAddress {
                        base: Base::Register(r),
                        index: None,
                        scale: None,
                        displacement: None,
                    },
                    size: self.arch.size(&type_, &self.scope),
                }))
            }
            Expr::Unary(expr) if expr.op == UnOp::Deref => {
                let type_ = expr.expr.type_(&self.scope)?;
                let r = self.arch.alloc()?;
                let dest = r.dest(self.arch.size(&type_, &self.scope));

                self.expr(*expr.expr, Some(&dest), None)?;

                Ok(Destination::Memory(Memory {
                    effective_address: EffectiveAddress {
                        base: Base::Register(r),
                        index: None,
                        scale: None,
                        displacement: None,
                    },
                    size: self.arch.size(&type_.inner()?, &self.scope),
                }))
            }
            _ => unreachable!("Can't get address of rvalue"),
        }
    }
}
