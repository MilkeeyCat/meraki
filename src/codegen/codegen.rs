use crate::{
    archs::{Architecture, LoadItem, SaveItem},
    parser::{
        BinOp, CmpOp, Expr, ExprBinary, ExprFunctionCall, ExprLit, ExprUnary, Expression,
        OpParseError, Stmt, StmtFunction, StmtReturn, StmtVarDecl, UnOp,
    },
    register_allocator::{AllocatorError, Register},
    scope::Scope,
    symbol_table::Symbol,
    type_::TypeError,
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

pub struct CodeGen<Arch: Architecture> {
    arch: Arch,
    scope: Scope,
}

impl<Arch: Architecture> CodeGen<Arch> {
    pub fn new(scope: Scope) -> Self {
        let arch = Arch::new();

        Self { arch, scope }
    }

    fn declare(&mut self, variable: StmtVarDecl) {
        if !self.scope.local() {
            self.arch.declare(variable)
        }
    }

    fn function(&mut self, mut func: StmtFunction) {
        self.scope.enter(*func.scope);
        let mut offset: usize = 0;

        for stmt in &mut func.body {
            if let Stmt::VarDecl(var_decl) = stmt {
                if let Symbol::Local(local) = self.scope.find_symbol_mut(&var_decl.name).unwrap() {
                    local.offset = offset;
                    offset += local.type_.size::<Arch>();
                }
            }
        }

        self.arch.fn_preamble(&func.name, offset);

        for stmt in func.body {
            self.stmt(stmt).unwrap();
        }

        self.arch.fn_postamble(&func.name, offset);
        self.scope.leave();
    }

    fn ret(&mut self, ret: StmtReturn) -> Result<(), CodeGenError> {
        let label = &ret.label;
        if let Some(expr) = ret.expr {
            let type_ = expr.type_(&self.scope)?;
            let r = self.expr(expr)?;

            self.arch.ret(&r, type_);
        }

        self.arch.jmp(label);

        Ok(())
    }

    fn expr(&mut self, expr: Expr) -> Result<Register, CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => Ok(self.arch.load(LoadItem::Lit(lit.clone()))?),
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expr::Ident(ident) => Ok(self.arch.load(LoadItem::Symbol(
                self.scope.find_symbol(&ident).unwrap().to_owned(),
            ))?),
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let type_size = cast_expr.type_(&self.scope)?.size::<Arch>();
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

                self.expr(expr)
            }
            Expr::FunctionCall(func_call) => self.call_function(func_call),
            _ => todo!(),
        }
    }

    fn bin_expr(&mut self, expr: ExprBinary) -> Result<Register, CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let left = expr.left.as_ref();

                if let Expr::Ident(name) = left {
                    let right = self.expr(*expr.right)?;

                    if !self.scope.local() {
                        self.arch
                            .save(SaveItem::Global(name), &right, left.type_(&self.scope)?);
                    } else {
                        let symbol = self.scope.find_symbol(name).unwrap();

                        match symbol {
                            Symbol::Local(symbol) => {
                                self.arch.save(
                                    SaveItem::Local(symbol.offset),
                                    &right,
                                    left.type_(&self.scope)?,
                                );
                            }
                            Symbol::Global(symbol) => {
                                self.arch.save(
                                    SaveItem::Global(&symbol.name),
                                    &right,
                                    left.type_(&self.scope)?,
                                );
                            }
                            Symbol::Param(_) => todo!("Can't assign value to param yet"),
                            Symbol::Function(_) => todo!("Can't assign function to a variable yet"),
                        }
                    }

                    Ok(right)
                } else {
                    Err(CodeGenError::Assign(left.to_owned()))
                }
            }
            BinOp::Add => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.arch.add(&left, right)?;

                Ok(left)
            }
            BinOp::Sub => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.arch.sub(&left, right)?;

                Ok(left)
            }
            BinOp::Mul => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.arch.mul(&left, right)?;

                Ok(left)
            }
            BinOp::Div => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.arch.div(&left, right)?;

                Ok(left)
            }
            BinOp::LessThan
            | BinOp::LessEqual
            | BinOp::GreaterThan
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.arch.cmp(&left, right, CmpOp::try_from(&expr.op)?)?;

                Ok(left)
            }
        }
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodeGenError> {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr).map(|_| ()),
            Stmt::VarDecl(var_decl) => Ok(self.declare(var_decl)),
            Stmt::Function(func) => Ok(self.function(func)),
            Stmt::Return(ret) => self.ret(ret),
        }
    }

    fn unary_expr(&mut self, unary_expr: ExprUnary) -> Result<Register, CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                let r = self.expr(*unary_expr.expr)?;
                self.arch.negate(&r);

                Ok(r)
            }
            UnOp::Not => {
                let r = self.expr(*unary_expr.expr)?;

                Ok(self.arch.not(r)?)
            }
        }
    }

    fn call_function(&mut self, call: ExprFunctionCall) -> Result<Register, CodeGenError> {
        if call.arguments.len() > 6 {
            panic!("Can't call function with more than 6 arguments");
        }

        for (i, argument) in call.arguments.into_iter().enumerate() {
            let r = self.expr(argument)?;

            self.arch.move_function_argument(r, i);
        }

        Ok(self.arch.call_fn(&call.name))
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
}
