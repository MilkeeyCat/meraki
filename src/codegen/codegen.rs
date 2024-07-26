use crate::{
    archs::{Architecture, MoveDestination, MoveSource},
    parser::{
        BinOp, CmpOp, Expr, ExprBinary, ExprFunctionCall, ExprLit, ExprStruct, ExprUnary,
        Expression, OpParseError, Stmt, StmtFunction, StmtReturn, StmtVarDecl, UnOp,
    },
    register_allocator::{AllocatorError, Register},
    scope::Scope,
    symbol_table::Symbol,
    type_::TypeError,
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

impl<'a> From<&'a Register> for MoveDestination<'a> {
    fn from(value: &'a Register) -> Self {
        Self::Register(value)
    }
}

impl<'a> From<&'a Symbol> for MoveSource<'a> {
    fn from(value: &'a Symbol) -> Self {
        match value {
            Symbol::Local(symbol) => Self::Local(symbol.offset, symbol.type_.clone()),
            Symbol::Global(symbol) => Self::Global(&symbol.name, symbol.type_.clone()),
            Symbol::Param(symbol) => Self::Param(symbol.n, symbol.type_.clone()),
            Symbol::Function(_) => unreachable!(),
        }
    }
}

impl<'a> From<&'a Symbol> for MoveDestination<'a> {
    fn from(value: &'a Symbol) -> Self {
        match value {
            Symbol::Local(symbol) => Self::Local(symbol.offset),
            Symbol::Global(symbol) => Self::Global(&symbol.name),
            Symbol::Param(symbol) => {
                todo!();
            }
            Symbol::Function(_) => unreachable!(),
        }
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
            self.arch
                .declare(&variable.name, variable.type_.size::<Arch>(&self.scope))
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
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr, dest),
            Expr::Lit(lit) => Ok(self.arch.mov(
                MoveSource::Lit(lit),
                dest.expect("Destination was not provided"),
                &self.scope,
            )),
            Expr::Unary(unary_expr) => {
                self.unary_expr(unary_expr, dest.expect("Destination was not provided"))
            }
            Expr::Ident(ident) => {
                let symbol = self.scope.find_symbol(&ident).unwrap();

                self.arch.mov(
                    symbol.into(),
                    dest.expect("Destination was not provided"),
                    &self.scope,
                );

                Ok(())
            }
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let type_size = cast_expr.type_(&self.scope)?.size::<Arch>(&self.scope);
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

                Ok(())
            }
            Expr::FunctionCall(func_call) => self.call_function(func_call),
            Expr::Struct(expr) => {
                self.struct_expr(expr, dest.expect("Destination was not provided"))
            }
        }
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
                    //NOTE: overwrite dest for assigning because it's expression statement and this
                    //son of the bitch is of type `Void`
                    let symbol = self.scope.find_symbol(&name).unwrap().clone();
                    let dest: MoveDestination = (&symbol).into();

                    self.expr(*expr.right, Some(dest))?;
                } else {
                    return Err(CodeGenError::Assign(left));
                }
            }
            BinOp::Add => {
                self.expr(*expr.left, dest.clone())?;

                let r = self.arch.alloc()?;
                self.expr(*expr.right, Some((&r).into()))?;

                self.arch.add(dest.unwrap().register().unwrap(), &r);
                self.arch.free(r)?;
            }
            BinOp::Sub => {
                self.expr(*expr.left, dest.clone())?;

                let r = self.arch.alloc()?;
                self.expr(*expr.right, Some((&r).into()))?;

                self.arch.sub(dest.unwrap().register().unwrap(), &r);
                self.arch.free(r)?;
            }
            BinOp::Mul => {
                self.expr(*expr.left, dest.clone())?;

                let r = self.arch.alloc()?;
                self.expr(*expr.right, Some((&r).into()))?;

                self.arch.mul(dest.unwrap().register().unwrap(), &r);
                self.arch.free(r)?;
            }
            BinOp::Div => {
                self.expr(*expr.left, dest.clone())?;

                let r = self.arch.alloc()?;
                self.expr(*expr.right, Some((&r).into()))?;

                self.arch.div(dest.unwrap().register().unwrap(), &r);
                self.arch.free(r)?;
            }
            BinOp::LessThan
            | BinOp::LessEqual
            | BinOp::GreaterThan
            | BinOp::GreaterEqual
            | BinOp::Equal
            | BinOp::NotEqual => {
                self.expr(*expr.left, dest.clone())?;

                let r = self.arch.alloc()?;
                self.expr(*expr.right, Some((&r).into()))?;

                self.arch.cmp(
                    dest.unwrap().register().unwrap(),
                    &r,
                    CmpOp::try_from(&expr.op)?,
                );
                self.arch.free(r)?;
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
                self.arch.negate(dest.register().unwrap());
            }
            UnOp::Not => {
                let r = self.arch.alloc()?;

                self.expr(*unary_expr.expr, Some((&r).into()))?;
                self.arch.not(&r, dest.register().unwrap());
                self.arch.free(r)?;
            }
        };

        Ok(())
    }

    fn call_function(&mut self, call: ExprFunctionCall) -> Result<(), CodeGenError> {
        if call.arguments.len() > 6 {
            panic!("Can't call function with more than 6 arguments");
        }

        for (i, argument) in call.arguments.into_iter().enumerate() {
            let r = self.arch.alloc()?;
            self.expr(argument, Some((&r).into()))?;

            //NOTE: should the register be freed?
            self.arch.move_function_argument(r, i);
        }

        let r = self.arch.alloc()?;

        self.arch.call_fn(&call.name, &r);

        //NOTE: data in r

        Ok(())
    }

    fn struct_expr(&mut self, expr: ExprStruct, dest: MoveDestination) -> Result<(), CodeGenError> {
        let type_struct = match self.scope.find_type(&expr.name).unwrap() {
            type_table::Type::Struct(type_) => type_,
            _ => panic!("Expected type to be struct"),
        }
        .clone();
        //NOTE: clone bad ^

        for (name, expr) in expr.fields.into_iter() {
            let offset = type_struct.offset::<Arch>(&name, &self.scope);
            self.expr(expr, Some(MoveDestination::Local(offset)))?;
        }

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
        let mut offset: usize = 0;

        for stmt in stmts {
            if let Stmt::VarDecl(var_decl) = stmt {
                if let Symbol::Local(local) = self.scope.find_symbol_mut(&var_decl.name).unwrap() {
                    local.offset = offset;
                    offset += var_decl.type_.size::<Arch>(&self.scope);
                }
            }
        }

        offset
    }
}
