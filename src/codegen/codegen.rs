use crate::{
    archs::{Architecture, LoadItem, SaveItem},
    parser::{
        BinOp, CmpOp, Expr, ExprBinary, ExprLit, ExprUnary, Expression, OpParseError, Stmt,
        StmtFunction, StmtReturn, StmtVarDecl, UnOp,
    },
    register_allocator::{AllocatorError, Register},
    scope::Scope,
    symbol_table::{Symbol, SymbolTable},
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
    symtable: SymbolTable,
    arch: Arch,
    scope: Scope,
}

impl<Arch: Architecture> CodeGen<Arch> {
    pub fn new(symtable: SymbolTable) -> Self {
        let arch = Arch::new();

        Self {
            symtable,
            arch,
            scope: Scope::default(),
        }
    }

    fn declare(&mut self, variable: StmtVarDecl) {
        if let Scope::Global = self.scope {
            self.arch.declare(variable)
        }
    }

    fn function(&mut self, mut func: StmtFunction) {
        self.symtable.enter(Box::new(func.symtable));
        let mut offset: usize = 0;

        for stmt in &mut func.body {
            if let Stmt::VarDecl(var_decl) = stmt {
                if let Symbol::Local(local) = self.symtable.find_mut(&var_decl.name).unwrap() {
                    local.offset = offset;
                    offset += local.type_.size::<Arch>();
                }
            }
        }

        self.arch.fn_preamble(&func.name, offset);
        self.scope = Scope::Local(func.name.clone(), func.return_type);

        for stmt in func.body {
            self.stmt(stmt).unwrap();
        }

        self.scope = Scope::Global;
        self.arch.fn_postamble(&func.name, offset);
        self.symtable.leave();
    }

    fn ret(&mut self, ret: StmtReturn) -> Result<(), CodeGenError> {
        let label = &ret.label;
        if let Some(expr) = ret.expr {
            let type_ = expr.type_(&self.symtable)?;
            let r = self.expr(expr)?;

            self.arch.ret(&r, type_);
        }

        self.jmp(label);

        Ok(())
    }

    fn jmp(&mut self, label: &str) {
        self.arch.jmp(label);
    }

    fn expr(&mut self, expr: Expr) -> Result<Register, CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => Ok(self.arch.load(LoadItem::Lit(lit.clone()))?),
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expr::Ident(ident) => Ok(self.arch.load(LoadItem::Symbol(
                self.symtable.find(&ident).unwrap().to_owned(),
            ))?),
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let type_size = cast_expr.type_(&self.symtable)?.size::<Arch>();
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
            _ => todo!(),
        }
    }

    fn bin_expr(&mut self, expr: ExprBinary) -> Result<Register, CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let left = expr.left.as_ref();

                if let Expr::Ident(name) = left {
                    let right = self.expr(*expr.right)?;

                    if let Scope::Global = self.scope {
                        self.arch
                            .save(SaveItem::Global(name), &right, left.type_(&self.symtable)?);
                    } else {
                        let symbol = self.symtable.find(name).unwrap();
                        if let Symbol::Local(symbol) = symbol {
                            self.arch.save(
                                SaveItem::Local(symbol.offset),
                                &right,
                                left.type_(&self.symtable)?,
                            );
                        } else {
                            panic!("FUCK");
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
