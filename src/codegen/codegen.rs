use crate::{
    archs::{Architecture, LoadItem, SaveItem},
    parser::{
        BinOp, CmpOp, Expr, ExprBinary, ExprLit, ExprUnary, OpParseError, Stmt, StmtFunction,
        StmtVarDecl, Type, TypeError, UnOp,
    },
    register_allocator::{AllocatorError, Register, RegisterAllocator},
    scope::Scope,
    symtable::{Symbol, SymbolTable},
};
use indoc::formatdoc;
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
    registers: RegisterAllocator,
    arch: Arch,
    data_section: String,
    text_section: String,
    bss_section: String,
    scope: Scope,
}

impl<Arch: Architecture> CodeGen<Arch> {
    pub fn new(symtable: SymbolTable) -> Self {
        let (registers, arch) = Arch::new();

        Self {
            symtable,
            arch,
            registers: RegisterAllocator::new(registers),
            bss_section: "section .bss\n".to_string(),
            data_section: "section .data\n".to_string(),
            text_section: formatdoc!(
                "
                section .text
                    global main

                "
            ),
            scope: Scope::default(),
        }
    }

    fn declare(&mut self, var: StmtVarDecl) {
        if let Scope::Global = self.scope {
            self.bss_section.push_str(&self.arch.declare(var));
        }
    }

    fn function(&mut self, mut func: StmtFunction) {
        self.symtable.enter(Box::new(func.symtable));
        let mut offset: usize = 0;

        for stmt in &mut func.body {
            if let Stmt::VarDecl(var_decl) = stmt {
                if let Symbol::LocalVar(local) = self.symtable.find_mut(&var_decl.name).unwrap() {
                    local.offset = offset;
                    offset += local.type_.size::<Arch>();
                }
            }
        }

        self.text_section
            .push_str(&self.arch.fn_preamble(&func.name, offset));
        self.scope = Scope::Local;

        for stmt in func.body {
            self.stmt(stmt).unwrap();
        }

        self.scope = Scope::Global;
        self.symtable.leave();
    }

    fn expr(&mut self, expr: Expr) -> Result<Register, CodeGenError> {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => self.load(LoadItem::Lit(lit.clone())),
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expr::Ident(ident) => self.load(LoadItem::Symbol(
                self.symtable.find(&ident).unwrap().to_owned(),
            )),
            Expr::Cast(cast_expr) => {
                //TODO: move this elsewhere
                let expr = if let Expr::Lit(ExprLit::Int(mut int_lit)) = cast_expr.expr().clone() {
                    int_lit.resize(cast_expr.type_(&self.symtable)?.size::<Arch>());
                    Expr::Lit(ExprLit::Int(int_lit))
                } else {
                    cast_expr.expr().to_owned()
                };

                self.expr(expr)
            }
        }
    }

    fn bin_expr(&mut self, expr: ExprBinary) -> Result<Register, CodeGenError> {
        match &expr.op {
            BinOp::Assign => {
                let left = expr.left.as_ref();

                if let Expr::Ident(name) = left {
                    assert!(self.symtable.exists(name));
                    let right = self.expr(*expr.right)?;

                    if let Scope::Global = self.scope {
                        self.save(SaveItem::Global(name), &right, left.type_(&self.symtable)?);
                    } else {
                        let symbol = self.symtable.find(name).unwrap();
                        if let Symbol::LocalVar(symbol) = symbol {
                            self.save(
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

                self.add(&left, right)?;

                Ok(left)
            }
            BinOp::Sub => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.sub(&left, right)?;

                Ok(left)
            }
            BinOp::Mul => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.mul(&left, right)?;

                Ok(left)
            }
            BinOp::Div => {
                let left = self.expr(*expr.left)?;
                let right = self.expr(*expr.right)?;

                self.div(&left, right)?;

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

                self.cmp(&left, right, CmpOp::try_from(&expr.op)?)?;

                Ok(left)
            }
        }
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CodeGenError> {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr).map(|_| ()),
            Stmt::VarDecl(var_decl) => Ok(self.declare(var_decl)),
            Stmt::Function(func) => Ok(self.function(func)),
        }
    }

    fn save(&mut self, item: SaveItem, r: &Register, type_: Type) {
        self.text_section.push_str(&self.arch.save(item, r, type_));
    }

    fn load(&mut self, item: LoadItem) -> Result<Register, CodeGenError> {
        let r = self.registers.alloc()?;

        self.text_section.push_str(&self.arch.load(&r, item));

        Ok(r)
    }

    fn unary_expr(&mut self, unary_expr: ExprUnary) -> Result<Register, CodeGenError> {
        match unary_expr.op {
            UnOp::Negative => {
                let r = self.expr(*unary_expr.expr)?;
                self.negate(&r);

                Ok(r)
            }
            UnOp::Not => {
                let r = self.expr(*unary_expr.expr)?;

                self.not(r)
            }
        }
    }

    fn negate(&mut self, r: &Register) {
        self.text_section.push_str(&self.arch.negate(r));
    }

    fn not(&mut self, r2: Register) -> Result<Register, CodeGenError> {
        let r = self.registers.alloc()?;

        self.text_section.push_str(&self.arch.not(&r, &r2));
        self.registers.free(r2)?;

        Ok(r)
    }

    fn add(&mut self, r1: &Register, r2: Register) -> Result<(), CodeGenError> {
        self.text_section.push_str(&self.arch.add(r1, &r2));
        self.registers.free(r2)?;

        Ok(())
    }

    fn sub(&mut self, r1: &Register, r2: Register) -> Result<(), CodeGenError> {
        self.text_section.push_str(&self.arch.sub(r1, &r2));
        self.registers.free(r2)?;

        Ok(())
    }

    fn mul(&mut self, r1: &Register, r2: Register) -> Result<(), CodeGenError> {
        self.text_section.push_str(&self.arch.mul(r1, &r2));
        self.registers.free(r2)?;

        Ok(())
    }

    fn div(&mut self, r1: &Register, r2: Register) -> Result<(), CodeGenError> {
        self.text_section.push_str(&self.arch.div(r1, &r2));
        self.registers.free(r2)?;

        Ok(())
    }

    fn cmp(&mut self, r1: &Register, r2: Register, cmp: CmpOp) -> Result<(), CodeGenError> {
        self.text_section.push_str(&self.arch.cmp(r1, &r2, cmp));
        self.registers.free(r2)?;

        Ok(())
    }

    pub fn compile(&mut self, program: Vec<Stmt>, path: &str) -> Result<(), CodeGenError> {
        let mut file = File::create(path).expect(&format!("Failed to open a file {}", path));

        for stmt in program {
            self.stmt(stmt)?;
        }

        file.write_all(self.bss_section.as_bytes())
            .expect("Failed to write generated .bss section to output file");
        file.write(&[10]).unwrap();
        file.write_all(self.data_section.as_bytes())
            .expect("Failed to write generated .data section to output file");
        file.write(&[10]).unwrap();
        file.write_all(self.text_section.as_bytes())
            .expect("Failed to write generated .text section to output file");
        //TODO: remove this hack
        file.write_all("\tleave\n\tret".as_bytes()).unwrap();

        Ok(())
    }
}
