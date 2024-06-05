use crate::{
    archs::{Architecture, LoadItem},
    parser::{BinOp, Expr, ExprBinary, ExprUnary, Stmt, StmtVarDecl, UnOp},
    register_allocator::{Register, RegisterAllocator},
    symtable::SymbolTable,
};
use indoc::formatdoc;
use std::fs::File;
use std::io::Write;

pub struct CodeGen<Arch: Architecture> {
    symtable: SymbolTable,
    registers: RegisterAllocator,
    arch: Arch,
    data_section: String,
    text_section: String,
    bss_section: String,
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

                main:
                "
            ),
        }
    }

    fn declare(&mut self, var: &StmtVarDecl) {
        self.bss_section.push_str(&self.arch.declare(&var));
    }

    fn expr(&mut self, expr: &Expr) -> Register {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => self.load(LoadItem::Lit(lit.clone())),
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expr::Ident(ident) => self.load(LoadItem::Symbol(
                self.symtable.find(ident).unwrap().to_owned(),
            )),
            _ => panic!("nono"),
        }
    }

    fn bin_expr(&mut self, expr: &ExprBinary) -> Register {
        match &expr.op {
            BinOp::Assign => {
                let left = expr.left.as_ref();

                if let Expr::Ident(name) = left {
                    assert!(self.symtable.exists(name));

                    let right = self.expr(expr.right.as_ref());
                    self.mov(name, &right);

                    right
                } else {
                    panic!("Cant assign to non ident");
                }
            }
            BinOp::Add => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.add(&left, right);

                left
            }
            BinOp::Sub => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.sub(&left, right);

                left
            }
            BinOp::Mul => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.mul(&left, right);

                left
            }
            BinOp::Div => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.div(&left, right);

                left
            }
            _ => panic!("lasjdf"),
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => _ = self.expr(expr),
            Stmt::VarDecl(var_decl) => {
                self.declare(var_decl);
            }
        }
    }

    fn mov(&mut self, label: &str, r: &Register) {
        self.text_section.push_str(&self.arch.mov(label, r));
    }

    fn load(&mut self, item: LoadItem) -> Register {
        let r = self.registers.alloc().unwrap();
        self.text_section.push_str(&self.arch.load(&r, item));

        r
    }

    fn unary_expr(&mut self, unary_expr: &ExprUnary) -> Register {
        match unary_expr.op {
            UnOp::Negative => {
                let r = self.expr(unary_expr.expr.as_ref());
                self.negate(&r);

                r
            }
            _ => panic!("not unary operator is not supported yet"),
        }
    }

    fn negate(&mut self, r: &Register) {
        self.text_section.push_str(&self.arch.negate(r));
    }

    fn add(&mut self, r1: &Register, r2: Register) {
        self.text_section.push_str(&self.arch.add(r1, &r2));
        self.registers.free(r2).unwrap();
    }

    fn sub(&mut self, r1: &Register, r2: Register) {
        self.text_section.push_str(&self.arch.sub(r1, &r2));
        self.registers.free(r2).unwrap();
    }

    fn mul(&mut self, r1: &Register, r2: Register) {
        self.text_section.push_str(&self.arch.mul(r1, &r2));
        self.registers.free(r2).unwrap();
    }

    fn div(&mut self, r1: &Register, r2: Register) {
        self.text_section.push_str(&self.arch.div(r1, &r2));
        self.registers.free(r2).unwrap();
    }

    pub fn compile(&mut self, program: Vec<Stmt>, path: &str) {
        let mut file = File::create(path).expect(&format!("Failed to open a file {}", path));

        for stmt in program {
            self.stmt(&stmt);
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
        file.write_all("\tret".as_bytes()).unwrap();
    }
}
