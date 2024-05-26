use crate::{
    parser::{
        BinOp, Expr, ExprBinary, ExprLit, ExprUnary, IntLitRepr, Stmt, StmtVarDecl, Type, UnOp,
    },
    symtable::SymbolTable,
};
use indoc::{formatdoc, writedoc};
use std::{fmt::Write, fs::File};

struct Register<'a> {
    name: &'a str,
    in_use: bool,
}

impl<'a> Register<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            in_use: false,
        }
    }
}

pub struct CodeGen<'a> {
    symtable: SymbolTable,
    registers: [Register<'a>; 6],
    data_section: String,
    text_section: String,
    bss_section: String,
}

impl<'a> CodeGen<'a> {
    pub fn new(symtable: SymbolTable) -> Self {
        Self {
            symtable,
            registers: [
                Register::new("r8"),
                Register::new("r9"),
                Register::new("rdi"),
                Register::new("rsi"),
                Register::new("rdx"),
                Register::new("rcx"),
            ],
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

    fn alloc(&mut self) -> usize {
        for (i, r) in self.registers.iter_mut().enumerate() {
            if !r.in_use {
                r.in_use = true;

                return i;
            }
        }

        panic!("All registers were in use");
    }

    fn free(&mut self, r: usize) {
        self.registers[r].in_use = false;
    }

    fn declare(&mut self, var: &StmtVarDecl) {
        let size = self.size(&var.type_);

        writedoc!(
            self.bss_section,
            "
            \t{} resb {size}
            ",
            &var.name
        )
        .unwrap();
    }

    fn expr(&mut self, expr: &Expr) -> usize {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => match lit {
                ExprLit::Int(int_lit) => self.load(int_lit),
            },
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            _ => panic!("nono"),
        }
    }

    fn bin_expr(&mut self, expr: &ExprBinary) -> usize {
        match &expr.op {
            BinOp::Assign => {
                let left = expr.left.as_ref();

                if let Expr::Ident(name) = left {
                    assert!(self.symtable.exists(name));

                    let right = self.expr(expr.right.as_ref());

                    self.mov(name, right);

                    69
                } else {
                    panic!("Cant assign to non ident");
                }
            }
            BinOp::Add => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.add(left, right)
            }
            BinOp::Sub => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.sub(left, right)
            }
            BinOp::Mul => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.mul(left, right)
            }
            BinOp::Div => {
                let left = self.expr(expr.left.as_ref());
                let right = self.expr(expr.right.as_ref());

                self.div(left, right)
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

    fn size(&self, type_: &Type) -> usize {
        match type_ {
            Type::I8 | Type::U8 => 1,
        }
    }

    fn mov(&mut self, label: &str, r: usize) {
        writedoc!(
            self.text_section,
            "
            \tmov [{}], {}
            ",
            &label,
            &self.registers[r].name
        )
        .unwrap();
    }

    fn load(&mut self, int_lit: &IntLitRepr) -> usize {
        let r = self.alloc();

        writedoc!(
            self.text_section,
            "
            \tmov {}, {}
            ",
            &self.registers[r].name,
            int_lit.to_string(),
        )
        .unwrap();

        r
    }

    fn unary_expr(&mut self, unary_expr: &ExprUnary) -> usize {
        match unary_expr.op {
            UnOp::Negative => {
                let r = self.expr(unary_expr.expr.as_ref());
                self.negate(r);

                r
            }
            _ => panic!("not unary operator is not supported yet"),
        }
    }

    fn negate(&mut self, r: usize) {
        writedoc!(
            self.text_section,
            "
            \tneg {}
            ",
            &self.registers[r].name,
        )
        .unwrap();
    }

    fn add(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.text_section,
            "
            \tadd {}, {}
            ",
            &self.registers[r1].name,
            &self.registers[r2].name,
        )
        .unwrap();

        self.free(r2);

        r1
    }

    fn sub(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.text_section,
            "
            \tsub {}, {}
            ",
            &self.registers[r1].name,
            &self.registers[r2].name,
        )
        .unwrap();

        self.free(r2);

        r1
    }

    fn mul(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.text_section,
            "
            \timul {}, {}
            ",
            &self.registers[r1].name,
            &self.registers[r2].name,
        )
        .unwrap();

        self.free(r2);

        r1
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.text_section,
            "
            \tmov rax, {}
            \tcqo
            \tidiv {}
            \tmov {}, rax
            ",
            &self.registers[r1].name,
            &self.registers[r2].name,
            &self.registers[r1].name,
        )
        .unwrap();

        self.free(r2);

        r1
    }

    pub fn compile(&mut self, program: Vec<Stmt>, path: &str) {
        let mut file = File::create(path).expect(&format!("Failed to open a file {}", path));

        for stmt in program {
            self.stmt(&stmt);
        }

        use std::io::Write;

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
