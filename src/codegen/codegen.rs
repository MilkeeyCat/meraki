use super::{Register, RegisterAllocator};
use crate::{
    parser::{BinOp, Expr, ExprBinary, ExprLit, ExprUnary, Stmt, StmtVarDecl, Type, UnOp},
    symtable::{Symbol, SymbolTable},
};
use indoc::{formatdoc, writedoc};
use std::{fmt::Write, fs::File};

enum LoadItem {
    Lit(ExprLit),
    //Register(Register),
    Symbol(Symbol),
}

pub struct CodeGen {
    symtable: SymbolTable,
    registers: RegisterAllocator,
    data_section: String,
    text_section: String,
    bss_section: String,
}

impl CodeGen {
    pub fn new(symtable: SymbolTable) -> Self {
        Self {
            symtable,
            registers: RegisterAllocator::new(vec![
                Register::new("", "r8"),
                Register::new("", "r9"),
                Register::new("", "rdi"),
                Register::new("", "rsi"),
                Register::new("", "rdx"),
                Register::new("", "rcx"),
            ]),
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

    fn expr(&mut self, expr: &Expr) -> Register {
        match expr {
            Expr::Binary(bin_expr) => self.bin_expr(bin_expr),
            Expr::Lit(lit) => self.load(&LoadItem::Lit(lit.clone())),
            Expr::Unary(unary_expr) => self.unary_expr(unary_expr),
            Expr::Ident(ident) => self.load(&LoadItem::Symbol(
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

    fn size(&self, type_: &Type) -> usize {
        match type_ {
            Type::I8 | Type::U8 => 1,
        }
    }

    fn mov(&mut self, label: &str, r: &Register) {
        writedoc!(
            self.text_section,
            "
            \tmov [{}], {}
            ",
            &label,
            r.dword(),
        )
        .unwrap();
    }

    fn load(&mut self, item: &LoadItem) -> Register {
        let r = self.registers.alloc().unwrap();

        match item {
            LoadItem::Lit(literal) => match literal {
                ExprLit::Int(integer) => {
                    writedoc!(
                        self.text_section,
                        "
                        \tmov {}, {}
                        ",
                        r.dword(),
                        integer.to_string(),
                    )
                    .unwrap();
                }
            },
            LoadItem::Symbol(symbol) => match symbol {
                Symbol::GlobalVar(global_var) => {
                    writedoc!(
                        self.text_section,
                        "
                        \tmov {}, [{}]
                        ",
                        r.dword(),
                        global_var.name,
                    )
                    .unwrap();
                }
            },
        }

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
        writedoc!(
            self.text_section,
            "
            \tneg {}
            ",
            r.dword(),
        )
        .unwrap();
    }

    fn add(&mut self, r1: &Register, r2: Register) {
        writedoc!(
            self.text_section,
            "
            \tadd {}, {}
            ",
            r1.dword(),
            r2.dword(),
        )
        .unwrap();

        self.registers.free(r2).unwrap();
    }

    fn sub(&mut self, r1: &Register, r2: Register) {
        writedoc!(
            self.text_section,
            "
            \tsub {}, {}
            ",
            r1.dword(),
            r2.dword(),
        )
        .unwrap();

        self.registers.free(r2).unwrap();
    }

    fn mul(&mut self, r1: &Register, r2: Register) {
        writedoc!(
            self.text_section,
            "
            \timul {}, {}
            ",
            r1.dword(),
            r2.dword(),
        )
        .unwrap();

        self.registers.free(r2).unwrap();
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&mut self, r1: &Register, r2: Register) {
        writedoc!(
            self.text_section,
            "
            \tmov rax, {}
            \tcqo
            \tidiv {}
            \tmov {}, rax
            ",
            r1.dword(),
            r2.dword(),
            r1.dword(),
        )
        .unwrap();

        self.registers.free(r2).unwrap();
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
