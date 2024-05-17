use crate::{
    parser::{Expr, Stmt, StmtVarDecl, Type},
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
                Register::new("rdi"),
                Register::new("rsi"),
                Register::new("rdx"),
                Register::new("rcx"),
                Register::new("r8"),
                Register::new("r9"),
            ],
            bss_section: "section .bss\n".to_string(),
            data_section: "section .data\n".to_string(),
            text_section: formatdoc!(
                "
                section .text
                    global main
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
        //todo!();
        0
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
    }
}
