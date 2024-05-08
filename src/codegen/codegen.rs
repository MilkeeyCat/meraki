use crate::{parser::Stmt, symtable::SymbolTable};
use std::{fs::File, io::Write};

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
    program: Vec<Stmt>,
    symtable: SymbolTable,
    registers: [Register<'a>; 6],
    data_section: String,
    text_section: String,
}

impl<'a> CodeGen<'a> {
    pub fn new(program: Vec<Stmt>, symtable: SymbolTable) -> Self {
        Self {
            program,
            symtable,
            registers: [
                Register::new("rdi"),
                Register::new("rsi"),
                Register::new("rdx"),
                Register::new("rcx"),
                Register::new("r8"),
                Register::new("r9"),
            ],
            text_section: ".text:\n".to_string(),
            data_section: ".data:\n".to_string(),
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

    pub fn compile(self, path: &str) {
        let mut file = File::create(path).expect(&format!("Failed to open a file {}", path));

        file.write_all(&self.data_section.into_bytes())
            .expect("Failed to write generated .data section to output file");
        file.write_all(&self.text_section.into_bytes())
            .expect("Failed to write generated .text section to output file");
    }
}
