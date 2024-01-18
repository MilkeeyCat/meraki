use std::{
    fs::File,
    io::{BufWriter, Write},
};

use crate::parser::{ASTNode, ASTNodeType};

struct Register<'a> {
    name: &'a str,
    in_use: bool,
}

impl<'a> Register<'a> {
    pub fn new(name: &'a str) -> Self {
        return Self {
            name,
            in_use: false,
        };
    }
}

pub struct CodeGen<'a> {
    program: ASTNode,
    writer: BufWriter<File>,
    registers: [Register<'a>; 4],
}

impl<'a> CodeGen<'a> {
    pub fn new(path: &str, program: ASTNode) -> Self {
        let file = File::create(path).unwrap();

        return Self {
            program,
            writer: BufWriter::new(file),
            registers: [
                Register::new("r8"),
                Register::new("r9"),
                Register::new("r10"),
                Register::new("r11"),
            ],
        };
    }

    fn alloc_register(&mut self) -> usize {
        for (i, register) in self.registers.iter_mut().enumerate() {
            if register.in_use == false {
                register.in_use = true;

                return i;
            }
        }

        panic!("no registers for you lel");
    }

    fn free_all_registers(&mut self) {
        for register in self.registers.iter_mut() {
            register.in_use = false;
        }
    }

    fn free_register(&mut self, id: usize) {
        self.registers[id].in_use = false;
    }

    fn compile(&mut self, node: ASTNode) -> usize {
        let mut left: usize = 0;
        let mut right: usize = 0;

        if node.left.is_some() {
            left = self.compile(*node.left.unwrap());
        }

        if node.right.is_some() {
            right = self.compile(*node.right.unwrap());
        }

        return match node.op {
            ASTNodeType::Add => self.add(left, right),
            ASTNodeType::Sub => self.sub(left, right),
            ASTNodeType::Mult => self.mul(left, right),
            ASTNodeType::Div => self.div(left, right),
            ASTNodeType::IntLit(int) => self.load(int.parse().unwrap()),
        };
    }

    pub fn generate(&mut self) {
        self.preabmble();
        let res = self.compile(self.program.clone());
        self.printint(res);
        self.postabmble();
    }

    // functions to work with registers
    // dont look below...

    fn preabmble(&mut self) {
        self.writer
            .write_all(
                &format!(
                    "section .data
    fmt: db '%d', 10, 0

section .text
    extern printf
    extern fflush
    global main

printint:
    mov rsi, rdi
    mov rdi, fmt
    xor rax, rax

    call printf

    ret

main:
"
                )
                .into_bytes(),
            )
            .unwrap();
    }

    fn postabmble(&mut self) {
        self.writer
            .write_all(
                &format!(
                    "
    mov rax, 60
    mov rsi, 0
    syscall
"
                )
                .into_bytes(),
            )
            .unwrap();
    }

    fn load(&mut self, value: i32) -> usize {
        let r = self.alloc_register();

        self.writer
            .write_all(
                &format!(
                    "
    mov {}, {value}
",
                    self.registers[r].name
                )
                .into_bytes(),
            )
            .unwrap();

        return r;
    }

    fn add(&mut self, r1: usize, r2: usize) -> usize {
        self.writer
            .write_all(
                &format!(
                    "
    add {}, {}
",
                    self.registers[r1].name, self.registers[r2].name
                )
                .into_bytes(),
            )
            .unwrap();
        self.free_register(r2);

        return r1;
    }

    fn sub(&mut self, r1: usize, r2: usize) -> usize {
        self.writer
            .write_all(
                &format!(
                    "
    sub {}, {}
",
                    self.registers[r1].name, self.registers[r2].name
                )
                .into_bytes(),
            )
            .unwrap();
        self.free_register(r2);

        return r1;
    }

    fn mul(&mut self, r1: usize, r2: usize) -> usize {
        self.writer
            .write_all(
                &format!(
                    "
    mov rax, {}
    mul {}
    mov {}, rax
",
                    self.registers[r1].name, self.registers[r2].name, self.registers[r1].name,
                )
                .into_bytes(),
            )
            .unwrap();
        self.free_register(r2);

        return r1;
    }

    fn div(&mut self, r1: usize, r2: usize) -> usize {
        self.writer
            .write_all(
                &format!(
                    "
    mov rax, {}
    idiv {}
    mov {}, rax
",
                    self.registers[r1].name, self.registers[r2].name, self.registers[r1].name,
                )
                .into_bytes(),
            )
            .unwrap();
        self.free_register(r2);

        return r1;
    }

    fn printint(&mut self, r: usize) {
        self.writer
            .write_all(
                &format!(
                    "
    mov rdi, {}
    call printint
",
                    self.registers[r].name
                )
                .into_bytes(),
            )
            .unwrap();
    }
}
