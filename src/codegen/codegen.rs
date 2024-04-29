use crate::{
    parser::{BinOp, Expr, ExprLit, Stmt},
    symtable::SymbolTable,
};
use indoc::writedoc;
use std::{
    fmt::Display,
    fs::File,
    io::{BufWriter, Write},
};

enum CmpType {
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for CmpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            Self::LessThan => "setl",
            Self::GreaterThan => "setg",
            Self::LessEqual => "setle",
            Self::GreaterEqual => "setge",
            Self::Equal => "sete",
            Self::NotEqual => "setne",
        };

        f.write_str(s)
    }
}

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
    writer: BufWriter<File>,
    registers: [Register<'a>; 4],
}

impl<'a> CodeGen<'a> {
    pub fn new(path: &str, program: Vec<Stmt>, symtable: SymbolTable) -> Self {
        let file = File::create(path).unwrap();

        Self {
            program,
            symtable,
            writer: BufWriter::new(file),
            registers: [
                Register::new("r8"),
                Register::new("r9"),
                Register::new("r10"),
                Register::new("r11"),
            ],
        }
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

    fn compile(&mut self, stmt: Stmt) -> usize {
        match stmt {
            Stmt::Local(_) => {
                todo!();
            }
            Stmt::Return(stmt) => {
                let r = stmt.0.map(|expr| self.compile(Stmt::Expr(*expr)));
                self.ret(r);

                //TODO: i don't have to return value here, think about something more clever
                0
            }
            Stmt::Expr(expr) => match expr {
                Expr::Ident(ident) => {
                    todo!();
                }
                Expr::Lit(literal) => match literal {
                    ExprLit::Int(mut int_repr) => self.loadint(int_repr.i64()),
                    ExprLit::Int(val) => todo!(),
                    ExprLit::Float(val) => {
                        todo!();
                    }
                    ExprLit::Bool(val) => {
                        todo!();
                    }
                    ExprLit::Str(val) => {
                        todo!();
                    }
                },
                Expr::Unary(unary) => {
                    todo!();
                }
                Expr::Binary(expr) => {
                    let left = self.compile(Stmt::Expr(*expr.left().to_owned().unwrap()));
                    let right = self.compile(Stmt::Expr(*expr.right().to_owned().unwrap()));

                    match *expr.op() {
                        BinOp::Add => self.add(left, right),
                        BinOp::Sub => self.sub(left, right),
                        BinOp::Mul => self.mul(left, right),
                        BinOp::Div => self.div(left, right),
                        BinOp::Equal => self.equal(left, right),
                        BinOp::NotEqual => self.not_equal(left, right),
                        BinOp::LessThan => self.less_than(left, right),
                        BinOp::GreaterThan => self.greater_than(left, right),
                        BinOp::LessEqual => self.less_equal(left, right),
                        BinOp::GreaterEqual => self.greater_equal(left, right),
                    }
                }
            },
        }
    }

    pub fn generate(&mut self) {
        self.preabmble();
        for stmt in self.program.clone() {
            let r = self.compile(stmt.clone());

            //remove this and clone above later
            if let Stmt::Expr(_) = stmt {
                self.printint(r);
            }
        }
    }

    fn preabmble(&mut self) {
        writedoc!(
            self.writer,
            "
            section .data
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
        .unwrap();
    }

    fn ret(&mut self, r: Option<usize>) {
        match r {
            Some(r) => {
                writedoc!(
                    self.writer,
                    "
                    \tmov rax, {}
                    \tret
                    ",
                    self.registers[r].name
                )
                .unwrap();
            }
            None => {
                writedoc!(
                    self.writer,
                    "
                    \tret
                    ",
                )
                .unwrap();
            }
        }
    }

    fn load(&mut self, name: String) -> usize {
        let r = self.alloc_register();

        writedoc!(
            self.writer,
            "
            \tmov {}, [{name}]
            ",
            self.registers[r].name
        )
        .unwrap();

        r
    }

    fn loadint(&mut self, value: i64) -> usize {
        let r = self.alloc_register();

        writedoc!(
            self.writer,
            "
            \tmov {}, {value}
            ",
            self.registers[r].name
        )
        .unwrap();

        r
    }

    fn generate_variable(&mut self, name: String) -> usize {
        writedoc!(
            self.writer,
            "
            \tcommon {name} 8:8
            "
        )
        .unwrap();
        self.free_all_registers();

        //this function should return nothing :|
        69420
    }

    fn store(&mut self, r: usize, name: String) -> usize {
        writedoc!(
            self.writer,
            "
            \tmov [{name}], {}
            ",
            self.registers[r].name
        )
        .unwrap();

        r
    }

    fn add(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.writer,
            "
            \tadd {}, {}
            ",
            self.registers[r1].name,
            self.registers[r2].name
        )
        .unwrap();
        self.free_register(r2);

        r1
    }

    fn sub(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.writer,
            "
            \tsub {}, {}
            ",
            self.registers[r1].name,
            self.registers[r2].name
        )
        .unwrap();
        self.free_register(r2);

        r1
    }

    fn mul(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.writer,
            "
            \tmov rax, {}
            \tmul {}
            \tmov {}, rax
            ",
            self.registers[r1].name,
            self.registers[r2].name,
            self.registers[r1].name,
        )
        .unwrap();
        self.free_register(r2);

        r1
    }

    fn div(&mut self, r1: usize, r2: usize) -> usize {
        writedoc!(
            self.writer,
            "
            \tmov rax, {}
            \tidiv {}
            \tmov {}, rax
             ",
            self.registers[r1].name,
            self.registers[r2].name,
            self.registers[r1].name,
        )
        .unwrap();
        self.free_register(r2);

        r1
    }

    fn printint(&mut self, r: usize) {
        writedoc!(
            self.writer,
            "
            \tmov rdi, {}
            \tcall printint
             ",
            self.registers[r].name
        )
        .unwrap();
    }

    fn compare(&mut self, r1: usize, r2: usize, cmp_type: CmpType) -> usize {
        writedoc!(
            self.writer,
            "
            \tcmp {}, {}
            \t{cmp_type} {}b
             ",
            self.registers[r1].name,
            self.registers[r2].name,
            self.registers[r2].name
        )
        .unwrap();
        self.free_register(r1);

        r2
    }

    fn equal(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::Equal)
    }

    fn not_equal(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::NotEqual)
    }

    fn less_than(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::LessThan)
    }

    fn greater_than(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::GreaterThan)
    }

    fn less_equal(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::LessEqual)
    }

    fn greater_equal(&mut self, r1: usize, r2: usize) -> usize {
        self.compare(r1, r2, CmpType::GreaterEqual)
    }
}
