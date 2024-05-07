use crate::{
    parser::{BinOp, Expr, ExprLit, Stmt, StmtFunction, StmtVarDecl, Type},
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
    symtable: SymbolTable<'a>,
    writer: BufWriter<File>,
    registers: [Register<'a>; 4],

    func: Option<StmtFunction>,
}

impl<'a> CodeGen<'a> {
    pub fn new(path: &str, program: Vec<Stmt>, symtable: SymbolTable<'a>) -> Self {
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
            func: None,
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
            Stmt::VarDecl(stmt) => {
                self.generate_variable(stmt);

                69420
            }
            Stmt::Return(stmt) => {
                let r = stmt.0.map(|expr| self.compile(Stmt::Expr(*expr)));
                self.ret(r);

                //TODO: i don't have to return value here, think about something more clever
                0
            }
            Stmt::Function(func) => self.generate_function(func),
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
            "
        )
        .unwrap();
    }

    fn generate_function(&mut self, func: StmtFunction) -> usize {
        //TODO: remove clone
        self.func = Some(func.clone());

        writedoc!(
            self.writer,
            "

            {}:
                push rbp
                mov rbp, rsp
                ; sub rsp, $stack_size
            ",
            &func.name,
        )
        .unwrap();

        for stmt in func.stmts {
            self.compile(stmt);
        }

        writedoc!(
            self.writer,
            "
            {}_end:
                leave
                ret
            ",
            &func.name
        )
        .unwrap();

        0
    }

    fn ret(&mut self, r: Option<usize>) {
        match r {
            Some(r) => {
                writedoc!(
                    self.writer,
                    "
                    \tmov rax, {}
                    \tjmp {}_end
                    ",
                    self.registers[r].name,
                    self.func.as_ref().unwrap().name
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

    fn generate_variable(&mut self, var: StmtVarDecl) {
        match &var.value {
            Some(_value) => {
                todo!("init a variable with the value");
            }
            None => {
                writedoc!(
                    self.writer,
                    "
                    common {} {}
                    ",
                    &var.name,
                    self.type_size(&var.type_)
                )
                .unwrap();
            }
        }
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

    fn type_size(&self, type_: &Type) -> usize {
        //NOTE: this thingy is probably platform dependant ™
        use Type::*;

        match &type_ {
            I8 => 1,
            U8 => 1,
            Char => 1,
            I16 => 2,
            U16 => 2,
            I32 => 4,
            U32 => 4,
            I64 => 8,
            U64 => 8,
            Bool => 1,
            Ptr(_) => 8,
            Arr(arr) => arr.len * self.type_size(&arr.type_),
            Void => panic!("what do i do here????"),
        }
    }
}
