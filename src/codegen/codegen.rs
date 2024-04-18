use crate::{
    parser::{ASTNode, ASTNodeType},
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

        return f.write_str(s);
    }
}

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
    program: Vec<ASTNode>,
    symtable: SymbolTable,
    writer: BufWriter<File>,
    registers: [Register<'a>; 4],
}

impl<'a> CodeGen<'a> {
    pub fn new(path: &str, program: Vec<ASTNode>, symtable: SymbolTable) -> Self {
        let file = File::create(path).unwrap();

        return Self {
            program,
            symtable,
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

    fn compile(&mut self, node: ASTNode, reg: Option<usize>) -> usize {
        let mut left: usize = 0;
        let mut right: usize = 0;

        if node.left.is_some() {
            left = self.compile(*node.left.unwrap(), None);
        }

        if node.right.is_some() {
            right = self.compile(*node.right.unwrap(), Some(left));
        }

        return match node.op {
            ASTNodeType::Add => self.add(left, right),
            ASTNodeType::Sub => self.sub(left, right),
            ASTNodeType::Mult => self.mul(left, right),
            ASTNodeType::Div => self.div(left, right),
            ASTNodeType::Equal => self.equal(left, right),
            ASTNodeType::NotEqual => self.not_equal(left, right),
            ASTNodeType::LessThan => self.less_than(left, right),
            ASTNodeType::GreaterThan => self.greater_than(left, right),
            ASTNodeType::LessEqual => self.less_equal(left, right),
            ASTNodeType::GreaterEqual => self.greater_equal(left, right),
            ASTNodeType::LvIdent(id) => self.store(reg.unwrap(), self.symtable.get(id).unwrap()),
            ASTNodeType::Ident(id) => self.load(self.symtable.get(id).unwrap()),
            ASTNodeType::IntLit(int) => self.loadint(int.parse().unwrap()),
            ASTNodeType::Assign => right,
            ASTNodeType::DeclareVariable(var_name) => self.generate_variable(var_name),
        };
    }

    pub fn generate(&mut self) {
        self.preabmble();
        for node in self.program.clone() {
            let res = self.compile(node, None);
            if res != 69420 {
                self.printint(res);
            }
        }
        self.postabmble();
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

    fn postabmble(&mut self) {
        writedoc!(
            self.writer,
            "
            \tmov rax, 60
            \tmov rsi, 0
            \tsyscall
            "
        )
        .unwrap();
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

        return r;
    }

    fn loadint(&mut self, value: i32) -> usize {
        let r = self.alloc_register();

        writedoc!(
            self.writer,
            "
            \tmov {}, {value}
            ",
            self.registers[r].name
        )
        .unwrap();

        return r;
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
        return 69420;
    }

    fn store(&mut self, reg: usize, name: String) -> usize {
        writedoc!(
            self.writer,
            "
            \tmov [{name}], {}
            ",
            self.registers[reg].name
        )
        .unwrap();

        return reg;
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

        return r1;
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

        return r1;
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

        return r1;
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

        return r1;
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

        return r2;
    }

    fn equal(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::Equal);
    }

    fn not_equal(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::NotEqual);
    }

    fn less_than(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::LessThan);
    }

    fn greater_than(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::GreaterThan);
    }

    fn less_equal(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::LessEqual);
    }

    fn greater_equal(&mut self, r1: usize, r2: usize) -> usize {
        return self.compare(r1, r2, CmpType::GreaterEqual);
    }
}
