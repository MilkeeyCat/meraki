use super::{
    arch::{Architecture, SaveItem},
    LoadItem,
};
use crate::{
    parser::{CmpOp, ExprLit, StmtVarDecl},
    register_allocator::{AllocatorError, Register, RegisterAllocator},
    symbol_table::{Symbol, SymbolParam},
    type_::Type,
};
use indoc::formatdoc;

pub struct Amd64 {
    buf: String,
    registers: RegisterAllocator,
}

impl Architecture for Amd64 {
    fn new() -> Self {
        Self {
            buf: String::from(".section .text\n"),
            registers: RegisterAllocator::new(vec![
                Register::new("r15b", "r15w", "r15d", "r15"),
                Register::new("r14b", "r14w", "r14d", "r14"),
                Register::new("r13b", "r13w", "r13d", "r13"),
                Register::new("r12b", "r12w", "r12d", "r12"),
                Register::new("r11b", "r11w", "r11d", "r11"),
                Register::new("r10b", "r10w", "r10d", "r10"),
                Register::new("r9b", "r9w", "r9d", "r9"),
                Register::new("r8b", "r8w", "r8d", "r8"),
                Register::new("cl", "cx", "ecx", "rcx"),
                Register::new("dl", "dx", "edx", "rdx"),
                Register::new("sil", "si", "esi", "rsi"),
                Register::new("dil", "di", "edi", "rdi"),
            ]),
        }
    }

    fn size(type_: &Type) -> usize {
        match type_ {
            _ => unreachable!(),
        }
    }

    fn size_name(size: usize) -> &'static str {
        match size {
            1 => "byte",
            2 => "word",
            4 => "dword",
            8 => "qword",
            _ => panic!("im done"),
        }
    }

    fn load(&mut self, item: LoadItem) -> Result<Register, AllocatorError> {
        let (ins, r) = match item {
            LoadItem::Lit(literal) => match literal {
                ExprLit::Int(integer) => {
                    let r = self.registers.alloc()?;
                    (
                        formatdoc!(
                            "
                        \tmov {}, {}
                        ",
                            r.qword(),
                            integer.to_string(),
                        ),
                        r,
                    )
                }
                ExprLit::UInt(integer) => {
                    let r = self.registers.alloc()?;
                    (
                        formatdoc!(
                            "
                        \tmov {}, {}
                        ",
                            r.qword(),
                            integer.to_string(),
                        ),
                        r,
                    )
                }
                ExprLit::Bool(value) => {
                    let r = self.registers.alloc()?;
                    (
                        formatdoc!(
                            "
                        \tmov {}, {}
                        ",
                            r.qword(),
                            value as u8
                        ),
                        r,
                    )
                }
            },
            LoadItem::Symbol(symbol) => match symbol {
                Symbol::Global(global) => {
                    let r = self.registers.alloc()?;
                    let ins = if global.type_.signed() {
                        "movsx"
                    } else {
                        "movzx"
                    };

                    (
                        formatdoc!(
                            "
                        \t{} {}, {} ptr [{}]
                        ",
                            ins,
                            r.qword(),
                            Self::size_name(Type::size::<Self>(&global.type_)),
                            global.name,
                        ),
                        r,
                    )
                }
                Symbol::Local(local) => {
                    let r = self.registers.alloc()?;
                    let ins = if local.type_.signed() {
                        "movsx"
                    } else {
                        "movzx"
                    };

                    (
                        formatdoc!(
                            "
                        \t{} {}, {} ptr [rbp - {}]
                        ",
                            ins,
                            r.qword(),
                            Self::size_name(Type::size::<Self>(&local.type_)),
                            local.offset,
                        ),
                        r,
                    )
                }
                Symbol::Param(param) => self.get_param(param),
                Symbol::Function(_) => todo!(),
            },
        };
        self.buf.push_str(&ins);

        Ok(r)
    }

    fn declare(&mut self, var: StmtVarDecl) {
        //\t{} resb {}
        self.buf.push_str(&formatdoc!(
            "
            \t.comm {} {}
            ",
            var.name,
            var.type_.size::<Self>(),
        ));
    }

    fn save(&mut self, item: SaveItem, r: &Register, type_: Type) {
        let ins = match item {
            SaveItem::Local(local) => {
                formatdoc!(
                    "
                    \tmov [rbp - {}], {}
                    ",
                    local,
                    r.from_size(type_.size::<Self>()),
                )
            }
            SaveItem::Global(global) => {
                formatdoc!(
                    "
                    \tmov [{}], {}
                    ",
                    global,
                    r.from_size(type_.size::<Self>()),
                )
            }
        };

        self.buf.push_str(&ins);
    }

    fn negate(&mut self, r: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \tneg {}
            ",
            r.qword(),
        ));
    }

    fn not(&mut self, r1: Register) -> Result<Register, AllocatorError> {
        let r2 = self.registers.alloc()?;

        self.buf.push_str(&formatdoc!(
            "
            \tcmp {}, 0
            \tsete {}
            ",
            r1.qword(),
            r2.byte(),
        ));
        self.registers.free(r1)?;

        Ok(r2)
    }

    fn add(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError> {
        self.buf.push_str(&formatdoc!(
            "
            \tadd {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
        self.registers.free(r2)?;

        Ok(())
    }

    fn sub(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError> {
        self.buf.push_str(&formatdoc!(
            "
            \tsub {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
        self.registers.free(r2)?;

        Ok(())
    }

    fn mul(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError> {
        self.buf.push_str(&formatdoc!(
            "
            \timul {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
        self.registers.free(r2)?;

        Ok(())
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError> {
        self.buf.push_str(&formatdoc!(
            "
            \tmov rax, {}
            \tcqo
            \tidiv {}
            \tmov {}, rax
            ",
            r1.qword(),
            r2.qword(),
            r1.qword(),
        ));
        self.registers.free(r2)?;

        Ok(())
    }

    fn cmp(&mut self, r1: &Register, r2: Register, cmp: CmpOp) -> Result<(), AllocatorError> {
        let ins = match cmp {
            CmpOp::LessThan => formatdoc!("setl {}", r1.byte()),
            CmpOp::LessEqual => formatdoc!("setle {}", r1.byte()),
            CmpOp::GreaterThan => formatdoc!("setg {}", r1.byte()),
            CmpOp::GreaterEqual => formatdoc!("setge {}", r1.byte()),
            CmpOp::Equal => formatdoc!("sete {}", r1.byte()),
            CmpOp::NotEqual => formatdoc!("setne {}", r1.byte()),
        };

        self.buf.push_str(&formatdoc!(
            "
           \tcmp {}, {}
           \t{}
           ",
            r1.qword(),
            r2.qword(),
            ins,
        ));

        self.registers.free(r2)
    }

    fn fn_preamble(&mut self, name: &str, stackframe: usize) {
        self.buf.push_str(&formatdoc!(
            "
            .global {name}
            {name}:
                push rbp
                mov rbp, rsp
                sub rsp, {stackframe}
            ",
        ));
    }

    fn fn_postamble(&mut self, name: &str, stackframe: usize) {
        self.buf.push_str(&formatdoc!(
            "
            {}_ret:
                add rsp, {}
                leave
                ret
            ",
            name,
            stackframe,
        ));
    }

    fn ret(&mut self, r: &Register, type_: Type) {
        let ins = if type_.signed() { "movsx" } else { "movzx" };

        self.buf.push_str(&formatdoc!(
            "
            \t{} rax, {}
            ",
            ins,
            r.from_size(Type::size::<Self>(&type_)),
        ));
    }

    fn jmp(&mut self, label: &str) {
        self.buf.push_str(&formatdoc!(
            "
            \tjmp {}
            ",
            label
        ));
    }

    fn call_fn(&mut self, name: &str) -> Register {
        let r = self.registers.alloc().unwrap();

        self.buf.push_str(&formatdoc!(
            "
            \tcall {name}
            \tmov {}, rax
            ",
            r.qword()
        ));

        r
    }

    fn move_function_argument(&mut self, r: Register, i: usize) {
        self.buf.push_str(&formatdoc!(
            "
            \tmov {}, {}
            ",
            self.registers
                .get(self.registers.len() - i - 1)
                .unwrap()
                .qword(),
            r.qword(),
        ));
    }

    fn finish(&self) -> Vec<u8> {
        self.buf.as_bytes().to_vec()
    }
}

impl Amd64 {
    pub fn get_param(&mut self, param: SymbolParam) -> (String, Register) {
        //NOTE: only 6 params are allowed in register, others go on stack
        if param.n < 6 {
            let r = self.registers.alloc().unwrap();
            (
                formatdoc!(
                    "
                    \tmov {}, {}
                    ",
                    r.qword(),
                    self.registers
                        .get(self.registers.len() - param.n - 1)
                        .unwrap()
                        .qword()
                ),
                r,
            )
        } else {
            todo!();
        }
    }
}
