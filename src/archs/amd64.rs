use super::arch::Architecture;
use crate::{
    codegen::locations::{
        DestinationLocal, DestinationRegister, MoveDestination, MoveSource, Offset, SourceGlobal,
        SourceLocal, SourceParam, SourceRegister,
    },
    parser::{CmpOp, ExprLit, Expression},
    register_allocator::{AllocatorError, Register, RegisterAllocator},
    scope::Scope,
    symbol_table::SymbolParam,
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

    fn size(&self, type_: &Type) -> usize {
        match type_ {
            _ => unreachable!(),
        }
    }

    fn alloc(&mut self) -> Result<Register, AllocatorError> {
        self.registers.alloc()
    }

    fn free(&mut self, register: Register) -> Result<(), AllocatorError> {
        self.registers.free(register)
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

    fn mov(&mut self, src: MoveSource, dest: MoveDestination, scope: &Scope) {
        match src {
            MoveSource::Global(global) => self.mov_global(global, dest, scope),
            MoveSource::Local(local) => self.mov_local(local, dest, scope),
            MoveSource::Param(param) => self.mov_param(param, dest, scope),
            MoveSource::Register(register) => self.mov_register(register, dest, scope),
            MoveSource::Lit(literal) => self.mov_literal(literal, dest, scope),
        }
    }

    fn declare(&mut self, name: &str, size: usize) {
        self.buf.push_str(&formatdoc!(
            "
            \t.comm {} {}
            ",
            name,
            size,
        ));
    }

    fn negate(&mut self, r: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \tneg {}
            ",
            r.qword(),
        ));
    }

    fn not(&mut self, r1: &Register, r2: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \tcmp {}, 0
            \tsete {}
            ",
            r1.qword(),
            r2.byte(),
        ));
    }

    fn add(&mut self, r1: &Register, r2: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \tadd {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
    }

    fn sub(&mut self, r1: &Register, r2: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \tsub {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
    }

    fn mul(&mut self, r1: &Register, r2: &Register) {
        self.buf.push_str(&formatdoc!(
            "
            \timul {}, {}
            ",
            r1.qword(),
            r2.qword(),
        ));
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&mut self, r1: &Register, r2: &Register) {
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
    }

    fn cmp(&mut self, r1: &Register, r2: &Register, cmp: CmpOp) {
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

    fn ret(&mut self, r: Register, type_: Type, scope: &Scope) {
        //NOTE: free the register?
        self.buf.push_str(&formatdoc!(
            "
            \t{} rax, {}
            ",
            Self::movx(type_.signed()),
            r.from_size(type_.size(self, scope)),
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

    fn call_fn(&mut self, name: &str, r: Option<&Register>) {
        match r {
            Some(r) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tcall {name}
                    \tmov {}, rax
                    ",
                    r.qword()
                ));
            }
            None => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tcall {name}
                    ",
                ));
            }
        }
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

    fn lea(&mut self, dest: &Register, offset: usize) {
        self.buf.push_str(&formatdoc!(
            "
            \tlea {}, [rbp - {}]
            ",
            dest.qword(),
            offset,
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

    fn movx(signed: bool) -> &'static str {
        if signed {
            "movsx"
        } else {
            "movzx"
        }
    }

    fn mov_literal(&mut self, literal: ExprLit, dest: MoveDestination, scope: &Scope) {
        match dest {
            MoveDestination::Local(local) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tmov {} ptr [rbp - {}], {}
                    ",
                    Self::size_name(literal.type_(scope).unwrap().size(self, scope)),
                    local.offset,
                    literal.to_string(),
                ));
            }
            MoveDestination::Global(global) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tmov {} ptr [{}], {}
                    ",
                    Self::size_name(literal.type_(scope).unwrap().size(self, scope)),
                    global.label,
                    literal.to_string(),
                ));
            }
            MoveDestination::Register(register) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tmov {}, {}
                    ",
                    register.register.qword(),
                    literal.to_string(),
                ));
            }
        }
    }

    fn mov_local(&mut self, src: SourceLocal, dest: MoveDestination, scope: &Scope) {
        match dest {
            // NOTE: x86-64 doesn't support indirect to indirect addressing mode so we use tools we already have
            MoveDestination::Local(local) => {
                let mut size = src.size;
                let r = self.alloc().unwrap();

                self.lea(&r, src.offset);

                while size > 0 {
                    let chunk_size = match size {
                        8.. => 8,
                        4..=7 => 4,
                        2..=3 => 2,
                        1 => 1,
                        0 => unreachable!(),
                    };

                    let r_tmp = self.alloc().unwrap();

                    self.mov_register(
                        SourceRegister {
                            size: chunk_size,
                            offset: Some(Offset((size - chunk_size).try_into().unwrap())),
                            signed: false,
                            register: &r,
                        },
                        MoveDestination::Register(DestinationRegister {
                            offset: None,
                            register: &r_tmp,
                        }),
                        scope,
                    );
                    self.mov_register(
                        SourceRegister {
                            size: chunk_size,
                            offset: None,
                            signed: false,
                            register: &r_tmp,
                        },
                        MoveDestination::Local(DestinationLocal {
                            offset: local.offset,
                        }),
                        scope,
                    );

                    size -= chunk_size;
                }

                self.free(r).unwrap();
            }
            MoveDestination::Global(label) => {
                // a lil bit of fuckery as well
                todo!();
            }
            MoveDestination::Register(register) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \t{} {}, {} ptr [rbp - {}]
                    ",
                    Self::movx(src.signed),
                    register.register.qword(),
                    Self::size_name(src.size),
                    src.offset,
                ));
            }
        }
    }

    fn mov_param(&mut self, src: SourceParam, dest: MoveDestination, scope: &Scope) {
        self.mov(
            MoveSource::Register(SourceRegister {
                register: &self
                    .registers
                    .get(self.registers.len() - 1 - src.n)
                    .unwrap(),
                size: src.size,
                offset: None,
                signed: src.signed,
            }),
            dest,
            scope,
        );
    }

    fn mov_global(&self, src: SourceGlobal, dest: MoveDestination, scope: &Scope) {
        todo!();
    }

    fn mov_register(&mut self, src: SourceRegister, dest: MoveDestination, scope: &Scope) {
        match dest {
            MoveDestination::Local(local) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tmov {} ptr [rbp - {}], {}
                    ",
                    Self::size_name(src.size),
                    local.offset,
                    src.register.from_size(src.size),
                ));
            }
            MoveDestination::Global(label) => {
                todo!();
            }
            MoveDestination::Register(dest_register) => {
                if let Some(offset) = src.offset {
                    self.buf.push_str(&formatdoc!(
                        "
                        \t{} {}, {} ptr [{}{}]
                        ",
                        Self::movx(src.signed),
                        dest_register.register.qword(),
                        Self::size_name(src.size),
                        src.register.qword(),
                        offset
                    ));

                    return;
                }

                self.buf.push_str(&formatdoc!(
                    "
                    \tmov {}, {}
                    ",
                    dest_register.register.qword(),
                    src.register.qword(),
                ));
            }
        }
    }
}
