use crate::{
    archs::{ArchError, Architecture},
    codegen::locations::{self, Global, Local, MoveDestination, MoveSource, Offset},
    parser::{CmpOp, ExprLit, Expression},
    register::{
        allocator::{AllocatorError, RegisterAllocator},
        Register,
    },
    scope::Scope,
    types::{Type, TypeError},
};
use indoc::formatdoc;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Hash)]
enum ParamClass {
    Integer,
}

impl From<&Type> for ParamClass {
    fn from(value: &Type) -> Self {
        match value {
            Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::U32
            | Type::I32
            | Type::U64
            | Type::I64
            | Type::Usize
            | Type::Isize
            | Type::Bool
            | Type::Ptr(_) => Self::Integer,
            _ => unreachable!("Unsupported parameter type"),
        }
    }
}

const MAX_REGISTER_SIZE: usize = 8;

#[derive(Clone)]
pub struct Amd64 {
    buf: String,
    registers: RegisterAllocator,
    rax: Register,
}

impl Architecture for Amd64 {
    fn new() -> Self {
        Self {
            buf: String::new(),
            rax: Register::new("al", "ax", "eax", "rax"),
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

    #[inline]
    fn alignment(&self) -> usize {
        16
    }

    fn align(&self, offset: usize, size: usize) -> usize {
        if offset % size == 0 {
            offset
        } else {
            offset + size - (offset % size)
        }
    }

    fn size(&self, type_: &Type) -> usize {
        match type_ {
            Type::Ptr(_) | Type::Usize | Type::Isize => MAX_REGISTER_SIZE,
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
            1 => "byte ptr",
            2 => "word ptr",
            4 => "dword ptr",
            8 => "qword ptr",
            _ => unreachable!(),
        }
    }

    fn mov(
        &mut self,
        src: MoveSource,
        dest: MoveDestination,
        scope: &Scope,
    ) -> Result<(), ArchError> {
        match src {
            MoveSource::Global(global, signed) => self.mov_global(global, dest, signed, scope),
            MoveSource::Local(local, signed) => self.mov_local(local, dest, signed, scope),
            MoveSource::Register(register, signed) => {
                self.mov_register(register, dest, signed, scope)
            }
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

    fn negate(&mut self, dest: &MoveDestination) {
        self.buf.push_str(&formatdoc!(
            "
            \tneg {dest}
            ",
        ));
    }

    fn not(&mut self, dest: &MoveDestination, dest2: &MoveDestination) {
        self.buf.push_str(&formatdoc!(
            "
            \tcmp {dest}, 0
            \tsete {dest2}
            ",
        ));
    }

    fn add(&mut self, dest: &MoveDestination, src: &MoveSource) {
        self.buf.push_str(&formatdoc!(
            "
            \tadd {dest}, {src}
            "
        ));
    }

    fn sub(&mut self, dest: &MoveDestination, src: &MoveSource) {
        self.buf.push_str(&formatdoc!(
            "
            \tsub {dest}, {src}
            ",
        ));
    }

    fn mul(&mut self, dest: &MoveDestination, src: &MoveSource) {
        self.mov_impl(
            (&dest, dest.size()),
            (self.rax.from_size(src.size()), src.size()),
            src.signed(),
        );
        self.buf.push_str(&formatdoc!(
            "
            \timul {src}
            ",
        ));
        self.mov_impl(
            (self.rax.from_size(src.size()), src.size()),
            (&dest, dest.size()),
            src.signed(),
        );
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&mut self, dest: &MoveDestination, src: &MoveSource) {
        self.mov_impl((&dest, dest.size()), ("rax", 8), src.signed());
        self.buf.push_str(&formatdoc!(
            "
            \tcqo
            \tidiv {src}
            ",
        ));
        self.mov_impl(
            (
                &locations::Register {
                    register: self.rax,
                    size: src.size(),
                    offset: None,
                },
                src.size(),
            ),
            (&dest, dest.size()),
            src.signed(),
        );
    }

    fn cmp(&mut self, dest: &MoveDestination, src: &MoveSource, cmp: CmpOp) {
        let ins = match cmp {
            CmpOp::LessThan => "setl",
            CmpOp::LessEqual => "setle",
            CmpOp::GreaterThan => "setg",
            CmpOp::GreaterEqual => "setge",
            CmpOp::Equal => "sete",
            CmpOp::NotEqual => "setne",
        };

        self.buf.push_str(&formatdoc!(
            "
            \tcmp {dest}, {src}
            \t{ins} {dest}
            ",
        ));
    }

    fn fn_preamble(&mut self, name: &str, params: &[Type], stackframe: usize, scope: &Scope) {
        self.buf.push_str(&formatdoc!(
            "
            .global {name}
            {name}:
                push rbp
                mov rbp, rsp
                sub rsp, {stackframe}
            ",
        ));

        let mut occurences: HashMap<ParamClass, usize> = HashMap::new();
        let mut offset = Offset(0);

        for type_ in params {
            let n = occurences.entry(ParamClass::from(type_)).or_insert(0);
            *n += 1;

            if n <= &mut 6 {
                let n = *n;
                offset = &offset - (MAX_REGISTER_SIZE as isize);

                self.mov(
                    MoveSource::Register(
                        locations::Register {
                            offset: None,
                            size: MAX_REGISTER_SIZE,
                            register: self.registers.get(self.registers.len() - n).unwrap(),
                        },
                        type_.signed(),
                    ),
                    MoveDestination::Local(Local {
                        size: MAX_REGISTER_SIZE,
                        offset: offset.clone(),
                    }),
                    scope,
                )
                .unwrap();
            }
        }
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

    fn ret(&mut self, src: MoveSource) -> Result<(), TypeError> {
        self.mov_impl((&src, src.size()), ("rax", 8), src.signed());

        Ok(())
    }

    fn jmp(&mut self, label: &str) {
        self.buf.push_str(&formatdoc!(
            "
            \tjmp {}
            ",
            label
        ));
    }

    fn call_fn(&mut self, name: &str, dest: Option<&MoveDestination>) {
        match dest {
            Some(dest) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tcall {name}
                    \tmov {dest}, rax
                    ",
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

    fn push_arg(&mut self, src: MoveSource, scope: &Scope, type_: &Type, preceding: &[Type]) {
        let mut occurences: HashMap<ParamClass, usize> = HashMap::new();
        let class = ParamClass::from(type_);

        preceding
            .iter()
            .for_each(|param| *occurences.entry(ParamClass::from(param)).or_insert(0) += 1);

        match class {
            ParamClass::Integer => match occurences.get(&class).unwrap_or(&0) {
                n if n <= &6 => self
                    .mov(
                        src,
                        MoveDestination::Register(locations::Register {
                            size: MAX_REGISTER_SIZE,
                            register: self.registers.get(self.registers.len() - n - 1).unwrap(),
                            offset: None,
                        }),
                        scope,
                    )
                    .unwrap(),
                _ => {
                    self.buf.push_str(&formatdoc!(
                        "
                        \tpush {src}
                        ",
                    ));
                }
            },
        }
    }

    fn lea(&mut self, dest: &Register, dest2: &MoveDestination) {
        match dest2 {
            MoveDestination::Local(local) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tlea {}, [rbp{}]
                    ",
                    dest.qword(),
                    local.offset,
                ));
            }
            MoveDestination::Global(global) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tlea {}, [{}{}]
                    ",
                    dest.qword(),
                    global.label,
                    (&global.offset).as_ref().unwrap_or(&Offset::default()),
                ));
            }
            MoveDestination::Register(register) => {
                self.buf.push_str(&formatdoc!(
                    "
                    \tlea {}, [{}{}]
                    ",
                    dest.qword(),
                    register.register.from_size(register.size),
                    (&register.offset).as_ref().unwrap_or(&Offset::default()),
                ));
            }
        }
    }

    fn param_dest(&self, _: &Scope, type_: &Type, preceding: &[Type]) -> MoveDestination {
        let mut occurences: HashMap<ParamClass, usize> = HashMap::new();
        let class = ParamClass::from(type_);
        preceding
            .iter()
            .for_each(|param| *occurences.entry(ParamClass::from(param)).or_insert(0) += 1);

        match class {
            ParamClass::Integer => match occurences.get(&class).unwrap_or(&0) {
                n if n <= &6 => MoveDestination::Register(locations::Register {
                    size: 8,
                    register: self.registers.get(self.registers.len() - n).unwrap(),
                    offset: None,
                }),
                n => MoveDestination::Local(locations::Local {
                    size: 8,
                    offset: Offset(((n - 6) * MAX_REGISTER_SIZE) as isize),
                }),
            },
        }
    }

    fn finish(&mut self) -> Vec<u8> {
        self.buf.insert_str(0, ".section .text\n");
        self.buf.as_bytes().to_vec()
    }
}

impl Amd64 {
    fn mov_impl<T, I>(
        &mut self,
        (src, src_size): (&T, usize),
        (dest, dest_size): (&I, usize),
        signed: bool,
    ) where
        T: std::fmt::Display + ?Sized,
        I: std::fmt::Display + ?Sized,
    {
        if dest_size > src_size {
            if signed {
                self.buf.push_str(&formatdoc!("\tmovsx {dest}, {src}\n",));
            } else {
                self.buf.push_str(&formatdoc!("\tmovzx {dest}, {src}\n",));
            }
        } else {
            self.buf.push_str(&formatdoc!("\tmov {dest}, {src}\n",));
        }
    }

    fn mov_literal(
        &mut self,
        literal: ExprLit,
        dest: MoveDestination,
        scope: &Scope,
    ) -> Result<(), ArchError> {
        self.mov_impl(
            (&literal, 8),
            (&dest, dest.size()),
            literal.type_(scope)?.signed(),
        );

        Ok(())
    }

    fn mov_local(
        &mut self,
        src: Local,
        dest: MoveDestination,
        signed: bool,
        scope: &Scope,
    ) -> Result<(), ArchError> {
        match dest {
            // NOTE: x86-64 doesn't support indirect to indirect addressing mode so we use tools we already have
            MoveDestination::Local(local) => {
                let size = src.size;
                let r = self.alloc()?;

                self.lea(&r, &MoveDestination::Local(src));

                for chunk_size in Self::size_iter(size) {
                    let r_tmp = self.alloc()?;

                    self.mov_register(
                        locations::Register {
                            size: chunk_size,
                            offset: Some(Offset((size - chunk_size).try_into().unwrap())),
                            register: r,
                        },
                        MoveDestination::Register(locations::Register {
                            size: chunk_size,
                            offset: None,
                            register: r_tmp,
                        }),
                        signed,
                        scope,
                    )?;
                    self.mov_register(
                        locations::Register {
                            size: chunk_size,
                            offset: None,
                            register: r_tmp,
                        },
                        MoveDestination::Local(Local {
                            offset: local.offset.clone(),
                            size: chunk_size,
                        }),
                        signed,
                        scope,
                    )?;

                    self.free(r_tmp)?;
                }

                self.free(r)?;
            }
            MoveDestination::Global(_) => {
                todo!();
            }
            MoveDestination::Register(register) => {
                self.mov_impl((&src, src.size), (&register, register.size), signed);
            }
        };

        Ok(())
    }

    fn mov_global(
        &self,
        _src: Global,
        _dest: MoveDestination,
        _signed: bool,
        _scope: &Scope,
    ) -> Result<(), ArchError> {
        todo!();
    }

    fn mov_register(
        &mut self,
        src: locations::Register,
        dest: MoveDestination,
        signed: bool,
        scope: &Scope,
    ) -> Result<(), ArchError> {
        match (&src.offset, &dest) {
            (Some(_), MoveDestination::Local(_)) | (Some(_), MoveDestination::Global(_)) => {
                let size = src.size;
                let r = self.alloc()?;

                self.mov(
                    MoveSource::Register(src.clone(), signed),
                    r.to_dest(size),
                    scope,
                )?;
                self.mov(r.to_dest(size).to_source(signed), dest, scope)?;

                self.free(r)?;
            }
            _ => self.mov_impl((&src, src.size), (&dest, dest.size()), signed),
        }

        Ok(())
    }

    /// Transform variable size into iterator or sizes which can be used for `mov`
    /// For example if you use value 11 you will have iterator wil values [8, 2, 1]
    fn size_iter(mut size: usize) -> impl Iterator<Item = usize> {
        let mut sizes = Vec::new();
        while size > 0 {
            let chunk_size = match size {
                8.. => 8,
                4..=7 => 4,
                2..=3 => 2,
                1 => 1,
                0 => unreachable!(),
            };

            sizes.push(chunk_size);
            size -= chunk_size;
        }

        sizes.into_iter()
    }
}

impl std::fmt::Display for locations::Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.offset {
            Some(offset) => {
                write!(
                    f,
                    "{} [{}{}]",
                    Amd64::size_name(self.size),
                    self.register.qword(),
                    offset
                )
            }
            None => {
                write!(f, "{}", self.register.from_size(self.size))
            }
        }
    }
}

impl std::fmt::Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [rbp{}]", Amd64::size_name(self.size), self.offset)
    }
}

impl std::fmt::Display for Global<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.offset {
            Some(offset) => {
                write!(
                    f,
                    "{} [{}{}]",
                    Amd64::size_name(self.size),
                    self.label,
                    offset
                )
            }
            None => {
                write!(f, "{} [{}]", Amd64::size_name(self.size), self.label)
            }
        }
    }
}

impl std::fmt::Display for MoveDestination<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Global(global) => write!(f, "{global}"),
            Self::Local(local) => write!(f, "{local}"),
            Self::Register(register) => write!(f, "{register}"),
        }
    }
}

impl std::fmt::Display for MoveSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Global(global, _) => write!(f, "{global}"),
            Self::Local(local, _) => write!(f, "{local}"),
            Self::Register(register, _) => write!(f, "{register}"),
            Self::Lit(lit) => write!(f, "{lit}"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Amd64;
    use crate::{
        archs::Architecture,
        codegen::locations::{self, MoveDestination, Offset},
        parser::{ExprLit, IntLitRepr, UIntLitRepr},
        register::Register,
        scope::Scope,
    };

    #[test]
    fn mov_literal() {
        let r = Register::new("r15b", "r15w", "r15d", "r15");
        let scope = Scope::new();
        let tests = vec![
            (
                (
                    MoveDestination::Global(locations::Global {
                        size: 4,
                        offset: Some(Offset(-5)),
                        label: "foo",
                    }),
                    ExprLit::UInt(UIntLitRepr::new(15_000)),
                ),
                "\tmov dword ptr [foo - 5], 15000\n",
            ),
            (
                (
                    MoveDestination::Global(locations::Global {
                        size: 8,
                        offset: None,
                        label: "foo",
                    }),
                    ExprLit::Int(IntLitRepr::new(-5)),
                ),
                "\tmov qword ptr [foo], -5\n",
            ),
            (
                (
                    MoveDestination::Local(locations::Local {
                        size: 4,
                        offset: Offset(-1),
                    }),
                    ExprLit::UInt(UIntLitRepr::new(5)),
                ),
                "\tmov dword ptr [rbp - 1], 5\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 8,
                        offset: None,
                        register: r,
                    }),
                    ExprLit::UInt(UIntLitRepr::new(5)),
                ),
                "\tmov r15, 5\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 8,
                        offset: Some(Offset(-15)),
                        register: r,
                    }),
                    ExprLit::UInt(UIntLitRepr::new(5)),
                ),
                "\tmov qword ptr [r15 - 15], 5\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 2,
                        offset: Some(Offset(8)),
                        register: r,
                    }),
                    ExprLit::Int(IntLitRepr::new(-7)),
                ),
                "\tmov word ptr [r15 + 8], -7\n",
            ),
        ];

        for ((dest, lit), expected) in tests {
            let mut arch = Amd64::new();
            arch.mov_literal(lit, dest, &scope).unwrap();

            assert_eq!(arch.buf, expected);
        }
    }

    #[test]
    fn mov_register() {
        let r = Register::new("r15b", "r15w", "r15d", "r15");
        let r2 = Register::new("r14b", "r14w", "r14d", "r14");
        let scope = Scope::new();
        let tests = vec![
            (
                (
                    MoveDestination::Global(locations::Global {
                        size: 4,
                        offset: Some(Offset(-5)),
                        label: "foo",
                    }),
                    locations::Register {
                        offset: None,
                        size: 4,
                        register: r,
                    },
                    false,
                ),
                "\tmov dword ptr [foo - 5], r15d\n",
            ),
            (
                (
                    MoveDestination::Global(locations::Global {
                        size: 8,
                        offset: None,
                        label: "foo",
                    }),
                    locations::Register {
                        offset: None,
                        size: 4,
                        register: r,
                    },
                    false,
                ),
                "\tmovzx qword ptr [foo], r15d\n",
            ),
            (
                (
                    MoveDestination::Global(locations::Global {
                        size: 8,
                        offset: None,
                        label: "foo",
                    }),
                    locations::Register {
                        offset: None,
                        size: 4,
                        register: r,
                    },
                    true,
                ),
                "\tmovsx qword ptr [foo], r15d\n",
            ),
            (
                (
                    MoveDestination::Local(locations::Local {
                        size: 1,
                        offset: Offset(-10),
                    }),
                    locations::Register {
                        offset: None,
                        size: 1,
                        register: r,
                    },
                    true,
                ),
                "\tmov byte ptr [rbp - 10], r15b\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 2,
                        offset: Some(Offset(10)),
                        register: r2,
                    }),
                    locations::Register {
                        offset: None,
                        size: 1,
                        register: r,
                    },
                    true,
                ),
                "\tmovsx word ptr [r14 + 10], r15b\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 8,
                        offset: None,
                        register: r2,
                    }),
                    locations::Register {
                        offset: None,
                        size: 4,
                        register: r,
                    },
                    false,
                ),
                "\tmovzx r14, r15d\n",
            ),
            (
                (
                    MoveDestination::Register(locations::Register {
                        size: 1,
                        offset: None,
                        register: r2,
                    }),
                    locations::Register {
                        offset: None,
                        size: 1,
                        register: r,
                    },
                    false,
                ),
                "\tmov r14b, r15b\n",
            ),
        ];

        for ((dest, r, signed), expected) in tests {
            let mut arch = Amd64::new();
            arch.mov_register(r, dest, signed, &scope).unwrap();

            assert_eq!(arch.buf, expected);
        }
    }
}
