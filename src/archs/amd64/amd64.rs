use crate::{
    archs::{Arch, ArchError, Architecture},
    codegen::{
        operands::{self, Base, EffectiveAddress, Immediate, Offset},
        Destination, Source,
    },
    parser::CmpOp,
    register::{
        allocator::{AllocatorError, RegisterAllocator},
        Register,
    },
    scope::Scope,
    symbol_table::{Symbol, SymbolTable},
    types::Type,
};
use indoc::formatdoc;
use std::collections::HashMap;

const WORD_SIZE: usize = 8;
const STACK_ALIGNMENT: usize = 16;

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
            | Type::Ptr(_)
            | Type::Array(_) => Self::Integer,
            _ => unreachable!("Unsupported parameter type"),
        }
    }
}

#[derive(Clone)]
pub struct Amd64 {
    buf: String,
    registers: RegisterAllocator,
    rax: Register,
    rbp: Register,
    literals: Vec<(String, String)>,
    label_counter: usize,
}

impl Architecture for Amd64 {
    fn new() -> Self {
        Self {
            buf: String::new(),
            rax: Register::new("al", "ax", "eax", "rax"),
            rbp: Register::new("there's no one byte one, hmmmm", "bp", "ebp", "rbp"),
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
            literals: Vec::new(),
            label_counter: 0,
        }
    }

    #[inline]
    fn word_size(&self) -> usize {
        WORD_SIZE
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
            Type::Ptr(_) | Type::Usize | Type::Isize => WORD_SIZE,
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

    fn mov(&mut self, src: &Source, dest: &Destination, signed: bool) -> Result<(), ArchError> {
        match (dest, src) {
            (Destination::Memory(dest), Source::Memory(src)) => {
                let size = src.size;
                let r = self.alloc()?;

                self.lea(
                    &Destination::Register(operands::Register {
                        register: r,
                        size: WORD_SIZE,
                    }),
                    &src,
                );

                for chunk_size in Self::size_iter(size) {
                    let r_tmp = self.alloc()?;

                    self.mov(
                        &Source::Memory(EffectiveAddress {
                            base: Base::Register(operands::Register {
                                register: r,
                                size: chunk_size,
                            }),
                            index: None,
                            scale: None,
                            displacement: Some(Offset((size - chunk_size).try_into().unwrap())),
                            size: chunk_size,
                        }),
                        &Destination::Register(operands::Register {
                            register: r_tmp,
                            size: chunk_size,
                        }),
                        false,
                    )?;
                    self.mov(
                        &Source::Register(operands::Register {
                            register: r_tmp,
                            size: chunk_size,
                        }),
                        &Destination::Memory(EffectiveAddress {
                            base: dest.base.clone(),
                            index: None,
                            scale: None,
                            displacement: None,
                            size: chunk_size,
                        }),
                        false,
                    )?;
                    self.free(r_tmp)?;
                }

                self.free(r)?;
            }
            (dest, src) => {
                let dest_size = dest.size();
                let src_size = src.size().unwrap_or(WORD_SIZE);

                if dest_size > src_size {
                    if signed {
                        self.buf.push_str(&formatdoc!("\tmovsx {dest}, {src}\n"));
                    } else {
                        self.buf.push_str(&formatdoc!("\tmovzx {dest}, {src}\n"));
                    }
                } else {
                    self.buf.push_str(&formatdoc!("\tmov {dest}, {src}\n"));
                }
            }
        };

        Ok(())
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

    fn negate(&mut self, dest: &Destination) {
        self.buf.push_str(&formatdoc!(
            "
            \tneg {dest}
            ",
        ));
    }

    fn not(&mut self, dest: &Destination, dest2: &Destination) {
        self.buf.push_str(&formatdoc!(
            "
            \tcmp {dest}, 0
            \tsete {dest2}
            ",
        ));
    }

    fn add(&mut self, dest: &Destination, src: &Source) {
        self.buf.push_str(&formatdoc!(
            "
            \tadd {dest}, {src}
            "
        ));
    }

    fn sub(&mut self, dest: &Destination, src: &Source) {
        self.buf.push_str(&formatdoc!(
            "
            \tsub {dest}, {src}
            ",
        ));
    }

    fn mul(&mut self, dest: &Destination, src: &Source, signed: bool) -> Result<(), ArchError> {
        let register = operands::Register {
            register: self.rax,
            size: src.size().unwrap(),
        };

        self.mov(src, &Destination::Register(register.clone()), signed)?;
        self.buf.push_str(&formatdoc!(
            "
            \timul {src}
            ",
        ));
        self.mov(&Source::Register(register), dest, signed)?;

        Ok(())
    }

    //NOTE: if mafs doesn't works, probably because of this xd
    fn div(&mut self, dest: &Destination, src: &Source, signed: bool) -> Result<(), ArchError> {
        let register = operands::Register {
            register: self.rax,
            size: WORD_SIZE,
        };

        self.mov(src, &Destination::Register(register.clone()), signed)?;
        self.buf.push_str(&formatdoc!(
            "
            \tcqo
            \tidiv {src}
            ",
        ));
        self.mov(&Source::Register(register.clone()), dest, signed)?;

        Ok(())
    }

    fn cmp(&mut self, dest: &Destination, src: &Source, cmp: CmpOp) {
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

    fn fn_preamble(
        &mut self,
        name: &str,
        params: &[Type],
        stackframe: usize,
        scope: &Scope,
    ) -> Result<(), ArchError> {
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

            match ParamClass::from(type_) {
                ParamClass::Integer => {
                    if *n <= 6 {
                        let n = *n;
                        let size = type_.size(&(Box::new(self.clone()) as Arch), scope)?;
                        offset = &offset - (size as isize);

                        self.mov(
                            &Source::Register(operands::Register {
                                register: self.registers.get(self.registers.len() - n).unwrap(),
                                size,
                            }),
                            &Destination::Memory(EffectiveAddress {
                                base: Base::Register(operands::Register {
                                    register: self.rbp,
                                    size,
                                }),
                                index: None,
                                scale: None,
                                displacement: Some(offset.clone()),
                                size,
                            }),
                            type_.signed(),
                        )?;
                    }
                }
            }
        }

        Ok(())
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

    fn ret(&mut self, src: &Source, signed: bool) -> Result<(), ArchError> {
        self.mov(
            &src,
            &Destination::Register(operands::Register {
                register: self.rax,
                size: WORD_SIZE,
            }),
            signed,
        )
    }

    fn jmp(&mut self, label: &str) {
        self.buf.push_str(&formatdoc!(
            "
            \tjmp {label}
            "
        ));
    }

    fn call_fn(&mut self, name: &str, dest: Option<&Destination>) {
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

    fn push_arg(&mut self, src: Source, type_: &Type, preceding: &[Type]) -> usize {
        let mut occurences: HashMap<ParamClass, usize> = HashMap::new();
        let class = ParamClass::from(type_);

        preceding
            .iter()
            .for_each(|param| *occurences.entry(ParamClass::from(param)).or_insert(0) += 1);

        match class {
            ParamClass::Integer => match occurences.get(&class).unwrap_or(&0) + 1 {
                n if n <= 6 => {
                    self.mov(
                        &src,
                        &Destination::Register(operands::Register {
                            register: self.registers.get(self.registers.len() - n).unwrap(),
                            size: WORD_SIZE,
                        }),
                        type_.signed(),
                    )
                    .unwrap();

                    0
                }
                _ => {
                    self.buf.push_str(&formatdoc!(
                        "
                        \tpush {src}
                        ",
                    ));

                    WORD_SIZE
                }
            },
        }
    }

    fn lea(&mut self, dest: &Destination, address: &EffectiveAddress) {
        self.buf.push_str(&formatdoc!(
            "
            \tlea {dest}, {address}
            "
        ));
    }

    fn populate_offsets(
        &mut self,
        symbol_table: &mut SymbolTable,
        scope: &Scope,
    ) -> Result<usize, ArchError> {
        let mut offset = 0;
        let mut occurences: HashMap<ParamClass, usize> = HashMap::new();

        for symbol in &mut symbol_table.0 {
            let type_ = symbol.type_();

            match symbol {
                Symbol::Local(local) => {
                    offset -= type_.size(&(Box::new(self.clone()) as Arch), &scope)? as isize;
                    local.offset = Offset(offset);
                }
                Symbol::Param(param) => {
                    let n = occurences.entry(ParamClass::from(&type_)).or_insert(0);
                    *n += 1;

                    if *n <= 6 {
                        offset -= type_.size(&(Box::new(self.clone()) as Arch), &scope)? as isize;
                        param.offset = Offset(offset);
                    } else {
                        // When call instruction is called it pushes return address on da stack
                        param.offset = Offset(((*n - 6) * WORD_SIZE + 8) as isize);
                    }
                }
                // Global variale in a scope, wot
                Symbol::Global(_) => unreachable!(),
                Symbol::Function(_) => unreachable!(),
            }
        }

        Ok(offset.unsigned_abs().next_multiple_of(STACK_ALIGNMENT))
    }

    fn shrink_stack(&mut self, size: usize) {
        self.buf.push_str(&formatdoc!(
            "
            \tsub rsp, {size}
            "
        ));
    }

    fn define_literal(&mut self, literal: String) -> String {
        let label = format!(".L{}", self.label_counter);

        self.literals.push((label.clone(), literal));
        self.label_counter += 1;

        label
    }

    fn array_offset(
        &mut self,
        base: &Destination,
        index: &Destination,
        size: usize,
    ) -> Result<(), ArchError> {
        let r = self.alloc().unwrap();
        let r_op = operands::Register {
            register: r,
            size: WORD_SIZE,
        };

        self.mov(
            &Source::Immediate(Immediate::UInt(size as u64)),
            &Destination::Register(r_op.clone()),
            false,
        )?;
        self.mul(index, &Source::Register(r_op), false)?;
        self.add(base, &index.to_owned().into());
        self.free(r)?;

        Ok(())
    }

    fn finish(&mut self) -> Vec<u8> {
        self.literals.iter().for_each(|(label, value)| {
            self.buf.insert_str(
                0,
                &formatdoc!(
                    "
                    {label}:
                        .string \"{value}\"
                    "
                ),
            );
        });
        self.buf.insert_str(0, ".section .text\n");
        self.buf.as_bytes().to_vec()
    }
}

impl Amd64 {
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

impl std::fmt::Display for operands::Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.register.from_size(self.size))
    }
}

impl std::fmt::Display for operands::Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::UInt(uint) => write!(f, "{uint}"),
            Self::Label(label) => write!(f, "OFFSET {label}"),
        }
    }
}

impl std::fmt::Display for operands::Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(register) => write!(f, "{}", register.register.from_size(WORD_SIZE)),
            Self::Label(label) => write!(f, "{label}"),
        }
    }
}

impl std::fmt::Display for operands::EffectiveAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = self.base.to_string();

        if let Some(index) = &self.index {
            str.push_str(&format!(" + {}", index.register.from_size(index.size)));
        }

        if let Some(scale) = self.scale {
            str.push_str(&format!("* {scale}"));
        }

        if let Some(displacement) = &self.displacement {
            str.push_str(&format!("{displacement}"));
        }

        write!(f, "{}", str)
    }
}

impl std::fmt::Display for operands::Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(effective_address) => write!(f, "{effective_address}"),
            Self::Register(register) => write!(f, "{register}"),
            Self::Immediate(immediate) => write!(f, "{immediate}"),
        }
    }
}

impl std::fmt::Display for operands::Destination {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Memory(effective_address) => write!(f, "{effective_address}"),
            Self::Register(register) => write!(f, "{register}"),
        }
    }
}

//#[cfg(test)]
//mod test {
//    use super::Amd64;
//    use crate::{
//        archs::Architecture,
//        codegen::locations::{self, MoveDestination, Offset},
//        parser::{ExprLit, IntLitRepr, UIntLitRepr},
//        register::Register,
//        scope::Scope,
//    };
//
//    #[test]
//    fn mov_literal() {
//        let r = Register::new("r15b", "r15w", "r15d", "r15");
//        let scope = Scope::new();
//        let tests = vec![
//            (
//                (
//                    MoveDestination::Global(locations::Global {
//                        size: 4,
//                        offset: Some(Offset(-5)),
//                        label: "foo".to_string(),
//                    }),
//                    ExprLit::UInt(UIntLitRepr::new(15_000)),
//                ),
//                "\tmov dword ptr [foo - 5], 15000\n",
//            ),
//            (
//                (
//                    MoveDestination::Global(locations::Global {
//                        size: 8,
//                        offset: None,
//                        label: "foo".to_string(),
//                    }),
//                    ExprLit::Int(IntLitRepr::new(-5)),
//                ),
//                "\tmov qword ptr [foo], -5\n",
//            ),
//            (
//                (
//                    MoveDestination::Local(locations::Local {
//                        size: 4,
//                        offset: Offset(-1),
//                    }),
//                    ExprLit::UInt(UIntLitRepr::new(5)),
//                ),
//                "\tmov dword ptr [rbp - 1], 5\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 8,
//                        offset: None,
//                        register: r,
//                    }),
//                    ExprLit::UInt(UIntLitRepr::new(5)),
//                ),
//                "\tmov r15, 5\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 8,
//                        offset: Some(Offset(-15)),
//                        register: r,
//                    }),
//                    ExprLit::UInt(UIntLitRepr::new(5)),
//                ),
//                "\tmov qword ptr [r15 - 15], 5\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 2,
//                        offset: Some(Offset(8)),
//                        register: r,
//                    }),
//                    ExprLit::Int(IntLitRepr::new(-7)),
//                ),
//                "\tmov word ptr [r15 + 8], -7\n",
//            ),
//        ];
//
//        for ((dest, lit), expected) in tests {
//            let mut arch = Amd64::new();
//            arch.mov_literal(lit, dest, &scope).unwrap();
//
//            assert_eq!(arch.buf, expected);
//        }
//    }
//
//    #[test]
//    fn mov_register() {
//        let r = Register::new("r15b", "r15w", "r15d", "r15");
//        let r2 = Register::new("r14b", "r14w", "r14d", "r14");
//        let scope = Scope::new();
//        let tests = vec![
//            (
//                (
//                    MoveDestination::Global(locations::Global {
//                        size: 4,
//                        offset: Some(Offset(-5)),
//                        label: "foo".to_string(),
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 4,
//                        register: r,
//                    },
//                    false,
//                ),
//                "\tmov dword ptr [foo - 5], r15d\n",
//            ),
//            (
//                (
//                    MoveDestination::Global(locations::Global {
//                        size: 8,
//                        offset: None,
//                        label: "foo".to_string(),
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 4,
//                        register: r,
//                    },
//                    false,
//                ),
//                "\tmovzx qword ptr [foo], r15d\n",
//            ),
//            (
//                (
//                    MoveDestination::Global(locations::Global {
//                        size: 8,
//                        offset: None,
//                        label: "foo".to_string(),
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 4,
//                        register: r,
//                    },
//                    true,
//                ),
//                "\tmovsx qword ptr [foo], r15d\n",
//            ),
//            (
//                (
//                    MoveDestination::Local(locations::Local {
//                        size: 1,
//                        offset: Offset(-10),
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 1,
//                        register: r,
//                    },
//                    true,
//                ),
//                "\tmov byte ptr [rbp - 10], r15b\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 2,
//                        offset: Some(Offset(10)),
//                        register: r2,
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 1,
//                        register: r,
//                    },
//                    true,
//                ),
//                "\tmovsx word ptr [r14 + 10], r15b\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 8,
//                        offset: None,
//                        register: r2,
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 4,
//                        register: r,
//                    },
//                    false,
//                ),
//                "\tmovzx r14, r15d\n",
//            ),
//            (
//                (
//                    MoveDestination::Register(locations::Register {
//                        size: 1,
//                        offset: None,
//                        register: r2,
//                    }),
//                    locations::Register {
//                        offset: None,
//                        size: 1,
//                        register: r,
//                    },
//                    false,
//                ),
//                "\tmov r14b, r15b\n",
//            ),
//        ];
//
//        for ((dest, r, signed), expected) in tests {
//            let mut arch = Amd64::new();
//            arch.mov_register(r, dest, signed, &scope).unwrap();
//
//            assert_eq!(arch.buf, expected);
//        }
//    }
//}
