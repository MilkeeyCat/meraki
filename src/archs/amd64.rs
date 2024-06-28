use super::{
    arch::{Architecture, SaveItem},
    LoadItem,
};
use crate::{
    parser::{CmpOp, ExprLit, StmtVarDecl},
    register_allocator::Register,
    symbol_table::Symbol,
    type_::Type,
};
use indoc::formatdoc;

pub struct Amd64;

impl Architecture for Amd64 {
    fn new() -> (Vec<Register>, Self) {
        (
            vec![
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
            ],
            Self {},
        )
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

    fn load(&self, r: &Register, item: LoadItem) -> String {
        match item {
            LoadItem::Lit(literal) => match literal {
                ExprLit::Int(integer) => {
                    formatdoc!(
                        "
                        \tmov {}, {}
                        ",
                        r.qword(),
                        integer.to_string(),
                    )
                }
                ExprLit::UInt(integer) => {
                    formatdoc!(
                        "
                        \tmov {}, {}
                        ",
                        r.qword(),
                        integer.to_string(),
                    )
                }
                ExprLit::Bool(value) => {
                    formatdoc!(
                        "
                        \tmov {}, {}
                        ",
                        r.qword(),
                        value as u8
                    )
                }
            },
            LoadItem::Symbol(symbol) => match symbol {
                Symbol::Global(global) => {
                    let ins = if global.type_.signed() {
                        "movsx"
                    } else {
                        "movzx"
                    };

                    formatdoc!(
                        "
                        \t{} {}, {} [{}]
                        ",
                        ins,
                        r.qword(),
                        Self::size_name(Type::size::<Self>(&global.type_)),
                        global.name,
                    )
                }
                Symbol::Local(local) => {
                    let ins = if local.type_.signed() {
                        "movsx"
                    } else {
                        "movzx"
                    };

                    formatdoc!(
                        "
                        \t{} {}, {} [rbp - {}]
                        ",
                        ins,
                        r.qword(),
                        Self::size_name(Type::size::<Self>(&local.type_)),
                        local.offset,
                    )
                }
                Symbol::Param(param) => todo!(),
            },
        }
    }

    fn declare(&self, var: StmtVarDecl) -> String {
        let size = &var.type_.size::<Self>();

        formatdoc!(
            "
            \t{} resb {size}
            ",
            var.name,
        )
    }

    fn save(&self, item: SaveItem, r: &Register, type_: Type) -> String {
        match item {
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
        }
    }

    fn negate(&self, r: &Register) -> String {
        formatdoc!(
            "
            \tneg {}
            ",
            r.qword(),
        )
    }

    fn not(&self, r1: &Register, r2: &Register) -> String {
        formatdoc!(
            "
            \tcmp {}, 0
            \tsete {}
            ",
            r2.qword(),
            r1.byte(),
        )
    }

    fn add(&self, r1: &Register, r2: &Register) -> String {
        formatdoc!(
            "
            \tadd {}, {}
            ",
            r1.qword(),
            r2.qword(),
        )
    }

    fn sub(&self, r1: &Register, r2: &Register) -> String {
        formatdoc!(
            "
            \tsub {}, {}
            ",
            r1.qword(),
            r2.qword(),
        )
    }

    fn mul(&self, r1: &Register, r2: &Register) -> String {
        formatdoc!(
            "
            \timul {}, {}
            ",
            r1.qword(),
            r2.qword(),
        )
    }

    //NOTE: if mafs doesn't works, prolly because of this
    fn div(&self, r1: &Register, r2: &Register) -> String {
        formatdoc!(
            "
            \tmov rax, {}
            \tcqo
            \tidiv {}
            \tmov {}, rax
            ",
            r1.qword(),
            r2.qword(),
            r1.qword(),
        )
    }

    fn cmp(&self, r1: &Register, r2: &Register, cmp: CmpOp) -> String {
        let ins = match cmp {
            CmpOp::LessThan => formatdoc!("setl {}", r1.byte()),
            CmpOp::LessEqual => formatdoc!("setle {}", r1.byte()),
            CmpOp::GreaterThan => formatdoc!("setg {}", r1.byte()),
            CmpOp::GreaterEqual => formatdoc!("setge {}", r1.byte()),
            CmpOp::Equal => formatdoc!("sete {}", r1.byte()),
            CmpOp::NotEqual => formatdoc!("setne {}", r1.byte()),
        };

        formatdoc!(
            "
           \tcmp {}, {}
           \t{}
           ",
            r1.qword(),
            r2.qword(),
            ins,
        )
    }

    fn fn_preamble(&self, name: &str, stackframe: usize) -> String {
        formatdoc!(
            "
            {}:
                push rbp
                mov rbp, rsp
                sub rsp, {}
            ",
            name,
            stackframe,
        )
    }

    fn fn_postamble(&self, name: &str, stackframe: usize) -> String {
        formatdoc!(
            "
            {}_ret:
                add rsp, {}
                leave
                ret
            ",
            name,
            stackframe,
        )
    }

    fn ret(&self, r: &Register, type_: Type) -> String {
        let ins = if type_.signed() { "movsx" } else { "movzx" };

        formatdoc!(
            "
            \t{} rax, {}
            ",
            ins,
            r.from_size(Type::size::<Self>(&type_)),
        )
    }

    fn jmp(&self, label: &str) -> String {
        formatdoc!(
            "
            \tjmp {}
            ",
            label
        )
    }
}
