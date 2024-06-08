use super::{
    arch::{Architecture, Cmp},
    LoadItem,
};
use crate::{
    parser::{ExprLit, StmtVarDecl, Type},
    register_allocator::Register,
    symtable::Symbol,
};
use indoc::formatdoc;

pub struct Amd64 {}

impl Architecture for Amd64 {
    fn new() -> (Vec<Register>, Self) {
        (
            vec![
                Register::new("r8b", "r8w", "r8d", "r8"),
                Register::new("r8b", "r9w", "r8d", "r9"),
                Register::new("dil", "di", "edi", "rdi"),
                Register::new("sil", "si", "esi", "rsi"),
                Register::new("dl", "dx", "edx", "rdx"),
                Register::new("cl", "cx", "ecx", "rcx"),
            ],
            Self {},
        )
    }

    fn size(type_: &Type) -> usize {
        match type_ {
            _ => unreachable!(),
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
                Symbol::GlobalVar(global_var) => {
                    formatdoc!(
                        "
                        \tmov {}, [{}]
                        ",
                        r.qword(),
                        global_var.name,
                    )
                }
            },
        }
    }

    fn declare(&self, var: &StmtVarDecl) -> String {
        let size = &var.type_.size::<Self>();

        formatdoc!(
            "
            \t{} resb {size}
            ",
            var.name,
        )
    }

    fn mov(&self, label: &str, r: &Register, type_: Type) -> String {
        formatdoc!(
            "
            \tmov [{}], {}
            ",
            label,
            r.from_size(type_.size::<Self>()),
        )
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

    fn cmp(&self, r1: &Register, r2: &Register, cmp: Cmp) -> String {
        let ins = match cmp {
            Cmp::LessThan => formatdoc!("setl {}", r1.byte()),
            Cmp::LessEqual => formatdoc!("setle {}", r1.byte()),
            Cmp::GreaterThan => formatdoc!("setg {}", r1.byte()),
            Cmp::GreaterEqual => formatdoc!("setge {}", r1.byte()),
            Cmp::Equal => formatdoc!("sete {}", r1.byte()),
            Cmp::NotEqual => formatdoc!("setne {}", r1.byte()),
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
}
