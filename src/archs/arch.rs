use crate::{
    parser::{BinOp, ExprLit, StmtVarDecl, Type},
    register_allocator::Register,
    symtable::Symbol,
};

pub enum LoadItem {
    Lit(ExprLit),
    Symbol(Symbol),
}

pub enum Cmp {
    LessEqual,
    LessThan,
    GreaterEqual,
    GreaterThan,
    Equal,
    NotEqual,
}

impl TryFrom<&BinOp> for Cmp {
    type Error = String;

    fn try_from(value: &BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::LessThan => Ok(Self::LessThan),
            BinOp::LessEqual => Ok(Self::LessEqual),
            BinOp::GreaterThan => Ok(Self::GreaterThan),
            BinOp::GreaterEqual => Ok(Self::GreaterEqual),
            BinOp::Equal => Ok(Self::Equal),
            BinOp::NotEqual => Ok(Self::NotEqual),
            _ => Err("xd".to_owned()),
        }
    }
}

pub trait Architecture {
    fn new() -> (Vec<Register>, Self);

    fn load(&self, r: &Register, item: LoadItem) -> String;
    fn size(type_: &Type) -> usize;
    fn declare(&self, var: &StmtVarDecl) -> String;
    fn mov(&self, label: &str, r: &Register, type_: Type) -> String;
    fn negate(&self, r: &Register) -> String;
    fn not(&self, r1: &Register, r2: &Register) -> String;
    fn add(&self, r1: &Register, r2: &Register) -> String;
    fn sub(&self, r1: &Register, r2: &Register) -> String;
    fn mul(&self, r1: &Register, r2: &Register) -> String;
    fn div(&self, r1: &Register, r2: &Register) -> String;
    fn cmp(&self, r1: &Register, r2: &Register, cmp: Cmp) -> String;
}
