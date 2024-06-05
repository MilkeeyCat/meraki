use crate::{
    parser::{ExprLit, StmtVarDecl, Type},
    register_allocator::Register,
    symtable::Symbol,
};

pub enum LoadItem {
    Lit(ExprLit),
    Symbol(Symbol),
}

pub trait Architecture {
    fn new() -> (Vec<Register>, Self);

    fn load(&self, r: &Register, item: LoadItem) -> String;
    fn size(type_: &Type) -> usize;
    fn declare(&self, var: &StmtVarDecl) -> String;
    fn mov(&self, label: &str, r: &Register) -> String;
    fn negate(&self, r: &Register) -> String;
    fn add(&self, r1: &Register, r2: &Register) -> String;
    fn sub(&self, r1: &Register, r2: &Register) -> String;
    fn mul(&self, r1: &Register, r2: &Register) -> String;
    fn div(&self, r1: &Register, r2: &Register) -> String;
}
