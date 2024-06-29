use crate::{
    parser::{CmpOp, ExprLit, StmtVarDecl},
    register_allocator::{AllocatorError, Register},
    symbol_table::Symbol,
    type_::Type,
};

pub enum LoadItem {
    Lit(ExprLit),
    Symbol(Symbol),
}

pub enum SaveItem<'a> {
    Global(&'a str),
    Local(usize),
}

pub trait Architecture {
    fn new() -> Self;
    fn load(&mut self, item: LoadItem) -> Result<Register, AllocatorError>;
    fn size(type_: &Type) -> usize;
    fn size_name(size: usize) -> &'static str;
    fn declare(&mut self, var: StmtVarDecl);
    fn save(&mut self, item: SaveItem, r: &Register, type_: Type);
    fn negate(&mut self, r: &Register);
    fn not(&mut self, r1: Register) -> Result<Register, AllocatorError>;
    fn add(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError>;
    fn sub(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError>;
    fn mul(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError>;
    fn div(&mut self, r1: &Register, r2: Register) -> Result<(), AllocatorError>;
    fn cmp(&mut self, r1: &Register, r2: Register, cmp: CmpOp) -> Result<(), AllocatorError>;
    fn fn_preamble(&mut self, name: &str, stackframe: usize);
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, r: &Register, type_: Type);
    fn jmp(&mut self, label: &str);
    fn finish(&self) -> Vec<u8>;
}
