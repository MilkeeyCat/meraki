use crate::{
    parser::{CmpOp, ExprLit, StmtVarDecl},
    register_allocator::{AllocatorError, Register},
    scope::Scope,
    type_::Type,
};

pub enum MoveSource<'a> {
    Global(&'a str, Type),
    Local(usize, Type),
    Param(usize, Type),
    Register(&'a Register, Type),
    Lit(ExprLit),
    Void,
}

impl<'a> MoveSource<'a> {
    pub fn from_dest(dest: MoveDestination<'a>, type_: Type) -> Self {
        match dest {
            MoveDestination::Global(label) => MoveSource::Global(label, type_),
            MoveDestination::Local(offset) => MoveSource::Local(offset, type_),
            MoveDestination::Register(register) => MoveSource::Register(register, type_),
            MoveDestination::Void => MoveSource::Void,
        }
    }
}

#[derive(Clone, Debug)]
pub enum MoveDestination<'a> {
    Global(&'a str),
    Local(usize),
    Register(&'a Register),
    Void,
}

impl<'a> MoveDestination<'a> {
    pub fn register(self) -> Option<&'a Register> {
        match self {
            Self::Register(register) => Some(register),
            _ => None,
        }
    }
}

pub trait Architecture {
    fn new() -> Self;
    fn size(type_: &Type) -> usize;
    fn alloc(&mut self) -> Result<Register, AllocatorError>;
    fn free(&mut self, register: Register) -> Result<(), AllocatorError>;
    fn size_name(size: usize) -> &'static str;
    fn declare(&mut self, var: StmtVarDecl);
    fn mov(&mut self, src: MoveSource, dest: MoveDestination, scope: &Scope);
    fn negate(&mut self, r: &Register);
    fn not(&mut self, dest: &Register, src: &Register);
    fn add(&mut self, dest: &Register, src: &Register);
    fn sub(&mut self, dest: &Register, src: &Register);
    fn mul(&mut self, dest: &Register, src: &Register);
    fn div(&mut self, dest: &Register, src: &Register);
    fn cmp(&mut self, dest: &Register, src: &Register, cmp: CmpOp);
    fn fn_preamble(&mut self, name: &str, stackframe: usize);
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, r: Register, type_: Type);
    fn jmp(&mut self, label: &str);
    fn call_fn(&mut self, name: &str, r: &Register);
    fn move_function_argument(&mut self, r: Register, i: usize);
    fn finish(&self) -> Vec<u8>;
}
