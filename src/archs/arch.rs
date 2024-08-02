use crate::{
    codegen::locations::{self, MoveDestination, MoveSource},
    parser::CmpOp,
    register_allocator::{AllocatorError, Register},
    scope::Scope,
    type_::Type,
};

pub trait Architecture {
    fn new() -> Self
    where
        Self: Sized;
    fn alignment(&self) -> usize;
    fn size(&self, type_: &Type) -> usize;
    fn alloc(&mut self) -> Result<Register, AllocatorError>;
    fn free(&mut self, register: Register) -> Result<(), AllocatorError>;
    fn size_name(size: usize) -> &'static str
    where
        Self: Sized;
    fn declare(&mut self, name: &str, size: usize);
    fn mov(&mut self, src: MoveSource, dest: MoveDestination, scope: &Scope);
    fn negate(&mut self, r: &Register);
    fn not(&mut self, dest: &Register, src: &Register);
    fn add(&mut self, dest: &MoveDestination, src: &locations::Register);
    fn sub(&mut self, dest: &Register, src: &Register);
    fn mul(&mut self, dest: &Register, src: &Register);
    fn div(&mut self, dest: &Register, src: &Register);
    fn cmp(&mut self, dest: &Register, src: &Register, cmp: CmpOp);
    fn fn_preamble(&mut self, name: &str, stackframe: usize);
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, r: Register, type_: Type, scope: &Scope);
    fn jmp(&mut self, label: &str);
    fn call_fn(&mut self, name: &str, r: Option<&Register>);
    fn move_function_argument(&mut self, r: Register, i: usize);
    fn lea(&mut self, dest: &Register, offset: usize);
    fn finish(&self) -> Vec<u8>;
}
