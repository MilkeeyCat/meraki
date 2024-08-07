use super::ArchError;
use crate::{
    codegen::locations::{MoveDestination, MoveSource, Offset},
    parser::CmpOp,
    register::{allocator::AllocatorError, Register},
    scope::Scope,
    types::{Type, TypeError},
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
    fn mov(
        &mut self,
        src: MoveSource,
        dest: MoveDestination,
        scope: &Scope,
    ) -> Result<(), ArchError>;
    fn negate(&mut self, dest: &MoveDestination);
    fn not(&mut self, dest: &MoveDestination, dest2: &MoveDestination);
    fn add(&mut self, dest: &MoveDestination, src: &MoveSource);
    fn sub(&mut self, dest: &MoveDestination, src: &MoveSource);
    fn mul(&mut self, dest: &MoveDestination, src: &MoveSource);
    fn div(&mut self, dest: &MoveDestination, src: &MoveSource);
    fn cmp(&mut self, dest: &MoveDestination, src: &MoveSource, cmp: CmpOp);
    fn fn_preamble(&mut self, name: &str, stackframe: usize);
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, src: MoveSource) -> Result<(), TypeError>;
    fn jmp(&mut self, label: &str);
    fn call_fn(&mut self, name: &str, r: Option<&MoveDestination>);
    fn move_function_argument(&mut self, r: Register, i: usize);
    fn lea(&mut self, dest: &Register, offset: Offset);
    fn finish(&mut self) -> Vec<u8>;
}
