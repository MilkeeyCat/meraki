use super::ArchError;
use crate::{
    codegen::locations::{MoveDestination, MoveSource},
    parser::CmpOp,
    register::{allocator::AllocatorError, Register},
    scope::Scope,
    symbol_table::SymbolTable,
    types::{Type, TypeError},
};

pub trait ArchitectureClone {
    fn clone_box(&self) -> Arch;
}

pub trait Architecture: ArchitectureClone {
    fn new() -> Self
    where
        Self: Sized;
    fn alignment(&self) -> usize;
    fn align(&self, offset: usize, size: usize) -> usize;
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
    fn fn_preamble(&mut self, name: &str, params: &[Type], stackframe: usize, scope: &Scope);
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, src: MoveSource) -> Result<(), TypeError>;
    fn jmp(&mut self, label: &str);
    fn call_fn(&mut self, name: &str, r: Option<&MoveDestination>);
    fn push_arg(
        &mut self,
        src: MoveSource,
        scope: &Scope,
        type_: &Type,
        preceding: &[Type],
    ) -> usize;
    fn populate_offsets(
        &mut self,
        symbol_table: &mut SymbolTable,
        scope: &Scope,
    ) -> Result<usize, ArchError>;
    fn lea(&mut self, dest: &Register, dest2: &MoveDestination);
    fn shrink_stack(&mut self, size: usize);
    fn finish(&mut self) -> Vec<u8>;
}

pub type Arch = Box<dyn Architecture>;

impl<T> ArchitectureClone for T
where
    T: 'static + Architecture + Clone,
{
    fn clone_box(&self) -> Arch {
        Box::new(self.clone())
    }
}

impl Clone for Arch {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
