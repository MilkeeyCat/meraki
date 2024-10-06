use super::ArchError;
use crate::{
    codegen::{Argument, Destination, EffectiveAddress, Source},
    parser::{BitwiseOp, Block, CmpOp},
    register::{allocator::AllocatorError, Register},
    scope::Scope,
    type_table as tt,
    types::Type,
};

pub enum Jump {
    Unconditional,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
}

pub trait ArchitectureClone {
    fn clone_box(&self) -> Arch;
}

pub trait Architecture: ArchitectureClone {
    fn new() -> Self
    where
        Self: Sized;
    fn word_size(&self) -> usize;
    fn stack_alignment(&self) -> usize;
    fn size(&self, type_: &Type, scope: &Scope) -> usize;
    fn struct_size(&self, type_struct: &tt::TypeStruct, scope: &Scope) -> usize {
        let mut offset: usize = 0;
        let mut largest = 0;

        for type_ in type_struct.types() {
            let size = self.size(type_, scope);

            offset = offset.next_multiple_of(size);
            offset += size;

            if size > largest {
                largest = size;
            }
        }

        // Align to the largest element in the struct
        offset.next_multiple_of(largest)
    }
    fn alloc(&mut self) -> Result<Register, AllocatorError>;
    fn free(&mut self, register: Register) -> Result<(), AllocatorError>;
    fn size_name(size: usize) -> &'static str
    where
        Self: Sized;
    fn declare(&mut self, name: &str, size: usize);
    fn mov(&mut self, src: &Source, dest: &Destination, signed: bool) -> Result<(), ArchError>;
    fn negate(&mut self, dest: &Destination);
    fn not(&mut self, dest: &Destination, dest2: &Destination);
    fn add(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), ArchError>;
    fn sub(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), ArchError>;
    fn mul(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), ArchError>;
    fn div(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        signed: bool,
    ) -> Result<(), ArchError>;
    fn bitwise(
        &mut self,
        lhs: &Source,
        rhs: &Source,
        dest: &Destination,
        op: BitwiseOp,
        signed: bool,
    ) -> Result<(), ArchError>;
    fn bitwise_not(&mut self, dest: &Destination);
    fn cmp(&mut self, dest: &Destination, src: &Source);
    fn setcc(&mut self, dest: &Destination, condition: CmpOp);
    fn fn_preamble(
        &mut self,
        name: &str,
        params: &[Type],
        stackframe: usize,
        scope: &Scope,
    ) -> Result<(), ArchError>;
    fn fn_postamble(&mut self, name: &str, stackframe: usize);
    fn ret(&mut self, src: &Source, signed: bool) -> Result<(), ArchError>;
    fn jcc(&mut self, label: &str, kind: Jump);
    fn call_fn(
        &mut self,
        name: &str,
        dest: Option<&Destination>,
        signed: bool,
        size: usize,
    ) -> Result<(), ArchError>;
    fn push_arg(&mut self, src: Source, type_: &Type, preceding: &[Type]) -> Argument;
    fn populate_offsets(
        &mut self,
        block: &mut Block,
        scope: &Scope,
        start_offset: isize,
    ) -> Result<isize, ArchError>;
    fn lea(&mut self, dest: &Destination, address: &EffectiveAddress);
    fn shrink_stack(&mut self, size: usize);
    fn generate_label(&mut self) -> String;
    fn write_label(&mut self, label: &str);
    fn define_literal(&mut self, literal: String) -> String;
    fn array_offset(
        &mut self,
        base: &Source,
        index: &Source,
        size: usize,
        dest: &Destination,
    ) -> Result<(), ArchError>;
    fn shl(&mut self, dest: &Destination, src: &Source) -> Result<(), ArchError>;
    fn shr(&mut self, dest: &Destination, src: &Source) -> Result<(), ArchError>;
    fn push(&mut self, src: &Source);
    fn pop(&mut self, dest: &Destination);
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
