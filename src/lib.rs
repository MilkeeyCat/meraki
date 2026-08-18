mod ast;
mod codegen;
pub mod compile;
mod diagnostics;
mod ir;
mod lexer;
mod lowering;
mod parser;
mod passes;
mod typecheck;

use ast::{IntTy, UintTy};
use bumpalo::Bump;
use ir::ty::{AdtDef, AdtIdx, AdtKind, Ty};

#[derive(Debug)]
pub(crate) struct CommonTypes<'ir> {
    pub(crate) null: &'ir Ty<'ir>,
    pub(crate) void: &'ir Ty<'ir>,
    pub(crate) bool: &'ir Ty<'ir>,
    pub(crate) i8: &'ir Ty<'ir>,
    pub(crate) i16: &'ir Ty<'ir>,
    pub(crate) i32: &'ir Ty<'ir>,
    pub(crate) i64: &'ir Ty<'ir>,
    pub(crate) isize: &'ir Ty<'ir>,
    pub(crate) u8: &'ir Ty<'ir>,
    pub(crate) u16: &'ir Ty<'ir>,
    pub(crate) u32: &'ir Ty<'ir>,
    pub(crate) u64: &'ir Ty<'ir>,
    pub(crate) usize: &'ir Ty<'ir>,
}

impl<'ir> CommonTypes<'ir> {
    fn new(allocator: &'ir Bump) -> Self {
        Self {
            null: allocator.alloc(ir::Ty::Null),
            void: allocator.alloc(ir::Ty::Void),
            bool: allocator.alloc(ir::Ty::Bool),
            i8: allocator.alloc(ir::Ty::Int(IntTy::I8)),
            i16: allocator.alloc(ir::Ty::Int(IntTy::I16)),
            i32: allocator.alloc(ir::Ty::Int(IntTy::I32)),
            i64: allocator.alloc(ir::Ty::Int(IntTy::I64)),
            isize: allocator.alloc(ir::Ty::Int(IntTy::Isize)),
            u8: allocator.alloc(ir::Ty::UInt(UintTy::U8)),
            u16: allocator.alloc(ir::Ty::UInt(UintTy::U16)),
            u32: allocator.alloc(ir::Ty::UInt(UintTy::U32)),
            u64: allocator.alloc(ir::Ty::UInt(UintTy::U64)),
            usize: allocator.alloc(ir::Ty::UInt(UintTy::Usize)),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Context<'ir> {
    pub(crate) allocator: &'ir Bump,
    pub(crate) types: CommonTypes<'ir>,
    aggregates: Vec<AdtDef<'ir>>,
}

impl<'ir> Context<'ir> {
    pub(crate) fn new(allocator: &'ir Bump) -> Self {
        Self {
            allocator,
            types: CommonTypes::new(allocator),
            aggregates: Vec::new(),
        }
    }

    pub(crate) fn mk_adt(&mut self, name: String, kind: AdtKind) -> AdtIdx {
        let idx = self.aggregates.len();

        self.aggregates.push(AdtDef {
            name,
            kind,
            variants: Vec::new(),
        });

        idx
    }

    pub(crate) fn get_adt(&self, idx: AdtIdx) -> &AdtDef<'ir> {
        &self.aggregates[idx]
    }

    pub(crate) fn get_adt_mut(&mut self, idx: AdtIdx) -> &mut AdtDef<'ir> {
        &mut self.aggregates[idx]
    }
}
