pub mod ast;
pub mod compile;
pub mod diagnostics;
pub mod ir;
pub mod lexer;
pub mod lowering;
//pub mod macros;
pub mod parser;
pub mod passes;
pub mod ty_problem;

use ast::{IntTy, UintTy};
use bumpalo::Bump;
use ir::{
    Ty,
    ty::{AdtDef, AdtIdx, AdtKind},
};

#[derive(Debug)]
pub struct CommonTypes<'ir> {
    null: &'ir Ty<'ir>,
    void: &'ir Ty<'ir>,
    bool: &'ir Ty<'ir>,
    i8: &'ir Ty<'ir>,
    i16: &'ir Ty<'ir>,
    i32: &'ir Ty<'ir>,
    i64: &'ir Ty<'ir>,
    isize: &'ir Ty<'ir>,
    u8: &'ir Ty<'ir>,
    u16: &'ir Ty<'ir>,
    u32: &'ir Ty<'ir>,
    u64: &'ir Ty<'ir>,
    usize: &'ir Ty<'ir>,
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
pub struct Context<'ir> {
    allocator: &'ir Bump,
    types: CommonTypes<'ir>,
    aggregates: Vec<AdtDef<'ir>>,
}

impl<'ir> Context<'ir> {
    pub fn new(allocator: &'ir Bump) -> Self {
        Self {
            allocator,
            types: CommonTypes::new(allocator),
            aggregates: Vec::new(),
        }
    }

    pub fn mk_adt(&mut self, name: String, kind: AdtKind) -> AdtIdx {
        let idx = self.aggregates.len();

        self.aggregates.push(AdtDef {
            name,
            kind,
            variants: Vec::new(),
        });

        idx
    }

    pub fn get_adt(&self, idx: AdtIdx) -> &AdtDef<'ir> {
        &self.aggregates[idx]
    }

    pub fn get_adt_mut(&mut self, idx: AdtIdx) -> &mut AdtDef<'ir> {
        &mut self.aggregates[idx]
    }
}
