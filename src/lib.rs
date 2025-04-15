pub mod ast;
pub mod compile;
pub mod diagnostics;
pub mod ir;
pub mod lexer;
pub mod lowering;
//pub mod macros;
pub mod parser;
pub mod passes;
//pub mod ty_problem;

use bumpalo::Bump;
use ir::ty::{AdtDef, AdtIdx, AdtKind};

#[derive(Debug)]
pub struct Context<'ir> {
    allocator: &'ir Bump,
    aggregates: Vec<AdtDef<'ir>>,
}

impl<'ir> Context<'ir> {
    pub fn new(allocator: &'ir Bump) -> Self {
        Self {
            allocator,
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

    pub fn get_adt_mut(&mut self, idx: AdtIdx) -> &mut AdtDef<'ir> {
        &mut self.aggregates[idx]
    }
}
