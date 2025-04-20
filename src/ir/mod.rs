pub mod ty;

use crate::ast::{BinOp, UnOp};
use ty::FieldIdx;
pub use ty::{AdtIdx, Ty, TyArray};

pub type FunctionIdx = usize;
pub type LocalIdx = usize;
pub type GlobalIdx = usize;
pub type BasicBlockIdx = usize;

#[derive(Debug)]
pub struct Module<'ir> {
    pub functions: Vec<Function<'ir>>,
    pub globals: Vec<Global<'ir>>,
}

impl<'ir> Module<'ir> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
        }
    }

    pub fn add_global_with_idx(&mut self, idx: GlobalIdx, global: Global<'ir>) {
        assert_eq!(self.globals.len(), idx);

        self.globals.push(global);
    }

    pub fn create_fn(
        &mut self,
        name: String,
        params: &[&'ir Ty<'ir>],
        ret_ty: &'ir Ty<'ir>,
    ) -> FunctionIdx {
        let idx = self.functions.len();
        self.functions.push(Function {
            name,
            basic_blocks: Vec::new(),
            locals: params.to_vec(),
            arg_count: params.len(),
            ret_ty,
        });

        idx
    }
}

#[derive(Debug)]
pub struct Global<'ir> {
    pub name: String,
    pub ty: &'ir Ty<'ir>,
}

#[derive(Debug)]
pub struct Function<'ir> {
    pub name: String,
    pub basic_blocks: Vec<BasicBlock>,
    pub locals: Vec<&'ir Ty<'ir>>,
    pub arg_count: usize,
    pub ret_ty: &'ir Ty<'ir>,
}

impl<'ir> Function<'ir> {
    pub fn create_block(&mut self) -> BasicBlockIdx {
        let idx = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlock::new());

        idx
    }

    pub fn get_block_mut(&mut self, idx: BasicBlockIdx) -> &mut BasicBlock {
        &mut self.basic_blocks[idx]
    }

    pub fn create_local(&mut self, ty: &'ir Ty<'ir>) -> LocalIdx {
        let idx = self.locals.len();
        self.locals.push(ty);

        idx
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
            terminator: Terminator::Return(None),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Assign(Place, Rvalue),
}

#[derive(Debug)]
pub enum Terminator {
    Goto(BasicBlockIdx),
    Return(Option<Rvalue>),
}

#[derive(Debug)]
pub enum Const {
    I8(i8),
    U8(u8),
}

#[derive(Debug)]
pub enum Operand {
    Place(Place),
    Const(ValueTree),
}

#[derive(Debug)]
pub enum ValueTree {
    Leaf(Const),
    Branch(Vec<Self>),
}

#[derive(Debug)]
pub enum Storage {
    Local(LocalIdx),
    Global(GlobalIdx),
}

#[derive(Debug)]
pub struct Place {
    pub storage: Storage,
    pub projection: Vec<Projection>,
}

#[derive(Debug)]
pub enum Projection {
    Deref,
    Field(FieldIdx),
    Index(Operand),
}

#[derive(Debug)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
    Call {
        fn_idx: FunctionIdx,
        args: Vec<Rvalue>,
        destination: Place,
    },
}
