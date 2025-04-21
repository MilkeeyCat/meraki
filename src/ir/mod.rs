pub mod ty;

use crate::ast::{BinOp, UnOp};
use ty::FieldIdx;
pub use ty::{AdtIdx, AdtKind, Ty, TyArray};

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
    pub basic_blocks: Vec<BasicBlock<'ir>>,
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

    pub fn get_block_mut(&mut self, idx: BasicBlockIdx) -> &mut BasicBlock<'ir> {
        &mut self.basic_blocks[idx]
    }

    pub fn create_local(&mut self, ty: &'ir Ty<'ir>) -> LocalIdx {
        let idx = self.locals.len();
        self.locals.push(ty);

        idx
    }
}

#[derive(Debug)]
pub struct BasicBlock<'ir> {
    pub statements: Vec<Statement<'ir>>,
    pub terminator: Terminator<'ir>,
}

impl<'ir> BasicBlock<'ir> {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
            terminator: Terminator::Return(None),
        }
    }
}

#[derive(Debug)]
pub enum Statement<'ir> {
    Assign(Place<'ir>, Rvalue<'ir>),
}

#[derive(Debug)]
pub enum Terminator<'ir> {
    Goto(BasicBlockIdx),
    Return(Option<Rvalue<'ir>>),
}

#[derive(Debug)]
pub enum Const {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

#[derive(Debug)]
pub enum Operand<'ir> {
    Place(Place<'ir>),
    Const(ValueTree, &'ir Ty<'ir>),
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
pub struct Place<'ir> {
    pub storage: Storage,
    pub projection: Vec<Projection<'ir>>,
}

#[derive(Debug)]
pub enum Projection<'ir> {
    Deref,
    Field(FieldIdx),
    Index(Operand<'ir>),
}

#[derive(Debug)]
pub enum Rvalue<'ir> {
    Use(Operand<'ir>),
    BinaryOp(BinOp, Operand<'ir>, Operand<'ir>),
    UnaryOp(UnOp, Operand<'ir>),
    Call {
        fn_idx: FunctionIdx,
        args: Vec<Rvalue<'ir>>,
        destination: Place<'ir>,
    },
}
