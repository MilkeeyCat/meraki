pub mod amd64_asm;

use crate::ir::Ir;
use crate::parser::OpParseError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error(transparent)]
    OpParse(#[from] OpParseError),
}

pub trait Codegen<'a, 'ir> {
    fn new(ir: &'a Ir<'ir>) -> Self
    where
        Self: Sized;

    fn compile(&mut self) -> Vec<u8>;
}
