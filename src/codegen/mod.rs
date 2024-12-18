pub mod amd64_asm;

use crate::{parser::OpParseError, Context};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error(transparent)]
    OpParse(#[from] OpParseError),
}

pub trait Codegen<'a, 'ir> {
    fn new(ctx: &'a Context<'ir>) -> Self
    where
        Self: Sized;

    fn compile(&mut self) -> Result<Vec<u8>, Box<dyn std::error::Error>>;
}
