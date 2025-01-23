pub mod amd64_asm;

use crate::Context;

pub trait Codegen<'a, 'ir> {
    fn new(ctx: &'a Context<'ir>) -> Self
    where
        Self: Sized;

    fn compile(&mut self) -> Result<Vec<u8>, Box<dyn std::error::Error>>;
}
