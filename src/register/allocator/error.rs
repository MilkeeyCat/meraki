use crate::register::Register;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AllocatorError {
    #[error("Register was double freed")]
    DoubleFree,
    #[error("Ran out of registers, whoops!")]
    RanOutOfRegisters,
    #[error("Register {} is already in use", .0.from_size(8))]
    AlreadyInUse(Register),
}
