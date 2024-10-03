use crate::register::Register;

#[derive(Debug)]
pub enum AllocatorError {
    DoubleFree,
    RanOutOfRegisters,
    AlreadyInUse(Register),
}

impl std::fmt::Display for AllocatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DoubleFree => write!(f, "Register was double freed"),
            Self::RanOutOfRegisters => write!(f, "Ran out of registers, whoops!"),
            Self::AlreadyInUse(r) => write!(f, "Register {} is already in use", r.from_size(8)),
        }
    }
}
