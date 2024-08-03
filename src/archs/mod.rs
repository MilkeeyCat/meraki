mod arch;
mod error;

pub use arch::Architecture;
pub use error::ArchError;

mod amd64;
pub use amd64::*;
