use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Failed to parse char {0}")]
    UnknownCharacter(char),
}
