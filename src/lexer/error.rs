use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Failed to parse char {0}")]
    UnknownCharacter(char),
}
