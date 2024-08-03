#[derive(Debug)]
pub enum LexerError {
    UnknownCharacter(char),
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownCharacter(ch) => write!(f, "Failed to parse char {ch}"),
        }
    }
}
