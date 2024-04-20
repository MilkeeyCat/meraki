use crate::lexer::Token;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    Lessgreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // foo()
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        use Token::*;

        match value {
            Equal | NotEqual => Self::Equals,
            LessThan | GreaterThan | LessEqual | GreaterEqual => Self::Lessgreater,
            Plus | Minus => Self::Sum,
            Asterisk | Slash => Self::Product,
            _ => Self::Lowest,
        }
    }
}

impl Into<u8> for Precedence {
    fn into(self) -> u8 {
        use Precedence::*;

        match self {
            Lowest => 0,
            Equals => 1,
            Lessgreater => 2,
            Sum => 3,
            Product => 4,
            Prefix => 5,
            Call => 6,
        }
    }
}
