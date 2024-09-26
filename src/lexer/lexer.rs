use super::{LexerError, Token};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.into_bytes(),
            ch: 0,
            read_position: 0,
            position: 0,
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            b'-' => {
                if self.peek() == b'>' {
                    self.read_char();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            b'+' => Token::Plus,
            b'/' => {
                if self.peek() == b'/' {
                    self.skip_comment();

                    return Ok(self.next_token()?);
                } else {
                    Token::Slash
                }
            }
            b'.' => Token::Period,
            b'~' => Token::Tilde,
            b'&' => {
                if self.peek() == b'&' {
                    self.read_char();
                    Token::And
                } else {
                    Token::Ampersand
                }
            }
            b'|' => {
                if self.peek() == b'|' {
                    self.read_char();
                    Token::Or
                } else {
                    Token::Bar
                }
            }
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            b'*' => Token::Asterisk,
            b'<' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::LessEqual
                } else if self.peek() == b'<' {
                    self.read_char();
                    Token::Shl
                } else {
                    Token::LessThan
                }
            }
            b'>' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::GreaterEqual
                } else if self.peek() == b'>' {
                    self.read_char();
                    Token::Shr
                } else {
                    Token::GreaterThan
                }
            }
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b',' => Token::Comma,
            b':' => Token::Colon,
            b'"' => Token::String(self.read_string()),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();

                return Ok(match ident.as_str() {
                    "const" => Token::Const,
                    "true" => Token::True,
                    "let" => Token::Let,
                    "fn" => Token::Fn,
                    "enum" => Token::Enum,
                    "struct" => Token::Struct,
                    "false" => Token::False,
                    "if" => Token::If,
                    "while" => Token::While,
                    "for" => Token::For,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    "as" => Token::As,
                    "continue" => Token::Continue,
                    "break" => Token::Break,
                    "u8" => Token::U8,
                    "u16" => Token::U16,
                    "u32" => Token::U32,
                    "u64" => Token::U64,
                    "i8" => Token::I8,
                    "i16" => Token::I16,
                    "i32" => Token::I32,
                    "i64" => Token::I64,
                    "usize" => Token::Usize,
                    "isize" => Token::Isize,
                    "bool" => Token::Bool,
                    "void" => Token::Void,
                    "NULL" => Token::Null,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => {
                return Ok(Token::Integer(self.read_int()));
            }
            0 => Token::Eof,
            c => return Err(LexerError::UnknownCharacter(char::from(c))),
        };

        self.read_char();

        Ok(token)
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }

        self.input[self.read_position]
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }

        String::from_utf8_lossy(&self.input[pos..self.position]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.ch != b'\n' {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, LexerError};
    use crate::lexer::Token;

    #[test]
    fn source_into_tokens() -> Result<(), LexerError> {
        let input = r#"
            ident
            69
            "string"

            =
            +
            -
            !
            *
            /
            ->
            .
            ~
            &
            |
            ==
            !=
            <
            >
            <=
            >=
            &&
            ||
            <<
            >>
            ,
            ;
            :
            (
            )
            {
            }
            [
            ]

            // keywords
            // heyo :D
            const
            true
            false
            let
            fn
            enum
            struct
            if
            while
            for
            else
            return
            as
            continue
            break

            u8
            u16
            u32
            u64
            i8
            i16
            i32
            i64
            usize
            isize
            bool
            void
            NULL
        "#;

        let tokens = vec![
            Token::Ident(String::from("ident")),
            Token::Integer(String::from("69")),
            Token::String(String::from("string")),
            Token::Assign,
            Token::Plus,
            Token::Minus,
            Token::Bang,
            Token::Asterisk,
            Token::Slash,
            Token::Arrow,
            Token::Period,
            Token::Tilde,
            Token::Ampersand,
            Token::Bar,
            Token::Equal,
            Token::NotEqual,
            Token::LessThan,
            Token::GreaterThan,
            Token::LessEqual,
            Token::GreaterEqual,
            Token::And,
            Token::Or,
            Token::Shl,
            Token::Shr,
            Token::Comma,
            Token::Semicolon,
            Token::Colon,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::LBracket,
            Token::RBracket,
            Token::Const,
            Token::True,
            Token::False,
            Token::Let,
            Token::Fn,
            Token::Enum,
            Token::Struct,
            Token::If,
            Token::While,
            Token::For,
            Token::Else,
            Token::Return,
            Token::As,
            Token::Continue,
            Token::Break,
            Token::U8,
            Token::U16,
            Token::U32,
            Token::U64,
            Token::I8,
            Token::I16,
            Token::I32,
            Token::I64,
            Token::Usize,
            Token::Isize,
            Token::Bool,
            Token::Void,
            Token::Null,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in tokens {
            let next_token = lexer.next_token()?;

            assert_eq!(token, next_token);
        }

        Ok(())
    }
}
