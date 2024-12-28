mod error;
mod token;

pub use error::Error;
pub use token::Token;

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            ch: '\0',
            position: 0,
            read_position: 0,
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        match self.input[self.read_position..].chars().next() {
            Some(ch) => {
                self.ch = ch;
                self.position = self.read_position;
                self.read_position += ch.len_utf8();
            }
            None => {
                self.ch = '\0';
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.read_position..].chars().next()
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }

        self.input[pos..self.position].to_string()
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        self.input[pos..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == '"' || self.ch == '\0' {
                break self.input[pos..self.position].to_string();
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn skip_comment(&mut self) {
        while self.ch != '\n' {
            self.read_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '-' => {
                if self.peek() == Some('>') {
                    self.read_char();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '+' => Token::Plus,
            '/' => {
                if self.peek() == Some('/') {
                    self.skip_comment();

                    return self.next();
                } else {
                    Token::Slash
                }
            }
            '.' => Token::Period,
            '~' => Token::Tilde,
            '&' => {
                if self.peek() == Some('&') {
                    self.read_char();
                    Token::And
                } else {
                    Token::Ampersand
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.read_char();
                    Token::Or
                } else {
                    Token::Bar
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            '*' => Token::Asterisk,
            '<' => match self.peek() {
                Some('=') => {
                    self.read_char();
                    Token::LessEqual
                }
                Some('<') => {
                    self.read_char();
                    Token::Shl
                }
                _ => Token::LessThan,
            },
            '>' => match self.peek() {
                Some('=') => {
                    self.read_char();
                    Token::GreaterEqual
                }
                Some('>') => {
                    self.read_char();
                    Token::Shr
                }
                _ => Token::GreaterThan,
            },
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '"' => Token::String(self.read_string()),
            '0'..='9' => {
                return Some(Ok(Token::Integer(self.read_int())));
            }
            '\0' => return None,
            ch if ch.is_alphanumeric() || ch == '_' => {
                let ident = self.read_ident();

                return Some(Ok(match ident.as_str() {
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
                }));
            }
            ch => {
                return Some(Err(Error::UnknownCharacter(ch)));
            }
        };

        self.read_char();

        Some(Ok(token))
    }
}

#[cfg(test)]
mod test {
    use super::{Error, Lexer};
    use crate::lexer::Token;

    #[test]
    fn source_into_tokens() -> Result<(), Error> {
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
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in tokens {
            let next_token = lexer.next().unwrap()?;

            assert_eq!(token, next_token);
        }

        Ok(())
    }
}
