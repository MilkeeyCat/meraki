use super::Token;

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

    pub fn next_token(&mut self) -> Result<Token, Box<dyn std::error::Error>> {
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
            b'/' => Token::Slash,
            b'.' => Token::Period,
            b'&' => Token::Ampersand,
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
                } else {
                    Token::LessThan
                }
            }
            b'>' => {
                if self.peek() == b'=' {
                    self.read_char();
                    Token::GreaterEqual
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
                    "enum" => Token::Enum,
                    "struct" => Token::Struct,
                    "false" => Token::False,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    "i8" => Token::I8,
                    "i16" => Token::I16,
                    "i32" => Token::I32,
                    "i64" => Token::I64,
                    "u8" => Token::U8,
                    "u16" => Token::U16,
                    "u32" => Token::U32,
                    "u64" => Token::U64,
                    "char" => Token::Char,
                    "bool" => Token::Bool,
                    "f32" => Token::Float32,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => {
                return Ok(Token::Integer(self.read_int()));
            }
            0 => Token::Eof,
            c => unreachable!("coudn't parse char {}, skill issue", char::from(c)),
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
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use crate::lexer::Token;

    #[test]
    fn source_into_tokens() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
            [>.<]->!-+/==!=:true,false<=>=

            const i8 a = 5;
            i8 *b = &a;
            char *str = "ddnet";

            if(false) {
            } else {
            }

            i8 foo() {
                return 69;
            }

            struct Foo {
            }

            enum Bar {
            }
        "#;

        let tokens = vec![
            Token::LBracket,
            Token::GreaterThan,
            Token::Period,
            Token::LessThan,
            Token::RBracket,
            Token::Arrow,
            Token::Bang,
            Token::Minus,
            Token::Plus,
            Token::Slash,
            Token::Equal,
            Token::NotEqual,
            Token::Colon,
            Token::True,
            Token::Comma,
            Token::False,
            Token::LessEqual,
            Token::GreaterEqual,
            Token::Const,
            Token::I8,
            Token::Ident(String::from("a")),
            Token::Assign,
            Token::Integer(String::from("5")),
            Token::Semicolon,
            Token::I8,
            Token::Asterisk,
            Token::Ident(String::from("b")),
            Token::Assign,
            Token::Ampersand,
            Token::Ident(String::from("a")),
            Token::Semicolon,
            Token::Char,
            Token::Asterisk,
            Token::Ident(String::from("str")),
            Token::Assign,
            Token::String(String::from("ddnet")),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::False,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::RBrace,
            Token::I8,
            Token::Ident(String::from("foo")),
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Integer(String::from("69")),
            Token::Semicolon,
            Token::RBrace,
            Token::Struct,
            Token::Ident(String::from("Foo")),
            Token::LBrace,
            Token::RBrace,
            Token::Enum,
            Token::Ident(String::from("Bar")),
            Token::LBrace,
            Token::RBrace,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in tokens {
            let next_token = lexer.next_token()?;
            println!("expected: {:?}, received: {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        Ok(())
    }
}
