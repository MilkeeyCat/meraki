use crate::span::{LineColumn, Span};

use super::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
    line_column: LineColumn,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.into_bytes(),
            ch: 0,
            read_position: 0,
            position: 0,
            line_column: LineColumn::default(),
        };

        lexer.read_char();

        return lexer;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.line_column.column += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, Box<dyn std::error::Error>> {
        self.skip_whitespace();
        let start = self.lc();

        let tok = match self.ch {
            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();
                    let end = self.lc();

                    Token::new(TokenType::Equal, Span::new(start, end))
                } else {
                    let end = self.lc();

                    Token::new(TokenType::Assign, Span::new(start, end))
                }
            }
            b'-' => {
                if self.peek() == b'>' {
                    self.read_char();
                    let end = self.lc();

                    Token::new(TokenType::Arrow, Span::new(start, end))
                } else {
                    let end = self.lc();

                    Token::new(TokenType::Minus, Span::new(start, end))
                }
            }
            b'+' => {
                let end = self.lc();

                Token::new(TokenType::Plus, Span::new(start, end))
            }
            b'/' => {
                let end = self.lc();

                Token::new(TokenType::Slash, Span::new(start, end))
            }
            b'.' => {
                let end = self.lc();

                Token::new(TokenType::Period, Span::new(start, end))
            }
            b'&' => {
                let end = self.lc();

                Token::new(TokenType::Ampersand, Span::new(start, end))
            }
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    let end = self.lc();

                    Token::new(TokenType::NotEqual, Span::new(start, end))
                } else {
                    let end = self.lc();

                    Token::new(TokenType::Bang, Span::new(start, end))
                }
            }
            b'*' => {
                let end = self.lc();

                Token::new(TokenType::Asterisk, Span::new(start, end))
            }
            b'<' => {
                if self.peek() == b'=' {
                    self.read_char();
                    let end = self.lc();

                    Token::new(TokenType::LessEqual, Span::new(start, end))
                } else {
                    let end = self.lc();

                    Token::new(TokenType::LessThan, Span::new(start, end))
                }
            }
            b'>' => {
                if self.peek() == b'=' {
                    self.read_char();
                    let end = self.lc();

                    Token::new(TokenType::GreaterEqual, Span::new(start, end))
                } else {
                    let end = self.lc();

                    Token::new(TokenType::GreaterThan, Span::new(start, end))
                }
            }
            b';' => {
                let end = self.lc();

                Token::new(TokenType::Semicolon, Span::new(start, end))
            }
            b'(' => {
                let end = self.lc();

                Token::new(TokenType::LParen, Span::new(start, end))
            }
            b')' => {
                let end = self.lc();

                Token::new(TokenType::RParen, Span::new(start, end))
            }
            b'{' => {
                let end = self.lc();

                Token::new(TokenType::LBrace, Span::new(start, end))
            }
            b'}' => {
                let end = self.lc();

                Token::new(TokenType::RBrace, Span::new(start, end))
            }
            b'[' => {
                let end = self.lc();

                Token::new(TokenType::LBracket, Span::new(start, end))
            }
            b']' => {
                let end = self.lc();

                Token::new(TokenType::RBracket, Span::new(start, end))
            }
            b',' => {
                let end = self.lc();

                Token::new(TokenType::Comma, Span::new(start, end))
            }
            b':' => {
                let end = self.lc();

                Token::new(TokenType::Colon, Span::new(start, end))
            }
            b'"' => {
                let token_type = TokenType::String(self.read_string());
                let end = self.lc();

                Token::new(token_type, Span::new(start, end))
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                let end = self.lc();

                return Ok(match ident.as_str() {
                    "const" => Token::new(TokenType::Const, Span::new(start, end)),
                    "true" => Token::new(TokenType::True, Span::new(start, end)),
                    "enum" => Token::new(TokenType::Enum, Span::new(start, end)),
                    "struct" => Token::new(TokenType::Struct, Span::new(start, end)),
                    "false" => Token::new(TokenType::False, Span::new(start, end)),
                    "if" => Token::new(TokenType::If, Span::new(start, end)),
                    "else" => Token::new(TokenType::Else, Span::new(start, end)),
                    "return" => Token::new(TokenType::Return, Span::new(start, end)),
                    "int" => Token::new(TokenType::Int, Span::new(start, end)),
                    "char" => Token::new(TokenType::Char, Span::new(start, end)),
                    "bool" => Token::new(TokenType::Bool, Span::new(start, end)),
                    "float" => Token::new(TokenType::Float, Span::new(start, end)),
                    "double" => Token::new(TokenType::Double, Span::new(start, end)),
                    _ => Token::new(TokenType::Ident(ident), Span::new(start, end)),
                });
            }
            b'0'..=b'9' => {
                let int = self.read_int();
                let end = self.lc();

                return Ok(Token::new(TokenType::Integer(int), Span::new(start, end)));
            }
            0 => {
                let end = self.lc();

                Token::new(TokenType::Eof, Span::new(start, end))
            }
            c => unreachable!("coudn't parse char {}, skill issue", char::from(c)),
        };

        self.read_char();

        return Ok(tok);
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }

        return self.input[self.read_position];
    }

    fn lc(&self) -> LineColumn {
        return self.line_column.clone();
    }

    fn read_ident(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }

    fn read_int(&mut self) -> String {
        let pos = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;

        loop {
            self.read_char();

            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }

        return String::from_utf8_lossy(&self.input[pos..self.position]).to_string();
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            if self.ch == b'\n' {
                self.line_column.line += 1;
                self.line_column.column = 0;
            }

            self.read_char();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::TokenType;

    use super::Lexer;

    #[test]
    fn source_into_tokens() -> Result<(), Box<dyn std::error::Error>> {
        let input = r#"
            [>.<]->!-+/==!=:true,false<=>=

            const int a = 5;
            int *b = &a;
            char *str = "ddnet";

            if(false) {
            } else {
            }

            int foo() {
                return 69;
            }

            struct Foo {
            }

            enum Bar {
            }
        "#;

        let tokens = vec![
            TokenType::LBracket,
            TokenType::GreaterThan,
            TokenType::Period,
            TokenType::LessThan,
            TokenType::RBracket,
            TokenType::Arrow,
            TokenType::Bang,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Slash,
            TokenType::Equal,
            TokenType::NotEqual,
            TokenType::Colon,
            TokenType::True,
            TokenType::Comma,
            TokenType::False,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::Const,
            TokenType::Int,
            TokenType::Ident(String::from("a")),
            TokenType::Assign,
            TokenType::Integer(String::from("5")),
            TokenType::Semicolon,
            TokenType::Int,
            TokenType::Asterisk,
            TokenType::Ident(String::from("b")),
            TokenType::Assign,
            TokenType::Ampersand,
            TokenType::Ident(String::from("a")),
            TokenType::Semicolon,
            TokenType::Char,
            TokenType::Asterisk,
            TokenType::Ident(String::from("str")),
            TokenType::Assign,
            TokenType::String(String::from("ddnet")),
            TokenType::Semicolon,
            TokenType::If,
            TokenType::LParen,
            TokenType::False,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Else,
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Int,
            TokenType::Ident(String::from("foo")),
            TokenType::LParen,
            TokenType::RParen,
            TokenType::LBrace,
            TokenType::Return,
            TokenType::Integer(String::from("69")),
            TokenType::Semicolon,
            TokenType::RBrace,
            TokenType::Struct,
            TokenType::Ident(String::from("Foo")),
            TokenType::LBrace,
            TokenType::RBrace,
            TokenType::Enum,
            TokenType::Ident(String::from("Bar")),
            TokenType::LBrace,
            TokenType::RBrace,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in tokens {
            let next_token = lexer.next_token()?.token_type;
            println!("expected: {:?}, received: {:?}", token, next_token);
            assert_eq!(token, next_token);
        }

        return Ok(());
    }
}
