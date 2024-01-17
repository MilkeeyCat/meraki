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
                let end = self.lc();

                Token::new(TokenType::Minus, Span::new(start, end))
            }
            b'+' => {
                let end = self.lc();

                Token::new(TokenType::Plus, Span::new(start, end))
            }
            b'/' => {
                let end = self.lc();

                Token::new(TokenType::Slash, Span::new(start, end))
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
                let end = self.lc();

                Token::new(TokenType::LessThan, Span::new(start, end))
            }
            b'>' => {
                let end = self.lc();

                Token::new(TokenType::GreaterThan, Span::new(start, end))
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
                    "fn" => Token::new(TokenType::Function, Span::new(start, end)),
                    "let" => Token::new(TokenType::Let, Span::new(start, end)),
                    "const" => Token::new(TokenType::Const, Span::new(start, end)),
                    "true" => Token::new(TokenType::True, Span::new(start, end)),
                    "enum" => Token::new(TokenType::Enum, Span::new(start, end)),
                    "impl" => Token::new(TokenType::Impl, Span::new(start, end)),
                    "struct" => Token::new(TokenType::Struct, Span::new(start, end)),
                    "false" => Token::new(TokenType::False, Span::new(start, end)),
                    "if" => Token::new(TokenType::If, Span::new(start, end)),
                    "else" => Token::new(TokenType::Else, Span::new(start, end)),
                    "return" => Token::new(TokenType::Return, Span::new(start, end)),
                    _ => Token::new(TokenType::Ident(ident), Span::new(start, end)),
                });
            }
            b'0'..=b'9' => {
                let int = self.read_int();
                let end = self.lc();

                return Ok(Token::new(TokenType::Int(int), Span::new(start, end)));
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
