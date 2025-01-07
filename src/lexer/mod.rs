mod token;

use span::Span;
pub use token::TokenKind;

pub mod span {
    #[derive(Debug, Clone, PartialEq)]
    pub struct Span {
        pub start: usize,
        pub end: usize,
    }

    impl Span {
        pub fn to(self, end: Span) -> Span {
            Span {
                start: std::cmp::min(self.start, end.start),
                end: std::cmp::max(self.end, end.end),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lexer<'src> {
    input: &'src str,
    position: usize,
    read_position: usize,
    ch: char,
    start: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        let mut lexer = Self {
            input,
            ch: '\0',
            position: 0,
            read_position: 0,
            start: 0,
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

    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.position,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token, Span>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        self.start = self.position;

        let kind = match self.ch {
            '=' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }
            '-' => {
                if self.peek() == Some('>') {
                    self.read_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '+' => TokenKind::Plus,
            '/' => {
                if self.peek() == Some('/') {
                    self.skip_comment();

                    return self.next();
                } else {
                    TokenKind::Slash
                }
            }
            '.' => TokenKind::Period,
            '~' => TokenKind::Tilde,
            '&' => {
                if self.peek() == Some('&') {
                    self.read_char();
                    TokenKind::And
                } else {
                    TokenKind::Ampersand
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.read_char();
                    TokenKind::Or
                } else {
                    TokenKind::Bar
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.read_char();
                    TokenKind::NotEqual
                } else {
                    TokenKind::Bang
                }
            }
            '*' => TokenKind::Asterisk,
            '<' => match self.peek() {
                Some('=') => {
                    self.read_char();
                    TokenKind::LessEqual
                }
                Some('<') => {
                    self.read_char();
                    TokenKind::Shl
                }
                _ => TokenKind::LessThan,
            },
            '>' => match self.peek() {
                Some('=') => {
                    self.read_char();
                    TokenKind::GreaterEqual
                }
                Some('>') => {
                    self.read_char();
                    TokenKind::Shr
                }
                _ => TokenKind::GreaterThan,
            },
            ';' => TokenKind::Semicolon,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '"' => TokenKind::String(self.read_string()),
            '0'..='9' => {
                let num = self.read_int();

                return Some(Ok(Token {
                    kind: TokenKind::Integer(num),
                    span: self.span(),
                }));
            }
            '\0' => return None,
            ch if ch.is_alphanumeric() || ch == '_' => {
                let ident = self.read_ident();

                return Some(Ok(Token {
                    kind: match ident.as_str() {
                        "const" => TokenKind::Const,
                        "true" => TokenKind::True,
                        "let" => TokenKind::Let,
                        "fn" => TokenKind::Fn,
                        "enum" => TokenKind::Enum,
                        "struct" => TokenKind::Struct,
                        "false" => TokenKind::False,
                        "if" => TokenKind::If,
                        "while" => TokenKind::While,
                        "for" => TokenKind::For,
                        "else" => TokenKind::Else,
                        "return" => TokenKind::Return,
                        "as" => TokenKind::As,
                        "continue" => TokenKind::Continue,
                        "break" => TokenKind::Break,
                        "u8" => TokenKind::U8,
                        "u16" => TokenKind::U16,
                        "u32" => TokenKind::U32,
                        "u64" => TokenKind::U64,
                        "i8" => TokenKind::I8,
                        "i16" => TokenKind::I16,
                        "i32" => TokenKind::I32,
                        "i64" => TokenKind::I64,
                        "usize" => TokenKind::Usize,
                        "isize" => TokenKind::Isize,
                        "bool" => TokenKind::Bool,
                        "void" => TokenKind::Void,
                        "NULL" => TokenKind::Null,
                        _ => TokenKind::Ident(ident),
                    },
                    span: self.span(),
                }));
            }
            _ => {
                self.read_char();

                return Some(Err(self.span()));
            }
        };

        self.read_char();

        Some(Ok(Token {
            kind,
            span: self.span(),
        }))
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use crate::lexer::TokenKind;

    #[test]
    fn source_into_tokens() {
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
            TokenKind::Ident(String::from("ident")),
            TokenKind::Integer(String::from("69")),
            TokenKind::String(String::from("string")),
            TokenKind::Assign,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Bang,
            TokenKind::Asterisk,
            TokenKind::Slash,
            TokenKind::Arrow,
            TokenKind::Period,
            TokenKind::Tilde,
            TokenKind::Ampersand,
            TokenKind::Bar,
            TokenKind::Equal,
            TokenKind::NotEqual,
            TokenKind::LessThan,
            TokenKind::GreaterThan,
            TokenKind::LessEqual,
            TokenKind::GreaterEqual,
            TokenKind::And,
            TokenKind::Or,
            TokenKind::Shl,
            TokenKind::Shr,
            TokenKind::Comma,
            TokenKind::Semicolon,
            TokenKind::Colon,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::Const,
            TokenKind::True,
            TokenKind::False,
            TokenKind::Let,
            TokenKind::Fn,
            TokenKind::Enum,
            TokenKind::Struct,
            TokenKind::If,
            TokenKind::While,
            TokenKind::For,
            TokenKind::Else,
            TokenKind::Return,
            TokenKind::As,
            TokenKind::Continue,
            TokenKind::Break,
            TokenKind::U8,
            TokenKind::U16,
            TokenKind::U32,
            TokenKind::U64,
            TokenKind::I8,
            TokenKind::I16,
            TokenKind::I32,
            TokenKind::I64,
            TokenKind::Usize,
            TokenKind::Isize,
            TokenKind::Bool,
            TokenKind::Void,
            TokenKind::Null,
        ];

        let mut lexer = Lexer::new(input);

        for kind in tokens {
            let next_token = lexer.next().unwrap().unwrap();

            assert_eq!(kind, next_token.kind);
        }
    }
}
