mod lexer;
mod span;

use lexer::{Lexer, TokenType};

fn main() {
    let mut lexer = Lexer::new(
        r#"
let a: u8 = 10;
        "#
        .to_string(),
    );

    let mut tok = lexer.next_token().unwrap();
    while tok.clone().token_type != TokenType::Eof {
        dbg!(&tok);
        tok = lexer.next_token().unwrap();
    }
}
