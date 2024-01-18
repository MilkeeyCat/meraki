mod lexer;
mod parser;
mod span;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new(
        r#"
        2 * 4 + 5 / 2
        "#
        .to_string(),
    );

    let mut parser = Parser::new(lexer);
    dbg!(parser.bin_expr());

    //let mut tok = lexer.next_token().unwrap();
    //while tok.clone().token_type != TokenType::Eof {
    //    dbg!(&tok);
    //    tok = lexer.next_token().unwrap();
    //}
}
