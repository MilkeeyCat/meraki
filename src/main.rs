mod codegen;
mod lexer;
mod parser;
mod span;

use codegen::CodeGen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new("2 + 4 * 5 - 10 / 2".to_string());

    let mut parser = Parser::new(lexer);
    let ast = parser.bin_expr(0);
    CodeGen::new("./nasm/main.nasm", ast).generate();
}
