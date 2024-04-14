mod codegen;
mod lexer;
mod parser;
mod symtable;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new("print 6 != 6;".to_string());

    let mut parser = Parser::new(lexer);
    parser.statements();
}
