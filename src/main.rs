mod codegen;
mod lexer;
mod parser;
mod symtable;

use codegen::CodeGen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new("return 5;".to_string());

    let mut parser = Parser::new(lexer);
    let exprs = parser.statements();
    CodeGen::new("./nasm/main.nasm", exprs, parser.symtable).generate();
}
