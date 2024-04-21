mod codegen;
mod lexer;
mod parser;
mod symtable;

use codegen::CodeGen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new(
        "
        5 + 5;
        return 0;
        "
        .to_string(),
    );

    let mut parser = Parser::new(lexer);
    let statements = parser.parse_statements();
    dbg!(&statements);
    CodeGen::new("./nasm/main.nasm", statements, parser.symtable).generate();
}
