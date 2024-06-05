mod archs;
mod codegen;
mod lexer;
mod parser;
mod register_allocator;
mod symtable;

use codegen::CodeGen;
use lexer::Lexer;
use parser::Parser;

use crate::archs::Amd64;

fn main() {
    let lexer = Lexer::new(
        "
        bool bar;
        bool bar1;
        bool bar2;
        bar = true;
        bar = false;
        "
        .to_string(),
    );

    let parser = Parser::new(lexer);
    let (stmts, symtable) = parser.into_parts();
    let stmts = match stmts {
        Ok(stmts) => stmts,
        Err(e) => {
            println!("Achtung:  {}", e);
            std::process::exit(1)
        }
    };

    dbg!(&stmts);
    dbg!(&symtable);

    CodeGen::<Amd64>::new(symtable).compile(stmts, "./nasm/main.nasm");
}
