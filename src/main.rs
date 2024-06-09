mod archs;
mod codegen;
mod lexer;
mod parser;
mod register_allocator;
mod scope;
mod symtable;

use crate::archs::Amd64;
use codegen::CodeGen;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let lexer = Lexer::new(
        "
        u8 main() {
            u16 foo;
            u8 bar;

            bar = 69;
            foo = bar;
        }
        "
        .to_string(),
    );

    let (stmts, symtable) = Parser::new(lexer)
        .into_parts()
        .unwrap_or_else(|e| giveup(Box::new(e)));

    dbg!(&stmts);
    dbg!(&symtable);

    CodeGen::<Amd64>::new(symtable)
        .compile(stmts, "./nasm/main.nasm")
        .unwrap_or_else(|e| giveup(Box::new(e)));
}

fn giveup(error: Box<dyn std::error::Error>) -> ! {
    println!("Achtung:  {}", error);
    std::process::exit(1)
}
