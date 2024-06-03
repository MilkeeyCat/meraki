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
        u8 bar;

        baz = 1;
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

    CodeGen::new(symtable).compile(stmts, "./nasm/main.nasm");
}
