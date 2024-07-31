mod archs;
mod codegen;
mod lexer;
mod parser;
mod register_allocator;
mod scope;
mod symbol_table;
mod type_;
mod type_table;

use std::{fs::File, io::Read};

use crate::archs::{Amd64, Architecture};
use clap::Parser;
use codegen::CodeGen;
use lexer::Lexer;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source code filenames
    #[arg(required = true, num_args = 1..)]
    files: Vec<String>,

    /// Output file
    #[arg(short, default_value = "main.s")]
    output: String,
}

fn main() {
    let args = Args::parse();

    for filename in args.files {
        let mut file = File::open(&filename).expect(&format!("Failed to open file: {}", filename));
        let mut source_code = String::new();

        file.read_to_string(&mut source_code)
            .expect(&format!("Failed to read contents of file: {}", filename));

        let lexer = Lexer::new(source_code);
        let (stmts, scope) = parser::Parser::new(lexer)
            .unwrap_or_else(|e| giveup(Box::new(e)))
            .into_parts()
            .unwrap_or_else(|e| giveup(Box::new(e)));

        dbg!(&stmts);
        dbg!(&scope);

        CodeGen::new(&mut Amd64::new(), scope)
            .compile(stmts, &args.output)
            .unwrap_or_else(|e| giveup(Box::new(e)));
    }
}

fn giveup(error: Box<dyn std::error::Error>) -> ! {
    println!("Achtung:  {}", error);
    std::process::exit(1)
}
