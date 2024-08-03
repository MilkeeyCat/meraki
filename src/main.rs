use clap::Parser;
use meraki::{
    archs::{Amd64, Architecture},
    codegen::CodeGen,
    lexer::Lexer,
    parser,
};
use std::{fs::File, io::Read};

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    for filename in args.files {
        let mut file = File::open(&filename).expect(&format!("Failed to open file: {}", filename));
        let mut source_code = String::new();

        file.read_to_string(&mut source_code)
            .expect(&format!("Failed to read contents of file: {}", filename));

        let lexer = Lexer::new(source_code);
        let (stmts, scope) = parser::Parser::new(lexer)?.into_parts()?;

        dbg!(&stmts);
        dbg!(&scope);

        CodeGen::new(&mut Amd64::new(), scope).compile(stmts, &args.output)?;
    }

    Ok(())
}
