use crate::{Context, diagnostics::Diagnostics, lexer::Lexer, lowering, parser};
use bumpalo::Bump;
use clap::Parser;
use std::{fs::File, io::Read, path::PathBuf};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CompileArgs {
    /// Source code file to compile
    #[arg(required = true)]
    pub file: PathBuf,

    /// Output binary file name
    #[arg(short, default_value = "a.out")]
    pub output: Option<PathBuf>,

    /// Produce assembly output
    #[arg(short = 'S', default_value_t = false, group = "output_t")]
    pub assembly_only: bool,

    /// Compile and assemble but do not link
    #[arg(short = 'c', default_value_t = false, group = "output_t")]
    pub object_only: bool,

    #[arg(long = "macro")]
    pub macro_libs: Vec<String>,

    #[arg(long = "shared", default_value_t = false)]
    pub shared: bool,
}

pub fn compile(args: CompileArgs) -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::open(&args.file)?;
    let mut source_code = String::new();

    file.read_to_string(&mut source_code)?;

    let report_diag_and_exit = |diag: &Diagnostics| -> ! {
        println!("{diag}");

        std::process::exit(0x45)
    };

    let mut diagnostics = Diagnostics::new(&source_code);
    let lexer = Lexer::new(&source_code);
    let ast = match parser::Parser::new(lexer, &mut diagnostics).parse() {
        Ok(ast) => ast,
        Err(_) => report_diag_and_exit(&diagnostics),
    };

    if diagnostics.has_errors() {
        report_diag_and_exit(&mut diagnostics);
    }

    let allocator = Bump::new();
    let mut ctx = Context::new(&allocator);

    let package = lowering::lower(&mut ctx, ast);
    dbg!(&package);

    //let module = Lowering::new(&mut ctx).lower(ast);
    //codegen::compile(&ctx, &module);

    //dbg!(module);

    //MacroExpansion::new(args.macro_libs).run_pass(&mut stmts, &mut scope);
    //SymbolResolver::new(()).run_pass(&mut stmts, &mut scope)?;
    //TypeChecker::new(()).run_pass(&mut stmts, &mut scope)?;

    Ok(())
}
