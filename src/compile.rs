use crate::{
    Context,
    codegen::{Codegen, amd64_asm::Amd64Asm},
    diagnostics::Diagnostics,
    lexer::Lexer,
    lowering::Lowering,
    parser,
};
use bumpalo::Bump;
use clap::Parser;
use std::{
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    process::Stdio,
};

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

    Lowering::new(&mut ctx).lower(ast);
    ctx.ty_problem.solve(&ctx.ir);

    //MacroExpansion::new(args.macro_libs).run_pass(&mut stmts, &mut scope);
    //SymbolResolver::new(()).run_pass(&mut stmts, &mut scope)?;
    //TypeChecker::new(()).run_pass(&mut stmts, &mut scope)?;

    let codegen: &mut dyn Codegen = &mut Amd64Asm::new(&ctx);
    let code = codegen.compile()?;

    if args.assembly_only {
        let asm_filename = args.file.with_extension("s");
        let mut file = std::fs::File::create(&asm_filename)?;

        file.write_all(&code)?;

        return Ok(());
    }

    let obj_filename = args.file.with_extension("o");

    assemble(&code, &obj_filename)?;

    if args.object_only {
        return Ok(());
    }

    let binary_filename = if let Some(output) = args.output {
        output
    } else {
        "a.out".into()
    };

    link(&obj_filename, &binary_filename, args.shared)?;

    // Remove intermediate steps file
    std::fs::remove_file(&obj_filename)?;

    Ok(())
}

fn assemble(source: &[u8], output: &Path) -> std::io::Result<()> {
    let source = std::process::Command::new("echo")
        .stdout(Stdio::piped())
        .arg(std::str::from_utf8(source).unwrap())
        .spawn()?;

    let as_args = vec![
        "-msyntax=intel",
        "-mnaked-reg",
        "-o",
        output.to_str().unwrap(),
    ];

    std::process::Command::new("as")
        .args(as_args)
        .stdin(Stdio::from(source.stdout.unwrap()))
        .spawn()?
        .wait()?;

    Ok(())
}

fn link(input: &Path, output: &Path, shared: bool) -> std::io::Result<()> {
    const OBJ_PATH: &'static str = "/usr/lib/x86_64-linux-gnu";
    let linker = format!("{OBJ_PATH}/ld-linux-x86-64.so.2");
    let crt = format!("{OBJ_PATH}/crt1.o");

    let mut args = vec![
        "-dynamic-linker",
        &linker,
        &crt,
        "-lc",
        input.to_str().unwrap(),
        "-z",
        "noexecstack",
        "-o",
        output.to_str().unwrap(),
    ];

    if shared {
        args.push("-shared");
    }

    std::process::Command::new("ld")
        .args(args)
        .spawn()?
        .wait()?;

    Ok(())
}
