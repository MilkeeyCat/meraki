use crate::{
    archs::{Amd64, Architecture},
    codegen::CodeGen,
    lexer::Lexer,
    parser,
};
use clap::Parser;
use std::{
    fs::File,
    io::{Read, Write},
    path::{Path, PathBuf},
    process::Stdio,
};

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct CompileArgs {
    /// Source code filenames
    #[arg(required = true, num_args = 1..)]
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
}

pub fn compile(args: CompileArgs) -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::open(&args.file)?;
    let mut source_code = String::new();

    file.read_to_string(&mut source_code)?;

    let lexer = Lexer::new(source_code);
    let (stmts, scope) = parser::Parser::new(lexer)?.into_parts()?;

    dbg!(&stmts);
    dbg!(&scope);

    let code = CodeGen::new(Box::new(Amd64::new()), scope).compile(stmts)?;

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

    link(&obj_filename, &binary_filename)?;

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

fn link(input: &Path, output: &Path) -> std::io::Result<()> {
    const OBJ_PATH: &'static str = "/usr/lib/x86_64-linux-gnu";

    std::process::Command::new("ld")
        .args([
            "-dynamic-linker",
            &format!("{OBJ_PATH}/ld-linux-x86-64.so.2"),
            &format!("{OBJ_PATH}/crt1.o"),
            "-lc",
            input.to_str().unwrap(),
            "-z",
            "noexecstack",
            "-o",
            output.to_str().unwrap(),
        ])
        .spawn()?
        .wait()?;

    Ok(())
}
