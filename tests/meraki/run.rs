use meraki::compile::{compile, CompileArgs};
use std::{path::Path, process::Output};

pub fn run(path: &Path) -> std::io::Result<Output> {
    let executable = path.with_extension("");
    let args = CompileArgs {
        file: path.to_path_buf(),
        output: Some(executable.clone()),
        object_only: false,
        assembly_only: false,
        macro_libs: Vec::new(),
    };

    compile(args).unwrap();

    let output = std::process::Command::new(&executable)
        .spawn()
        .unwrap()
        .wait_with_output()
        .unwrap();

    std::fs::remove_file(&executable)?;

    Ok(output)
}
