use clap::Parser;
use meraki::compile::{compile, CompileArgs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let options = CompileArgs::parse();

    compile(options)
}
