use clap::{Parser, ValueEnum};

use familia_frontend::{parse, ir};
use familia_codegen::generate_llvm;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// The output file
    #[arg(short, long)]
    output: Option<String>,
    #[arg(value_enum)]
    mode: Mode,
    /// The input file, ending in .fm
    file: String,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Mode {
    /// dump the ir
    DumpIR,
    /// dump the llvm ir
    DumpLLVM,
}

fn main() {
    let args = Cli::parse();
    let program = std::fs::read_to_string(&args.file).unwrap();
    let ast = parse(&program).unwrap();
    let ir = ir::ast_to_ir(&ast).unwrap();
    let result = match args.mode {
        Mode::DumpIR => ir::dump_ir(&ir),
        Mode::DumpLLVM => generate_llvm(&ir).unwrap(),
    };

    match args.output {
        Some(output) => std::fs::write(output, result).unwrap(),
        None => println!("{}", result),
    }
}
