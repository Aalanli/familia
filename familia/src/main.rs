use clap::{Parser, ValueEnum};

use familia_codegen as codegen;
use familia_frontend as frontend;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    
    #[arg(value_enum)]
    mode: Mode,
    /// The input file, ending in .fm
    file: String,

    #[arg(value_enum, default_value_t = OptLevel::None)]
    opt_level: OptLevel,
    /// The output file
    #[arg(short, long)]
    output: Option<String>,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Mode {
    /// dump the ir
    DumpIR,
    /// dump the llvm ir
    DumpLLVM,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum OptLevel {
    /// no optimizations
    None,
    /// -O1
    O1,
}

fn main() {
    let args = Cli::parse();
    let text = std::fs::read_to_string(&args.file).unwrap();
    let src = frontend::ModSource::new(Some(args.file.clone()), text);
    let ast = frontend::parse(&src).unwrap();
    let mut ir = frontend::ast_to_ir(&src, &ast).unwrap();
    let opt = match args.opt_level {
        OptLevel::None => codegen::OptLevel::None,
        OptLevel::O1 => codegen::OptLevel::O1,
    };
    frontend::transform_ir(&mut ir, &src).unwrap();
    let result = match args.mode {
        Mode::DumpIR => frontend::ir::print_basic(&ir),
        Mode::DumpLLVM => codegen::generate_llvm(&ir, opt).unwrap(),
    };

    match args.output {
        Some(output) => std::fs::write(output, result).unwrap(),
        None => println!("{}", result),
    }
}
