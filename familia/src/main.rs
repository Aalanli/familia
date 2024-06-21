use clap::{Parser, ValueEnum};

use familia_frontend as frontend;

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
    let text = std::fs::read_to_string(&args.file).unwrap();
    let ast = frontend::parse(text, Some(args.file.into())).unwrap();
    let ir = frontend::ast_to_ir(&ast).unwrap();
    // let result = match args.mode {
    //     Mode::DumpIR => frontend::ir::dump_ir(&ir),
    //     Mode::DumpLLVM => generate_llvm(&ir).unwrap(),
    // };

    // match args.output {
    //     Some(output) => std::fs::write(output, result).unwrap(),
    //     None => println!("{}", result),
    // }
}
