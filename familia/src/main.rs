
use clap::{Parser, ValueEnum};

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
    DumpLLVM
}

fn main() {
    let args = Cli::parse();
    println!("{:?}", args);
}
