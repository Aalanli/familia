use std::io::Write;

use clap::{Parser, ValueEnum};

use familia_codegen as codegen;
use familia_frontend as frontend;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// The input file, ending in .fm
    file: String,
    #[arg(value_enum, default_value_t = Mode::Exe)]
    mode: Mode,
    #[arg(value_enum, default_value_t = OptLevel::None)]
    opt_level: OptLevel,
    /// The output file
    #[arg(short, long)]
    output: Option<String>,
}

#[derive(Copy, Clone, Debug, ValueEnum, PartialEq, Eq)]
enum Mode {
    /// dump the ir
    DumpIR,
    /// dump the llvm ir
    DumpLLVM,
    /// output as an executable file
    Exe,
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
    let mut ir = frontend::ast_to_ir(src, ast).unwrap();
    let opt = match args.opt_level {
        OptLevel::None => codegen::OptLevel::None,
        OptLevel::O1 => codegen::OptLevel::O1,
    };
    frontend::transform_ir(&mut ir).unwrap();

    let (object_file, exe_file) = if let Some(output) = &args.output {
        if args.mode == Mode::Exe { // remove the object file if we generate an executable
            (output.to_string() + ".o", output.to_string())
        } else { // otherwise we just straight up dump the ir/llvm to the file
            (output.to_string(), output.to_string())
        }
    } else if let Mode::Exe = args.mode {
        ("a.o".to_string(), "a.out".to_string())
    } else {
        ("".to_string(), "".to_string())
    };

    let mut ostream = if let Some(_) = &args.output {
        Box::new(std::fs::File::create(&object_file).unwrap()) as Box<dyn Write>
    } else if let Mode::Exe = args.mode {
        Box::new(std::fs::File::create(&object_file).unwrap()) as Box<dyn Write>
    } else {
        Box::new(std::io::stdout()) as Box<dyn Write>
    };

    match args.mode {
        Mode::DumpIR => ostream
            .write_all(frontend::ir::print_basic(&ir).as_bytes())
            .unwrap(),
        Mode::DumpLLVM => {
            let mut options = codegen::CodeGenOptions {
                opt_level: opt,
                add_rts: false,
                write_obj: false,
                output: &mut ostream,
            };
            codegen::generate_llvm(&ir, &mut options)
        }
        Mode::Exe => {
            let mut options = codegen::CodeGenOptions {
                opt_level: opt,
                add_rts: true,
                write_obj: true,
                output: &mut ostream,
            };
            codegen::generate_llvm(&ir, &mut options);
            ostream.flush().unwrap();
            codegen::object_to_executable(&object_file, &exe_file);
            std::fs::remove_file(&object_file).unwrap();
        }
    }
}
