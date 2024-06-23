use anyhow::{anyhow, Error};

pub mod query;
pub mod ast;
mod ast_to_ir;
pub mod ir;
mod parser;
pub mod transforms;

pub use ir::IR;

pub use ast_to_ir::ast_to_ir;
pub use parser::parse;
pub use transforms::transform_ir;

// Text -> AST
// (Text, AST) -> Program
// Program -> IR
// (IR, Program) -> IR (typecheck)
// IR -> IR (transforms/optimizations)
// IR -> LLVM IR

#[derive(Clone, Debug)]
pub struct Program {
    pub file: Option<String>,
    pub text: String,
    pub ast: ast::Decl,
}

impl Program {
    pub fn err(&self, _span: ast::Span, msg: &str) -> Error {
        // TODO: better error message
        anyhow!("{msg}")
    }
}

pub enum ErrorType {
    Error,
    Warning
}

pub struct ProgramError {
    pub error_type: ErrorType,
    pub error_message: String,
    pub file: Option<String>,
    pub span: Option<ast::Span>,
    pub highlight_message: Option<String>,
}

impl Default for ProgramError {
    fn default() -> Self {
        ProgramError {
            error_type: ErrorType::Error,
            error_message: String::new(),
            file: None,
            span: None,
            highlight_message: None,
        }
    }
}

pub struct ErrorManager {
    pub file: Option<String>,
    pub lines: Vec<String>,
    pub errors: Vec<ProgramError>,
}

impl ErrorManager {
    pub fn new() -> Self { 
        todo!()
    }

    pub fn err(&mut self, err: ProgramError) {
        self.errors.push(err);
    }
}
