use anyhow::{anyhow, Error};

mod parser;
pub mod ast;
mod ast_to_ir;
pub mod ir;
pub mod transforms;

pub use ir::IR;

pub use parser::parse;
pub use ast_to_ir::ast_to_ir;
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
