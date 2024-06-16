use anyhow::{anyhow, Error};

pub mod ast;
mod ast_to_ir;
pub mod ir;
mod parser;
mod printer;

pub use ast_to_ir::ast_to_ir;
pub use ir::IR;
pub use parser::parse;

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
