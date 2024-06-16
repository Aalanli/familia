use anyhow::{anyhow, Error, Result};

pub mod ast;
pub mod ir;
mod ast_to_ir;
mod parser;
mod printer;

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
