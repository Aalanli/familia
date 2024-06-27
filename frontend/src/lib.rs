pub mod prelude;
pub mod error;
pub mod ast;
mod ast_to_ir;
pub mod ir;
mod parser;
pub mod query;
pub mod transforms;

use std::cell::RefCell;

pub use parser::parse;
pub use ast_to_ir::ast_to_ir;
pub use transforms::transform_ir;


pub type PhaseResult<'a, T> = Result<T, &'a ModSource>;

#[derive(Debug)]
pub struct ModSource {
    pub text: String,
    file: Option<String>,
    errors: RefCell<Vec<error::ProgramError>>,
}

impl ModSource {
    pub fn new(file: Option<String>, text: String) -> Self {
        ModSource {
            file,
            text,
            errors: RefCell::new(Vec::new()),
        }
    }

    pub fn add_err(&self, err: error::ProgramError) {
        self.errors.borrow_mut().push(err);
    }

    pub fn commit_error<T>(&self, t: T) -> PhaseResult<T> {
        if self.errors.borrow().is_empty() {
            Ok(t)
        } else {
            Err(self)
        }
    }
}

impl From<&str> for ModSource {
    fn from(text: &str) -> Self {
        ModSource::new(None, text.to_string())
    }
}

