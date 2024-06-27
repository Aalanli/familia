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

pub struct ModSource {
    file: Option<String>,
    lines: Vec<String>,
    errors: RefCell<Vec<error::ProgramError>>,
}

impl ModSource {
    pub fn new(file: Option<String>, text: String) -> Self {
        ModSource {
            file,
            lines: text.lines().map(|s| s.to_string()).collect(),
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



