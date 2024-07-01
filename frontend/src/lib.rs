pub mod ast;
mod ast_to_ir;
pub mod error;
pub mod ir;
mod parser;
pub mod prelude;
pub mod query;
pub mod transforms;

use std::{cell::RefCell, fmt::Display};

pub use ast_to_ir::ast_to_ir;
pub use parser::parse;
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

use colored::Colorize;
impl Display for ModSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lines = self.text.lines().collect::<Vec<_>>();
        for err in self.errors.borrow().iter() {
            match err.error_type {
                error::Error => write!(f, "{} : {}", "Error".red().bold(), err.error_message.red())?,
                error::Warning => write!(f, "{} : {}", "Warning".yellow().bold(), err.error_message.yellow())?,
                error::InternalError => write!(f, "{} : {}", "Internal Error".red().bold(), err.error_message.red())?,
            }
            write!(f, "\n")?;
            if let Some(file) = &self.file {
                write!(f, "> {}", file)?;
            }
            if let Some(span) = &err.span {
                if span.lhs.line == span.rhs.line {
                    write!(f, ":{}:{}\n", span.lhs.line, span.lhs.start)?;
                    let line = lines[span.lhs.line as usize - 1];
                    write!(f, "{}\n", line)?;
                    assert!(span.lhs.start <= span.rhs.start);
                    write!(f, "{}", " ".repeat(span.lhs.start as usize - 1))?;
                    write!(f, "{}", "^".repeat((span.rhs.start - span.lhs.start) as usize).yellow())?;
                    if let Some(highlight) = &err.highlight_message {
                        write!(f, "  {}", highlight.green())?;
                    }                    
                } else if span.lhs.line < span.rhs.line {
                    write!(f, ": {}:{} <=> {}:{}\n", span.lhs.line, span.lhs.start, span.rhs.line, span.rhs.start)?;
                    for i in span.lhs.line..=span.rhs.line {
                        let line = lines[i as usize - 1];
                        write!(f, "{}\n", line)?;
                    }
                    if let Some(highlight) = &err.highlight_message {
                        write!(f, "  {}", highlight.green())?;
                    }
                }
            }
            write!(f, "\n\n")?;
        }
        
        Ok(())
    }
}
