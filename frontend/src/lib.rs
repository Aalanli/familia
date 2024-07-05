pub mod ast;
pub mod error;
pub mod ir;
mod parser;
pub mod prelude;
pub mod query;
pub mod transforms;

use std::{cell::RefCell, collections::HashSet, fmt::{Debug, Display}, hash::Hash, rc::Rc};

pub use parser::parse;
pub use transforms::{ast_to_ir, transform_ir};

pub type PhaseResult<T> = Result<T, PhaseError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModSource {
    text: Rc<String>,
    file: Option<Rc<String>>,
    errors: Rc<RefCell<Vec<error::ProgramError>>>,
    inserted_errors: Rc<RefCell<HashSet<error::ProgramError>>>,
}

impl Hash for ModSource {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text.hash(state);
        self.file.hash(state);
    }
}

impl ModSource {
    pub fn new(file: Option<String>, text: String) -> Self {
        ModSource {
            file: file.map(Rc::new),
            text: Rc::new(text),
            errors: Rc::new(RefCell::new(Vec::new())),
            inserted_errors: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    pub fn add_err(&self, err: error::ProgramError) {
        if self.inserted_errors.borrow().contains(&err) {
            return;
        }
        self.inserted_errors.borrow_mut().insert(err.clone());
        self.errors.borrow_mut().push(err);
    }

    pub fn err(&self) -> PhaseError {
        let mut new_errors = vec![];
        std::mem::swap(&mut new_errors, &mut *self.errors.borrow_mut());
        self.inserted_errors.borrow_mut().clear();
        PhaseError {
            text: self.text.clone(),
            file: self.file.clone(),
            errors: new_errors,
        }
    }

    pub fn commit_error<T>(&self, t: T) -> PhaseResult<T> {
        if self.errors.borrow().is_empty() {
            Ok(t)
        } else {
            Err(self.err())
        }
    }
}

impl From<&str> for ModSource {
    fn from(text: &str) -> Self {
        ModSource::new(None, text.to_string())
    }
}

// #[derive(Debug)]
pub struct PhaseError {
    text: Rc<String>,
    file: Option<Rc<String>>,
    errors: Vec<error::ProgramError>,
}

impl Debug for PhaseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}", self)
    }
}

use colored::Colorize;
impl Display for PhaseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lines = self.text.lines().collect::<Vec<_>>();
        for err in self.errors.iter() {
            match err.error_type {
                error::Error => {
                    write!(f, "{} : {}", "Error".red().bold(), err.error_message.red())?
                }
                error::Warning => write!(
                    f,
                    "{} : {}",
                    "Warning".yellow().bold(),
                    err.error_message.yellow()
                )?,
                error::InternalError => write!(
                    f,
                    "{} : {}",
                    "Internal Error".red().bold(),
                    err.error_message.red()
                )?,
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
                    write!(f, "{}", " ".repeat(span.lhs.start as usize))?;
                    write!(
                        f,
                        "{}",
                        "^".repeat((span.rhs.start - span.lhs.start) as usize)
                            .yellow()
                    )?;
                    if let Some(highlight) = &err.highlight_message {
                        write!(f, "  {}", highlight.green())?;
                    }
                } else if span.lhs.line < span.rhs.line {
                    write!(
                        f,
                        ": {}:{} <=> {}:{}\n",
                        span.lhs.line, span.lhs.start, span.rhs.line, span.rhs.start
                    )?;
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
