use super::ast;

pub type PResult<T> = Result<T, ProgramError>;

#[derive(Debug, Clone)]
pub enum ErrorType {
    InternalError,
    Error,
    Warning,
}

pub use ErrorType::*;

#[derive(Debug, Clone)]
pub struct ProgramError {
    pub error_type: ErrorType,
    pub error_message: &'static str,
    pub span: Option<ast::Span>,
    pub highlight_message: Option<String>,
}

impl ProgramError {
    pub fn simple(msg: &'static str, span: ast::Span) -> Self {
        ProgramError {
            error_type: Error,
            error_message: msg,
            span: Some(span),
            highlight_message: None,
        }
    }
}

impl Default for ProgramError {
    fn default() -> Self {
        ProgramError {
            error_type: ErrorType::Error,
            error_message: "",
            span: None,
            highlight_message: None,
        }
    }
}

pub const UNBOUND_SYMBOL: &str = "Unbound symbol";
