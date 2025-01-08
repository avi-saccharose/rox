use std::{
    error::Error,
    fmt::{self},
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum RoxError {
    CompileError,
    RuntimeError,
    LexerError,
    ParseError,
}

impl Error for RoxError {}

impl fmt::Display for RoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Error")
    }
}
