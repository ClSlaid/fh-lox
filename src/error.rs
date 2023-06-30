use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum InterpreterError {
    SyntaxError(#[from] SyntaxError),
    RuntimeError(#[from] RuntimeError),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            InterpreterError::SyntaxError(s) => s.fmt(f),
            InterpreterError::RuntimeError(r) => r.fmt(f),
        }
    }
}

/// error in parser
#[derive(Debug, Clone, Error)]
pub enum SyntaxError {
    UnterminatedString(String),
    InvalidNumber(String),
    UnrecognizedToken(String),
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            SyntaxError::UnterminatedString(s) => {
                write!(f, "Unterminated string: {}", s)
            }
            SyntaxError::UnrecognizedToken(s) => {
                write!(f, "Unrecognized token: {}", s)
            }
            SyntaxError::InvalidNumber(s) => {
                write!(f, "Invalid number: {}", s)
            }
        }
    }
}

/// runtime error
#[derive(Debug, Clone, Error)]
pub enum RuntimeError {
    StackOverflow,
    UndefinedVariable(String),
    // TODO: implement type checker
    InappropriateType(String),
    IOError(#[from] Rc<std::io::Error>),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::StackOverflow => {
                write!(f, "stack overflow!")
            }
            RuntimeError::InappropriateType(s) => {
                write!(f, "InappropriateType: {}", s)
            }
            RuntimeError::UndefinedVariable(s) => {
                write!(f, "UndefinedVariable: {}", s)
            }
            RuntimeError::IOError(e) => {
                write!(f, "IOError: {:?}", e)
            }
        }
    }
}
