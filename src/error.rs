use std::fmt::{self, Display, Formatter};
use thiserror::Error;

use crate::token::InvalidToken;

#[derive(Debug, Clone, Error)]
pub enum InterpreterError {
    InvalidToken(#[from] InvalidToken),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            InterpreterError::InvalidToken(err) => err.fmt(f),
        }
    }
}
