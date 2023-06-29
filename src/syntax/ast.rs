#![allow(dead_code)]

use std::fmt::Display;

pub use crate::types::Spanned;

#[derive(Clone, Debug)]
pub enum Expr {
    Unary(ExprUnary),
    Binary(ExprBinary),
    Literal(ExprLiteral),
}

#[derive(Clone, Debug)]
pub struct ExprUnary {
    pub op: OpPrefix,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprBinary {
    pub op: OpInfix,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprLiteral {
    Str(String),
    Bool(bool),
    Num(f64),
    Nil,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpInfix {
    Add,
    Sub,
    Mul,
    Div,

    Assign,

    Gt,
    Ls,
    Eq,
    NEq,
    GEq,
    LEq,

    And,
    Or,
}

impl Display for OpInfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpInfix::Add => write!(f, "+"),
            OpInfix::Sub => write!(f, "-"),
            OpInfix::Mul => write!(f, "*"),
            OpInfix::Div => write!(f, "/"),
            OpInfix::Assign => write!(f, "="),
            OpInfix::Gt => write!(f, ">"),
            OpInfix::Ls => write!(f, "<"),
            OpInfix::Eq => write!(f, "=="),
            OpInfix::NEq => write!(f, "!="),
            OpInfix::GEq => write!(f, ">="),
            OpInfix::LEq => write!(f, "<="),
            OpInfix::And => write!(f, "&&"),
            OpInfix::Or => write!(f, "||"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpPrefix {
    Neg,
    Not,
}

impl Display for OpPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpPrefix::Neg => write!(f, "-"),
            OpPrefix::Not => write!(f, "!"),
        }
    }
}
