#![allow(dead_code)]

use std::fmt::Display;

pub use crate::types::Spanned;

#[derive(Clone, Debug)]
pub enum Expr {
    Unary(ExprUnary),
    Binary(ExprBinary),
    Literal(ExprLiteral),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unary(u) => write!(f, "{}", u),
            Expr::Binary(b) => write!(f, "{}", b),
            Expr::Literal(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprUnary {
    pub op: OpPrefix,
    pub rhs: Box<Expr>,
}

impl Display for ExprUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.rhs)
    }
}

#[derive(Clone, Debug)]
pub struct ExprBinary {
    pub op: OpInfix,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Display for ExprBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op {
            OpInfix::Assign => write!(f, "{} = {}", self.lhs, self.rhs),
            _ => write!(f, "{} {} {}", self.lhs, self.op, self.rhs),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprLiteral {
    Str(String),
    Bool(bool),
    Num(f64),
    Nil,
}

impl Display for ExprLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprLiteral::Str(s) => write!(f, "\"{}\"", s),
            ExprLiteral::Bool(b) => write!(f, "{}", b),
            ExprLiteral::Num(n) => write!(f, "{}", n),
            ExprLiteral::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OpInfix {
    Add,
    Sub,
    Mul,
    Div,

    Assign,

    Gt,
    Lt,
    Eq,
    Ne,
    Ge,
    Le,

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
            OpInfix::Lt => write!(f, "<"),
            OpInfix::Eq => write!(f, "=="),
            OpInfix::Ne => write!(f, "!="),
            OpInfix::Ge => write!(f, ">="),
            OpInfix::Le => write!(f, "<="),
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
