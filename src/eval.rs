use std::fmt::Display;

use crate::{
    error::RuntimeError,
    syntax::{Expr, ExprBinary, ExprLiteral, ExprUnary, OpInfix, OpPrefix},
};

#[derive(Debug, Clone)]
pub enum EvalValue {
    Num(f64),
    Bool(bool),
    Str(String),
    Unit,
    Nil,
    // TODO: support these types
    // Tuple(Vec<EvalValue>),
    // NewType(String, Box<EvalValue>),
}

impl Display for EvalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalValue::Num(n) => write!(f, "{}", n),
            EvalValue::Str(s) => write!(f, "\"{}\"", s),
            EvalValue::Bool(b) => write!(f, "{}", b),
            EvalValue::Unit => write!(f, "()"),
            EvalValue::Nil => write!(f, "nil"),
        }
    }
}

pub fn eval(expr: &Expr) -> Result<EvalValue, RuntimeError> {
    eval_expr(expr)
}

fn eval_expr(expr: &Expr) -> Result<EvalValue, RuntimeError> {
    match expr {
        Expr::Unary(ExprUnary { op, rhs }) => {
            let rhs_val = eval_expr(rhs)?;
            match op {
                OpPrefix::Neg => {
                    if let EvalValue::Num(n) = rhs_val {
                        let v = EvalValue::Num(-n);
                        return Ok(v);
                    }
                }
                OpPrefix::Not => {
                    if let EvalValue::Bool(b) = rhs_val {
                        let v = EvalValue::Bool(!b);
                        return Ok(v);
                    }
                }
            }
            let msg = format!("cannot apply operator {} to {}", op, rhs_val);
            Err(RuntimeError::InappropriateType(msg))
        }
        Expr::Binary(ExprBinary { lhs, op, rhs }) => {
            let lhs_val = eval_expr(lhs)?;
            let rhs_val = eval_expr(rhs)?;
            match (lhs_val, rhs_val) {
                (EvalValue::Num(l), EvalValue::Num(r)) => match op {
                    OpInfix::Add => Ok(EvalValue::Num(l + r)),
                    OpInfix::Sub => Ok(EvalValue::Num(l - r)),
                    OpInfix::Mul => Ok(EvalValue::Num(l * r)),
                    OpInfix::Div => Ok(EvalValue::Num(l / r)),
                    OpInfix::Eq => Ok(EvalValue::Bool(l == r)),
                    OpInfix::Ne => Ok(EvalValue::Bool(l != r)),
                    OpInfix::Gt => Ok(EvalValue::Bool(l > r)),
                    OpInfix::Lt => Ok(EvalValue::Bool(l < r)),
                    OpInfix::Ge => Ok(EvalValue::Bool(l >= r)),
                    OpInfix::Le => Ok(EvalValue::Bool(l <= r)),
                    _ => {
                        let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                        Err(RuntimeError::InappropriateType(msg))
                    }
                },
                (EvalValue::Bool(l), EvalValue::Bool(r)) => match op {
                    OpInfix::Eq => Ok(EvalValue::Bool(l == r)),
                    OpInfix::Ne => Ok(EvalValue::Bool(l != r)),
                    OpInfix::And => Ok(EvalValue::Bool(l && r)),
                    OpInfix::Or => Ok(EvalValue::Bool(l || r)),
                    _ => {
                        let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                        Err(RuntimeError::InappropriateType(msg))
                    }
                },
                (EvalValue::Str(l), EvalValue::Str(r)) => match op {
                    OpInfix::Eq => Ok(EvalValue::Bool(l == r)),
                    OpInfix::Ne => Ok(EvalValue::Bool(l != r)),
                    OpInfix::Add => Ok(EvalValue::Str(l + &r)),
                    OpInfix::Gt => Ok(EvalValue::Bool(l > r)),
                    OpInfix::Ge => Ok(EvalValue::Bool(l >= r)),
                    OpInfix::Lt => Ok(EvalValue::Bool(l < r)),
                    OpInfix::Le => Ok(EvalValue::Bool(l <= r)),
                    _ => {
                        let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                        Err(RuntimeError::InappropriateType(msg))
                    }
                },
                (EvalValue::Unit, EvalValue::Unit) => match op {
                    OpInfix::Eq => Ok(EvalValue::Bool(true)),
                    OpInfix::Ne => Ok(EvalValue::Bool(false)),
                    _ => {
                        let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                        Err(RuntimeError::InappropriateType(msg))
                    }
                },
                (EvalValue::Nil, EvalValue::Nil) => match op {
                    OpInfix::Eq => Ok(EvalValue::Bool(true)),
                    OpInfix::Ne => Ok(EvalValue::Bool(false)),
                    _ => {
                        let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                        Err(RuntimeError::InappropriateType(msg))
                    }
                },
                _ => {
                    let msg = format!("cannot apply operator {} to {} and {}", op, lhs, rhs);
                    Err(RuntimeError::InappropriateType(msg))
                }
            }
        }
        Expr::Literal(lit) => {
            let v = match lit {
                ExprLiteral::Str(s) => EvalValue::Str(s.clone()),
                ExprLiteral::Bool(b) => EvalValue::Bool(*b),
                ExprLiteral::Num(n) => EvalValue::Num(*n),
                ExprLiteral::Nil => EvalValue::Nil,
            };
            Ok(v)
        }
    }
}

#[cfg(test)]
mod eval_tests {
    use crate::syntax::{ExprParser, SourceLexer};

    use super::*;
    fn evaled_expr_source(source: &str) -> Result<EvalValue, RuntimeError> {
        let lexer = SourceLexer::new(source);
        let mut errors = Vec::new();
        let parsed = ExprParser::new().parse(&mut errors, lexer);
        let expr = parsed.unwrap();
        eval(&expr)
    }

    #[test]
    fn test_eval_str() {
        let source = r#""1""#;
        let evaled = evaled_expr_source(source);
        assert!(evaled.is_ok());
        let v = evaled.unwrap();
        let EvalValue::Str(s) = v else {
            panic!("expected string, got {:?}", v);
        };
        assert_eq!(s, "1");

        // test concatenation
        let source = r#""1" + "2""#;
        let evaled = evaled_expr_source(source);
        assert!(evaled.is_ok());
        let v = evaled.unwrap();
        let EvalValue::Str(s) = v else {
            panic!("expected string, got {:?}", v);
        };
        assert_eq!(s, "12");
    }
    #[test]
    fn test_eval_calc() {
        let source = r#"1 + 2 + (3 + 5) * 6"#;
        let evaled = evaled_expr_source(source);
        assert!(evaled.is_ok());
        let v = evaled.unwrap();
        let EvalValue::Num(n) = v else {
            panic!("expected number, got {:?}", v);
        };
        assert!((n - 51.0).abs() < 0.0001);
    }
    #[test]
    fn test_eval_logic() {
        let source = r#"1 + 2 + (3 + 5) * 6 > 10"#;
        let evaled = evaled_expr_source(source);
        assert!(evaled.is_ok());
        let v = evaled.unwrap();
        let EvalValue::Bool(b) = v else {
            panic!("expected bool, got {:?}", v);
        };
        assert!(b);
    }
}
