use std::{
    collections::HashMap,
    fmt::Display,
    io::{Read, Stdin, Stdout, Write},
    ops::Range,
    rc::Rc,
};

use crate::{
    error::RuntimeError,
    syntax::{
        Expr, ExprBinary, ExprLiteral, ExprUnary, OpInfix, OpPrefix, Program, Stmt, Var as VarStmt,
    },
};

#[derive(Debug)]
pub struct RuntimeContext<R, W>
where
    R: Read,
    W: Write,
{
    vars: HashMap<String, EvalValue>,
    #[allow(dead_code)]
    input: R,
    output: W,
}

// context on stdio
impl RuntimeContext<Stdin, Stdout> {
    /// quickly create a context that uses stdin and stdout
    pub fn new_stdio() -> Self {
        let stdin = std::io::stdin();
        let stdout = std::io::stdout();
        Self::new(stdin, stdout)
    }
}

impl<R, W> RuntimeContext<R, W>
where
    R: Read,
    W: Write,
{
    pub fn new(input: R, output: W) -> Self {
        Self {
            vars: HashMap::new(),
            input,
            output,
        }
    }

    pub fn get(&self, name: &str) -> Option<&EvalValue> {
        self.vars.get(name)
    }

    pub fn create(&mut self, name: &str) {
        self.vars.insert(name.to_string(), EvalValue::Nil);
    }

    pub fn create_and_assign(&mut self, name: &str, value: EvalValue) {
        self.vars.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: EvalValue) -> Result<(), RuntimeError> {
        match self.vars.get_mut(name) {
            Some(v) => {
                *v = value;
                Ok(())
            }
            None => Err(RuntimeError::UndefinedVariable(name.to_string())),
        }
    }

    /// print values
    pub fn println(&mut self, value: EvalValue) -> Result<(), RuntimeError> {
        writeln!(self.output, "{}", value).map_err(|e| RuntimeError::IOError(Rc::new(e)))?;
        self.output
            .flush()
            .map_err(|e| RuntimeError::IOError(Rc::new(e)))
    }

    /// print messages
    pub fn print_msg(&mut self, msg: &str) -> Result<(), RuntimeError> {
        write!(self.output, "{}", msg).map_err(|e| RuntimeError::IOError(Rc::new(e)))?;

        self.output
            .flush()
            .map_err(|e| RuntimeError::IOError(Rc::new(e)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalValue {
    Num(f64),
    Bool(bool),
    Str(String),
    Unit,
    Nil,
    // TODO: support these types
    // List(Vec<EvalValue>), // can this be made like GADT?
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

pub fn eval<R, W>(
    program: &Program,
    ctx: &mut RuntimeContext<R, W>,
) -> Result<EvalValue, (RuntimeError, Range<usize>)>
where
    R: Read,
    W: Write,
{
    let mut val = EvalValue::Unit;
    for (stmt, range) in program.stmts.iter() {
        val = eval_stmt(stmt, ctx).map_err(|e| (e, range.clone()))?;
    }
    Ok(val)
}

fn eval_stmt<R, W>(stmt: &Stmt, ctx: &mut RuntimeContext<R, W>) -> Result<EvalValue, RuntimeError>
where
    R: Read,
    W: Write,
{
    match stmt {
        Stmt::Var(VarStmt { name, initializer }) => {
            let val = match initializer {
                Some((expr, _)) => eval_expr(expr, ctx)?,
                None => EvalValue::Nil,
            };
            ctx.create_and_assign(name, val);
        }
        Stmt::Expr((expr, _)) => {
            eval_expr(expr, ctx)?;
        }
        Stmt::Print((expr, _)) => {
            let val = eval_expr(expr, ctx)?;
            ctx.println(val)?;
        }
        Stmt::Empty => {
            // do nothing
        }
    }
    // currently, all statements will evaluate to Unit value
    // this will change after block statements are introduced
    Ok(EvalValue::Unit)
}

pub fn eval_expr<R, W>(
    expr: &Expr,
    ctx: &mut RuntimeContext<R, W>,
) -> Result<EvalValue, RuntimeError>
where
    R: Read,
    W: Write,
{
    match expr {
        Expr::Unary(ExprUnary { op, rhs }) => {
            let rhs_val = eval_expr(rhs, ctx)?;
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
            let rhs_val = eval_expr(rhs, ctx)?;

            // assignment should be handled separately
            // the lhs should be an identifier and assign the rhs value to it
            if let Expr::Literal(ExprLiteral::Ident(var)) = &**lhs {
                if *op == OpInfix::Assign {
                    ctx.assign(var, rhs_val)?;
                    return Ok(EvalValue::Unit);
                }
            }

            let lhs_val = eval_expr(lhs, ctx)?;
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
                ExprLiteral::Ident(i) => {
                    let val = ctx.get(i).cloned();
                    if val.is_none() {
                        let err = RuntimeError::UndefinedVariable(i.clone());
                        return Err(err);
                    }
                    val.unwrap()
                }
            };
            Ok(v)
        }
    }
}

#[cfg(test)]
mod eval_tests {
    use std::io::{BufWriter, Empty};

    use crate::syntax::{ExprParser, ProgramParser, SourceLexer};

    use super::*;
    type BufferOutputContext = RuntimeContext<Empty, BufWriter<Vec<u8>>>;

    fn evaled_expr_source(source: &str) -> Result<EvalValue, RuntimeError> {
        let mut ctx = RuntimeContext::new_stdio();

        let lexer = SourceLexer::new(source);
        let mut errors = Vec::new();
        let parsed = ExprParser::new().parse(&mut errors, lexer);
        let expr = parsed.unwrap();
        eval_expr(&expr, &mut ctx)
    }

    fn evaled_context(source: &str) -> Result<(EvalValue, BufferOutputContext), RuntimeError> {
        let input = std::io::empty();
        let output = BufWriter::new(vec![]);
        let mut ctx = RuntimeContext::new(input, output);

        let lexer = SourceLexer::new(source);
        let mut errors = Vec::new();
        let parsed = ProgramParser::new().parse(&mut errors, lexer);
        let program = parsed.unwrap();
        let v = eval(&program, &mut ctx).map_err(|(e, _)| e)?;
        Ok((v, ctx))
    }

    #[test]
    fn test_eval_named_calc() {
        let source = r#"
            var a = 1;
            var b = 2;   
            print a + b;
            "#;
        let evaled = evaled_context(source);
        assert!(evaled.is_ok());
        // check program output
        let (v, ctx) = evaled.unwrap();
        let dumped_output = ctx.output.into_inner().unwrap();
        assert_eq!(dumped_output, b"3\n");
        assert_eq!(v, EvalValue::Unit);
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
