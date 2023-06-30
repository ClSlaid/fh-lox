use lalrpop_util::lalrpop_mod;

pub use self::grammar::ExprParser;
pub use self::grammar::ProgramParser;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(dead_code)]
    grammar,
    "/syntax/grammar.rs"
);

#[cfg(test)]
mod parser_test {
    use lalrpop_util::ParseError;

    use super::*;
    use crate::{
        error::SyntaxError,
        syntax::{
            ast::*,
            token::{SourceLexer, Token},
        },
    };

    fn parse_expr_res(source: &str) -> Result<Expr, ParseError<usize, Token, SyntaxError>> {
        let mut v = vec![];
        let lexer = SourceLexer::new(source);
        ExprParser::new().parse(&mut v, lexer)
    }

    fn parse_program_res(source: &str) -> Result<Program, ParseError<usize, Token, SyntaxError>> {
        let mut v = vec![];
        let lexer = SourceLexer::new(source);
        ProgramParser::new().parse(&mut v, lexer)
    }

    fn parse_program(source: &str) -> Program {
        let program = parse_program_res(source);
        assert!(program.is_ok());
        program.unwrap()
    }

    fn parse_expr(source: &str) -> Expr {
        let expr = parse_expr_res(source);
        assert!(expr.is_ok());

        expr.unwrap()
    }

    mod program_test {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn parse_error() {
            let source = "var 1 + 2";
            let res = parse_program_res(source);
            assert!(res.is_err());
            let _err = res.unwrap_err();
        }

        #[test]
        fn parse_var_stmt() {
            let source = r#"
                var a = 1;
                var b;
                ;
            "#;
            let res = parse_program(source);
            let var_a = res.stmts[0].clone().0;
            let var_b = res.stmts[1].clone().0;
            let var_end = res.stmts[2].clone().0;

            let Stmt::Var(var_a) = var_a else {panic!("var a should be parsed as Stmt::Var!")};
            let Stmt::Var(var_b) = var_b else {panic!("var b should be parsed as Stmt::Var!")};
            let Stmt::Empty = var_end else {panic!("var_end should be parsed as Stmt::Empty!")};
            assert_eq!(var_a.name, "a");
            assert!(var_a.initializer.is_some());
            let init = var_a.initializer.unwrap().0;
            assert!(matches!(init, Expr::Literal(_)));

            assert_eq!(var_b.name, "b");
            assert!(var_b.initializer.is_none());
        }
    }

    mod expr_test {
        use super::*;
        use pretty_assertions::assert_eq;
        #[test]
        fn parse_error() {
            let expr = parse_expr_res("1 + 2.");
            assert!(expr.is_err());
            let _err = expr.unwrap_err();
        }
        #[test]
        fn priority_test() {
            // test priority of add and mul
            let expr = parse_expr("1 + 2 * 3");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 + 2 * 3 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Add);
            assert!(matches!(*bin_expr.lhs, Expr::Literal(_)));
            let rhs = bin_expr.rhs;
            let Expr::Binary(rhs_expr) = *rhs else {unreachable!("2 * 3 should be parsed as binary!")};
            assert_eq!(rhs_expr.op, OpInfix::Mul);

            // test priority of grouping
            let expr = parse_expr("(1 + 2) * 3");
            let Expr::Binary(bin_expr) = expr else {unreachable!("(1 + 2) * 3 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Mul);
            assert!(matches!(*bin_expr.rhs, Expr::Literal(_)));
            let lhs = bin_expr.lhs;
            let Expr::Binary(lhs_expr) = *lhs else {unreachable!("(1 + 2) should be parsed as binary!")};
            assert_eq!(lhs_expr.op, OpInfix::Add);

            // test priority of comparing
            let expr = parse_expr("1 + 2 > 3");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 + 2 > 3 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Gt);
            assert!(matches!(*bin_expr.lhs, Expr::Binary(_)));
            assert!(matches!(*bin_expr.rhs, Expr::Literal(_)));

            // test priority of equality
            let expr = parse_expr("1 > 2 == 3 > 4");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 > 2 == 3 > 4 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Eq);
            assert!(matches!(*bin_expr.lhs, Expr::Binary(_)));
            assert!(matches!(*bin_expr.rhs, Expr::Binary(_)));

            // test priority of logical
            let expr = parse_expr("1 > 2 == 3 > 4 and 3 > 4 == 5 > 6");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 > 2 == 3 > 4 and 3 > 4 == 5 > 6 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::And);
            let Expr::Binary(lhs) = *bin_expr.lhs else {unreachable!("1 > 2 == 3 > 4 should be parsed as binary!")};
            assert_eq!(lhs.op, OpInfix::Eq);

            // test priority between logical `and` and `or`
            let expr = parse_expr("1 > 2 == 3 > 4 or 3 > 4 == 5 > 6 and 7 > 8 == 9 > 10");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 > 2 == 3 > 4 or 3 > 4 == 5 > 6 and 7 > 8 == 9 > 10 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Or);
            let Expr::Binary(rhs) = *bin_expr.rhs else {unreachable!("3 > 4 == 5 > 6 and 7 > 8 == 9 > 10 should be parsed as binary!")};
            assert_eq!(rhs.op, OpInfix::And);
        }

        #[test]
        fn unary_test() {
            let expr = parse_expr("1 + -1");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 + - 1 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Add);
            assert!(matches!(*bin_expr.lhs, Expr::Literal(_)));
            let rhs = bin_expr.rhs;
            assert!(matches!(
                *rhs,
                Expr::Unary(ExprUnary {
                    op: OpPrefix::Neg,
                    ..
                })
            ));
        }
    }
}
