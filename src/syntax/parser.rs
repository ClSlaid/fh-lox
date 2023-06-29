use lalrpop_util::lalrpop_mod;

pub use self::grammar::ExprParser;

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

    fn parse_source_res(source: &str) -> Result<Expr, ParseError<usize, Token, SyntaxError>> {
        let mut v = vec![];
        let lexer = SourceLexer::new(source);
        ExprParser::new().parse(&mut v, lexer)
    }

    fn parse_source(source: &str) -> Expr {
        let expr = parse_source_res(source);
        assert!(expr.is_ok());

        expr.unwrap()
    }

    mod expr_test {
        use super::*;
        use pretty_assertions::assert_eq;
        #[test]
        fn parse_error() {
            let expr = parse_source_res("1 + 2.");
            assert!(expr.is_err());
            let err = expr.unwrap_err();
            println!("{:?}", err);
        }
        #[test]
        fn priority_test() {
            // test priority of add and mul
            let expr = parse_source("1 + 2 * 3");
            let Expr::Binary(bin_expr) = expr else {unreachable!("1 + 2 * 3 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Add);
            assert!(matches!(*bin_expr.lhs, Expr::Literal(_)));
            let rhs = bin_expr.rhs;
            let Expr::Binary(rhs_expr) = *rhs else {unreachable!("2 * 3 should be parsed as binary!")};
            assert_eq!(rhs_expr.op, OpInfix::Mul);

            // test priority of grouping
            let expr = parse_source("(1 + 2) * 3");
            let Expr::Binary(bin_expr) = expr else {unreachable!("(1 + 2) * 3 should be parsed as binary!")};
            assert_eq!(bin_expr.op, OpInfix::Mul);
            assert!(matches!(*bin_expr.rhs, Expr::Literal(_)));
            let lhs = bin_expr.lhs;
            let Expr::Binary(lhs_expr) = *lhs else {unreachable!("(1 + 2) should be parsed as binary!")};
            assert_eq!(lhs_expr.op, OpInfix::Add);
        }

        #[test]
        fn unary_test() {
            let expr = parse_source("1 + -1");
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
