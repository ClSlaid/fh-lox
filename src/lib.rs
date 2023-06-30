#![feature(assert_matches)]

pub mod error;
mod eval;
mod syntax;
mod types;
pub use eval::eval;
pub use eval::eval_expr;
pub use eval::RuntimeContext;
pub use syntax::Expr;
pub use syntax::ExprParser;
pub use syntax::ProgramParser;
pub use syntax::SourceLexer;
