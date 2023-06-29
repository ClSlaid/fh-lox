#![feature(assert_matches)]

pub mod error;
mod eval;
mod syntax;
mod types;
pub use eval::eval;
pub use syntax::Expr;
pub use syntax::ExprParser;
pub use syntax::SourceLexer;
