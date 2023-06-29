#![allow(dead_code)]

use std::{
    iter::Iterator,
    ops::{Range, RangeBounds},
};

use logos::{Lexer, Logos, SpannedIter};

use crate::error::SyntaxError;

pub type Spanned<T, E> = Result<(usize, T, usize), E>;

pub struct SourceLexer<'a> {
    token_stream: SpannedIter<'a, Token>,
}

impl<'a> SourceLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            token_stream: Token::lexer(source).spanned(),
        }
    }
}

impl Iterator for SourceLexer<'_> {
    type Item = Spanned<Token, SyntaxError>;
    fn next(&mut self) -> Option<Self::Item> {
        let (nxt, span) = self.token_stream.next()?;

        if let Ok(inner) = &nxt {
            if !matches!(inner, Token::Error) {
                return Some(Ok((span.start, nxt.unwrap(), span.end)));
            }
        }

        let error_type = match nxt {
            Ok(_) => LexerError::Unexpected,
            Err(e) => e,
        };

        let source = self.token_stream.slice().to_string();
        let error = match error_type {
            LexerError::InvalidNum => SyntaxError::InvalidNumber(source),
            LexerError::UnclosedString => SyntaxError::UnterminatedString(source),
            LexerError::Unexpected => SyntaxError::UnrecognizedToken(source),
        };
        Some(Err(error))
    }
}

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(error = LexerError)]
pub enum Token {
    // single-character tokens
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,

    // one or two character tokens
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    // Literal
    #[regex(r"[\w--\d][\w\d_]*", lexer_ident)]
    Ident(String),
    #[regex(r#""([^"]|(\\"))*""#, lexer_string)]
    Str(String),
    // unsigned float, use string to prevent overflow
    #[regex(r"\d+(\.\d+)?", lexer_number)]
    Num(f64),

    // Keywords
    #[token("and")]
    And,
    #[token("class")]
    Class,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("fun")]
    Fun,
    #[token("for")]
    For,
    #[token("nil")]
    Nil,
    #[token("or")]
    Or,
    #[token("print")]
    Print,
    #[token("return")]
    Return,
    #[token("super")]
    Super,
    #[token("this")]
    This,
    #[token("var")]
    Var,
    #[token("while")]
    While,

    #[regex(r"//.*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    Error,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    InvalidNum,
    UnclosedString,
    #[default]
    Unexpected,
}

fn lexer_ident(lex: &mut Lexer<Token>) -> String {
    lex.slice().to_string()
}

fn lexer_string(lex: &mut Lexer<Token>) -> String {
    lex.slice().trim_matches('"').to_string()
}

fn lexer_number(lex: &mut Lexer<Token>) -> Result<f64, LexerError> {
    let s = lex.slice();
    let _span = lex.span();
    s.parse::<f64>().map_err(|_| LexerError::InvalidNum)
}

#[cfg(test)]
mod lexer_test {
    use std::assert_matches::assert_matches;

    use super::*;
    use pretty_assertions::assert_eq;

    fn assert_token_list(
        got: Vec<Result<(usize, Token, usize), SyntaxError>>,
        expected_tt: Vec<Token>,
    ) {
        assert_eq!(got.len(), expected_tt.len());
        for (got, expected) in got.iter().zip(expected_tt.iter()) {
            assert!(got.is_ok());
            let (_, got, _) = &got.clone().unwrap();
            if let Token::Num(g) = got {
                let Token::Num(expected) = expected else {unreachable!()};
                assert!(g - expected <= 1e-6);
                continue;
            }

            assert_eq!(*got, *expected);
        }
    }

    #[test]
    fn test_invalid_token() {
        let source = "&";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 1);
        assert!(got[0].is_err());
        let got = got[0].clone().unwrap_err();
        assert!(matches!(got, SyntaxError::UnrecognizedToken(_)));
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"unterminated string";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 1);
        assert!(got[0].is_err());
    }

    #[test]
    fn test_multiple_line_string() {
        let source = "\"multiple\nline\nstring\"";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 1);
        assert!(got[0].is_ok());
        let got = &got[0];
        assert_eq!(
            got.clone().unwrap().1,
            Token::Str("multiple\nline\nstring".to_string())
        );
    }

    #[test]
    fn test_comment() {
        // single line comment
        let source = "// comment\n123";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 1);
        let token = got[0].clone();
        assert!(token.is_ok());
        assert_matches!(token.unwrap().1, Token::Num(_));

        // block comment
        let source = "/* comment */123";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 1);
        let token = got[0].clone();
        assert!(token.is_ok());
        assert_matches!(token.unwrap().1, Token::Num(_));

        // block comment, nested, not supported
        let source = "/* comment /* nested */ */123";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 3);
        let expected = vec![Token::Star, Token::Slash, Token::Num(123.0)];
        assert_token_list(got, expected);

        // single line comment shadows block comment
        let source = "// comment /* comment\n */123";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        assert_eq!(got.len(), 3);
        let expected = vec![Token::Star, Token::Slash, Token::Num(123.0)];
        assert_token_list(got, expected);
    }

    #[test]
    fn test_tokens() {
        let source = "{var a = 1 + 2;}";
        let lexer = SourceLexer::new(source);
        let got = lexer.collect::<Vec<_>>();
        let expected_tt = vec![
            Token::LeftBrace,
            Token::Var,
            Token::Ident("a".to_string()),
            Token::Equal,
            Token::Num(1.0),
            Token::Plus,
            Token::Num(2.0),
            Token::Semicolon,
            Token::RightBrace,
        ];
        assert_token_list(got, expected_tt);
    }
}
