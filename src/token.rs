#![allow(dead_code)]

use std::fmt::Display;

use thiserror::Error;

pub const KEYWORDS: phf::Map<&str, TokenType> = phf::phf_map! {
    "and" => TokenType::And,
    "or" => TokenType::Or,
    "class" => TokenType::Class,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "fun" => TokenType::Fun,
    "var" => TokenType::Var,
    "for" => TokenType::For,
    "nil" => TokenType::Nil,
    "print" => TokenType::Print,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "this" => TokenType::This,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literal
    Ident(String),
    Str(String),
    // unsigned integer, use string to prevent overflow
    Nat(String),
    // unsigned float, use string to prevent overflow
    NatDot(String, String),

    // Keywords
    And,
    Class,
    If,
    Else,
    True,
    False,
    Fun,
    For,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,

    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),

            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),

            TokenType::Ident(s) => write!(f, "{}", s),
            TokenType::Str(s) => {
                let _trans = s
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t")
                    .replace('\"', "\\\"");
                write!(f, "\"{}\"", s)
            }
            TokenType::Nat(n) => {
                write!(f, "{}", n)
            }
            TokenType::NatDot(i, frac) => {
                write!(f, "{}.{}", i, frac)
            }
            TokenType::And => write!(f, "and"),
            TokenType::Class => write!(f, "class"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Fun => write!(f, "fun"),
            TokenType::For => write!(f, "for"),
            TokenType::Nil => write!(f, "nil"),
            TokenType::Or => write!(f, "or"),
            TokenType::Print => write!(f, "print"),
            TokenType::Return => write!(f, "return"),
            TokenType::Super => write!(f, "super"),
            TokenType::This => write!(f, "this"),
            TokenType::Var => write!(f, "var"),
            TokenType::While => write!(f, "while"),
            TokenType::Eof => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: String,
    line: usize,
}

impl Token {
    pub fn new(tt: TokenType, lexeme: String, literal: String, line: usize) -> Self {
        Self {
            token_type: tt,
            lexeme,
            literal,
            line,
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn lexeme(&self) -> &str {
        self.lexeme.as_ref()
    }

    pub fn literal(&self) -> &str {
        self.literal.as_ref()
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self.token_type, self.lexeme, self.literal)
    }
}

#[derive(Debug, Clone, Error)]
pub struct InvalidToken {
    pub position: usize,
    pub got: char,
    // error prompt
    pub prompt: String,
}

impl InvalidToken {
    pub fn new(position: usize, got: char, prompt: String) -> Self {
        Self {
            position,
            got,
            prompt,
        }
    }
}

impl Display for InvalidToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] Invalid token: {} at position {}",
            self.position, self.got, self.position
        )
    }
}
