use super::token;
use crate::token::InvalidToken;

type Result<T> = ::core::result::Result<T, InvalidToken>;

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &'static str) -> Self {
        let source = source.chars().collect();

        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<token::Token>> {
        let empty_cnt = self.source.iter().filter(|c| c.is_whitespace()).count();
        // tokens should be close to the number of whitespaces
        // to minimize realocations, set it to whitespaces + 2
        let mut tokens = Vec::with_capacity(empty_cnt + 2);
        while !self.is_at_end() {
            self.start = self.current;
            let token = self.scan_token()?;
            tokens.push(token);
        }
        // may cause double EoF token
        // but could be tolerated
        tokens.push(token::Token::new(
            token::TokenType::Eof,
            "".to_string(),
            "".to_string(),
            self.line,
        ));
        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    // advance and scan out a token
    fn scan_token(&mut self) -> Result<token::Token> {
        // a state machine to scan out a token
        let mut tt_acc = None;
        let mut lexeme_acc = String::new();
        let mut literal_acc = String::new();

        let mut tt_final: token::TokenType = loop {
            let c = self.source.get(self.current);

            // meet EoF
            if c.is_none() {
                match tt_acc {
                    Some(token::TokenType::Str(_)) => {
                        let err = InvalidToken::new(
                            self.current,
                            '\0',
                            "Unterminated string.".to_string(),
                        );
                        return Err(err);
                    }
                    Some(token::TokenType::NatDot(_, frac)) if frac.is_empty() => {
                        let err = InvalidToken::new(
                            self.current,
                            '.',
                            "Expect a fractional part after '.'.".to_string(),
                        );
                        return Err(err);
                    }
                    Some(tt) => break tt,
                    None => break token::TokenType::Eof,
                }
            }

            let c = c.unwrap();

            // quickly go through long lexemes
            // meet string
            match (tt_acc, *c) {
                (None, '"') => {
                    tt_acc = Some(token::TokenType::Str(String::new()));
                    self.current += 1;
                    continue;
                }
                (Some(token::TokenType::Str(s)), '"') => {
                    // we have met a valid token
                    break token::TokenType::Str(s);
                }

                (Some(token::TokenType::Str(mut s)), c) => {
                    if c == '\n' {
                        self.line += 1;
                    }
                    s.push(c);
                    tt_acc = Some(token::TokenType::Str(s));

                    literal_acc.push(c);
                    lexeme_acc.push(c);
                    self.current += 1;
                    continue;
                }
                (tt, _) => tt_acc = tt,
            }
            // meet ident
            match (tt_acc, *c) {
                (None, ch) if ch.is_alphabetic() => {
                    tt_acc = Some(token::TokenType::Ident(ch.to_string()));
                    literal_acc.push(ch);
                    lexeme_acc.push(ch);
                    self.current += 1;
                    continue;
                }
                (Some(token::TokenType::Ident(mut id)), ch) if ch.is_alphanumeric() => {
                    id.push(ch);
                    tt_acc = Some(token::TokenType::Ident(id));

                    literal_acc.push(ch);
                    lexeme_acc.push(ch);
                    self.current += 1;
                    continue;
                }
                (tt, _) => tt_acc = tt,
            }
            // meet number
            match (tt_acc, *c) {
                (None, ch) if ch.is_ascii_digit() => {
                    tt_acc = Some(token::TokenType::Nat(ch.to_string()));

                    literal_acc.push(ch);
                    lexeme_acc.push(ch);
                    self.current += 1;
                    continue;
                }
                (Some(token::TokenType::Nat(mut n)), ch) if ch.is_ascii_digit() => {
                    n.push(ch);
                    tt_acc = Some(token::TokenType::Nat(n));

                    literal_acc.push(ch);
                    lexeme_acc.push(ch);
                    self.current += 1;
                    continue;
                }
                (Some(token::TokenType::Nat(n)), '.') => {
                    tt_acc = Some(token::TokenType::NatDot(n, String::new()));

                    literal_acc.push('.');
                    lexeme_acc.push('.');
                    self.current += 1;
                    continue;
                }
                (Some(token::TokenType::NatDot(i, mut frac)), ch) if ch.is_ascii_digit() => {
                    frac.push(ch);
                    tt_acc = Some(token::TokenType::NatDot(i, frac));

                    literal_acc.push(ch);
                    lexeme_acc.push(ch);
                    self.current += 1;
                    continue;
                }
                (tt, _) => tt_acc = tt,
            }

            // check if we meet comment, if meet skip to next line
            // double slash comment
            if tt_acc == Some(token::TokenType::Slash) && *c == '/' {
                // clear states
                tt_acc = None;
                literal_acc = String::new();
                lexeme_acc = String::new();
                // skip to next line
                while let Some(p) = self.peek_one() {
                    self.current += 1;
                    if p == '\n' {
                        self.line += 1;
                        break; // break from skip
                    }
                }
                // correctness: we have cleared the status
                // if we meet EoF, we will return EoF token
                continue;
            }

            // meet whitespaces
            if c.is_ascii_whitespace() {
                if let Some(tt) = tt_acc {
                    // we have met a valid token
                    // jump out of the loop
                    // correctness: we have handled EoF, comment and Str
                    break tt;
                }

                if *c == '\n' {
                    self.line += 1;
                }
                // safety: we have checked if c is whitespace
                // we definitely will not meet EoF
                self.current += 1;
                continue;
            }

            let tt_nxt = match c {
                '(' => token::TokenType::LeftParen,
                ')' => token::TokenType::RightParen,
                '{' => token::TokenType::LeftBrace,
                '}' => token::TokenType::RightBrace,
                ',' => token::TokenType::Comma,
                '.' => token::TokenType::Dot,
                '-' => token::TokenType::Minus,
                '+' => token::TokenType::Plus,
                ';' => token::TokenType::Semicolon,
                '*' => token::TokenType::Star,

                // operators
                '!' => token::TokenType::Bang,
                '=' => token::TokenType::Equal,
                '<' => token::TokenType::Less,
                '>' => token::TokenType::Greater,
                '/' => token::TokenType::Slash,

                // long lexemes, but actually won't be used, because they must be the next token
                // we can use Eof token here to save computations
                '"' => token::TokenType::Eof, // Str
                _ if c.is_alphanumeric() => token::TokenType::Eof, // identifiers and numbers

                // invalid tokens
                _ => {
                    // invalid token
                    return Err(InvalidToken::new(
                        self.current,
                        *c,
                        format!("Invalid token: {} at position {}", c, self.current),
                    ));
                }
            };
            literal_acc.push(*c);

            // won't do: slash star comment

            // move scanner status
            tt_acc = match (tt_acc, tt_nxt) {
                (None, t) => Some(t),

                // combinative tokens
                // check '!='
                (Some(token::TokenType::Bang), token::TokenType::Equal) => {
                    Some(token::TokenType::BangEqual)
                }
                // check '=='
                (Some(token::TokenType::Equal), token::TokenType::Equal) => {
                    Some(token::TokenType::EqualEqual)
                }
                // check '>='
                (Some(token::TokenType::Greater), token::TokenType::Equal) => {
                    Some(token::TokenType::GreaterEqual)
                }
                // check '<='
                (Some(token::TokenType::Less), token::TokenType::Equal) => {
                    Some(token::TokenType::LessEqual)
                }

                // not combinative tokens, but not separated by whitespaces is allowed
                // break with the previous token
                (Some(tt), _) => break tt,

                // untoleratable token combinations?
                #[allow(unreachable_patterns)]
                _ => {
                    let err =
                        InvalidToken::new(self.current, *c, "should be a valid token".to_string());
                    return Err(err);
                }
            };

            // step one after state transition
            self.current += 1;
        };

        // keyword check
        if let token::TokenType::Ident(s) = tt_final {
            if let Some(tt) = token::KEYWORDS.get(s.as_str()) {
                tt_final = tt.clone();
            } else {
                // not a keyword, move string back inside tt_final
                tt_final = token::TokenType::Ident(s);
            }
        }

        let t = token::Token::new(tt_final, lexeme_acc, literal_acc, self.line);
        Ok(t)
    }

    /// check the next character
    fn peek_one(&self) -> Option<char> {
        self.source.get(self.current + 1).copied()
    }
}

#[cfg(test)]
mod scanner_tests {
    use crate::token::TokenType;
    #[test]
    fn test_invalid_token() {
        let source = "&";
        let mut scanner = super::Scanner::new(source);
        let got = scanner.scan_token();
        assert!(got.is_err());
    }
    #[test]
    fn test_unterminated_string() {
        let source = "\"unterminated string";
        let mut scanner = super::Scanner::new(source);
        let got = scanner.scan_token();
        assert!(got.is_err());
    }
    #[test]
    fn test_unterminated_number() {
        // dot without frac
        let source = "123.";
        let mut scanner = super::Scanner::new(source);
        let got = scanner.scan_token();
        assert!(got.is_err());
    }
    #[test]
    fn test_multiple_line_string() {
        let source = "\"multiple\nline\nstring\"";
        let mut scanner = super::Scanner::new(source);
        let got = scanner.scan_token();
        assert!(got.is_ok());
        let tt = got.unwrap();
        assert_eq!(tt.literal(), source[1..source.len() - 1].to_string());
    }
    #[test]
    fn test_comment() {
        let source = "// comment\n123";
        let mut scanner = super::Scanner::new(source);
        let got = scanner.scan_tokens();
        assert!(got.is_ok());
        let tokens = got.unwrap();
        assert_eq!(tokens.len(), 2);
        let token = &tokens[0];
        assert_eq!(*token.token_type(), TokenType::Nat("123".to_string()));
    }
    #[test]
    fn test_tokens() {
        let source = "{var a = 1 + 2;}";
        let mut scanner = super::Scanner::new(source);
        let tokens = scanner.scan_tokens();
        assert!(tokens.is_ok());
        let tokens = tokens.unwrap();
        let expected_tt = vec![
            TokenType::LeftBrace,
            TokenType::Var,
            TokenType::Ident("a".to_string()),
            TokenType::Equal,
            TokenType::Nat(1.to_string()),
            TokenType::Plus,
            TokenType::Nat(2.to_string()),
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_eq!(tokens.len(), expected_tt.len());
        for (got, expected) in tokens.iter().zip(expected_tt.iter()) {
            assert_eq!(*got.token_type(), *expected);
        }
    }
}
