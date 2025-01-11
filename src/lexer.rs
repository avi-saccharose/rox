#![allow(dead_code)]
use std::{error::Error, fmt::Display};

use crate::token::{Kind, Literal, Token};

#[derive(Debug)]
pub(crate) struct LexerError {
    msg: String,
    line: usize,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = format!("[line: {}] {}", self.line, self.msg);
        f.write_str(&msg)
    }
}

impl Error for LexerError {}

struct Lexer<'a> {
    source: Vec<char>,
    current: usize,
    eof: &'a bool,
    col: u8,
    line: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source: source.chars().rev().collect(),
            current: 0,
            col: 0,
            line: 1,
            eof: &false,
        }
    }

    fn make_token(&self, kind: Kind, literal: Option<Literal>) -> Token {
        Token {
            kind,
            literal,
            line: self.line,
        }
    }

    fn peek(&self) -> char {
        self.source.last().copied().unwrap_or('\0')
    }

    fn eof(&self) -> bool {
        self.source.is_empty()
    }

    fn advance(&mut self) -> char {
        self.col += 1;
        self.source.pop().unwrap_or('\0')
    }

    fn digit(&mut self, initial: char) -> Result<Token, LexerError> {
        let mut int = String::from(initial);
        while self.peek().is_ascii_digit() {
            int.push(self.advance());
        }
        let res = int.parse::<i64>();
        match res {
            Ok(val) => Ok(self.make_token(Kind::Num, Some(Literal::Number(val)))),
            Err(_) => Err(LexerError {
                msg: "error parsing number".to_string(),
                line: self.line,
            }),
        }
    }

    fn identifier(&mut self, initial: char) -> Result<Token, LexerError> {
        let mut ident = String::from(initial);
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            ident.push(self.advance());
        }
        let (kind, literal) = match ident.as_str() {
            "if" => (Kind::If, None),
            "else" => (Kind::Else, None),
            "def" => (Kind::Def, None),
            "var" => (Kind::Var, None),
            "true" => (Kind::True, None),
            "false" => (Kind::False, None),
            "nil" => (Kind::Nil, None),
            "while" => (Kind::While, None),
            "for" => (Kind::For, None),
            "print" => (Kind::Print, None),
            _ => (Kind::Ident, Some(Literal::Ident(ident))),
        };
        Ok(self.make_token(kind, literal))
    }

    fn string(&mut self, closing: char) -> Result<Token, LexerError> {
        let mut string = String::new();
        while !self.eof() {
            let ch = self.advance();
            if ch == '"' {
                return Ok(self.make_token(Kind::String, Some(Literal::String(string))));
            }
            string.push(ch);
        }
        Err(LexerError {
            msg: "unterminated string".to_string(),
            line: self.line,
        })
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let ch = self.advance();
            if ch.is_whitespace() {
                if ch == '\n' {
                    self.line += 1;
                    self.col = 0;
                }
                continue;
            }
            if ch.is_ascii_digit() {
                tokens.push(self.digit(ch)?);
                continue;
            }
            if ch.is_alphabetic() || ch == '_' {
                tokens.push(self.identifier(ch)?);
                continue;
            }
            if ch == '"' {
                tokens.push(self.string(ch)?);
                continue;
            }

            let kind = match ch {
                '(' => Kind::Lparen,
                ')' => Kind::Rparen,
                '{' => Kind::LBrace,
                '}' => Kind::RBrace,
                ';' => Kind::Semicolon,
                ',' => Kind::Comma,
                '+' => Kind::Plus,
                '-' => Kind::Minus,
                '*' => Kind::Star,
                '/' => Kind::Slash,
                '<' => {
                    if self.peek() == '=' {
                        self.advance();
                        Kind::LtEq
                    } else {
                        Kind::Lt
                    }
                }
                '>' => {
                    if self.peek() == '=' {
                        self.advance();
                        Kind::GtEq
                    } else {
                        Kind::Gt
                    }
                }
                '=' => {
                    if self.peek() == '=' {
                        self.advance();
                        Kind::EqEq
                    } else {
                        Kind::Eq
                    }
                }
                '!' => {
                    if self.peek() == '=' {
                        self.advance();
                        Kind::NtEq
                    } else {
                        Kind::Bang
                    }
                }
                '\0' => break,

                _ => {
                    let msg = format!("Unknown character {}", ch);
                    return Err(LexerError {
                        msg,
                        line: self.line,
                    });
                }
            };

            tokens.push(self.make_token(kind, None));
            continue;
        }
        Ok(tokens)
    }
}

pub(crate) fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map_kind(tokens: Vec<Token>) -> Vec<Kind> {
        tokens.into_iter().map(|token| token.kind).collect()
    }
    #[test]
    fn lex_single_token() {
        let res = lex("123 def if (true)").unwrap();
        let res: Vec<Kind> = map_kind(res);
        assert_eq!(
            res,
            vec![
                Kind::Num,
                Kind::Def,
                Kind::If,
                Kind::Lparen,
                Kind::True,
                Kind::Rparen
            ]
        );
    }

    #[test]
    fn double_characters() {
        let res = lex("<= == !=").unwrap();
        assert_eq!(map_kind(res), vec![Kind::LtEq, Kind::EqEq, Kind::NtEq])
    }
    #[test]
    fn lex_string() {
        let res = lex("\"Hello world\"").unwrap();
        assert_eq!(
            Literal::String("Hello world".to_string()),
            res[0].to_owned().literal.unwrap()
        )
    }

    #[test]
    fn unterminated_string() {
        let res = lex("\"unterminated_string");
        assert!(res.is_err());
    }
}
