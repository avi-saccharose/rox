#![allow(dead_code)]

use crate::{
    expr::{Bin, Expr, Literal, Stmt, Unary},
    token::{self, Kind, Token},
};

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
    ast: Vec<Stmt>,
    previous: Option<Token>,
}

type ParseResult<T> = Result<T, String>;
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().rev().collect(),
            ast: Vec::new(),
            previous: None,
        }
    }

    fn eof(&self) -> bool {
        self.tokens.is_empty()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

    fn check(&self, kind: &Kind) -> bool {
        if let Some(token) = self.peek() {
            return token.kind == *kind;
        }
        false
    }

    fn matches(&self, kinds: &[Kind]) -> bool {
        for kind in kinds.iter() {
            if self.check(kind) {
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> Option<Token> {
        if self.eof() {
            return None;
        }
        self.tokens.pop()
    }

    fn consume(&mut self, kind: Kind, msg: &str) -> ParseResult<()> {
        if !self.check(&kind) {
            return Err(msg.to_string());
        }
        self.advance();
        Ok(())
    }

    fn stmt_print(&mut self) -> ParseResult<()> {
        self.advance();
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "EXpect ';' after expression")?;
        self.ast.push(Stmt::Print(expr));
        Ok(())
    }

    fn stmt_expr(&mut self) -> ParseResult<()> {
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "Expect ';' after expression")?;
        self.ast.push(Stmt::Expr(expr));
        Ok(())
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;

        while self.matches(&[Kind::EqEq, Kind::NtEq]) {
            let op = self.advance().unwrap().kind;
            let right = self.comparison()?;
            expr = Expr::Bin(Bin {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            });
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;

        while self.matches(&[Kind::Lt, Kind::LtEq, Kind::Gt, Kind::GtEq]) {
            let op = self.advance().unwrap().kind;
            let right = self.term()?;
            expr = Expr::Bin(Bin {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            });
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;

        while self.matches(&[Kind::Plus, Kind::Minus]) {
            let op = self.advance().unwrap().kind;
            let right = self.factor()?;
            expr = Expr::Bin(Bin {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            });
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;

        while self.matches(&[Kind::Star, Kind::Slash]) {
            let op = self.advance().unwrap().kind;
            let right = self.unary()?;
            expr = Expr::Bin(Bin {
                left: Box::new(expr),
                right: Box::new(right),
                op,
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.matches(&[Kind::Bang, Kind::Minus]) {
            let op = self.advance().unwrap().kind;
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                op,
                right: Box::new(right),
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.eof()
            || !self.matches(&[
                Kind::Num,
                Kind::String,
                Kind::Lparen,
                Kind::Ident,
                Kind::True,
                Kind::False,
                Kind::Nil,
                Kind::Plus,
            ])
        {
            //   dbg!(&self);
            return Err("Expected expression".to_string());
        }

        if self.matches(&[Kind::Lparen]) {
            self.advance();
            let expr = self.expr()?;
            self.consume(Kind::Rparen, "Expected ')'")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }
        //   dbg!(&self);

        let token = self.advance().unwrap();
        Ok(self.make_literal(token))
    }

    fn make_literal(&self, token: Token) -> Expr {
        let literal = token.literal.unwrap();
        let literal = match (token.kind, literal) {
            (Kind::Num, token::Literal::Number(int)) => Literal::Number(int),
            (Kind::String, token::Literal::String(str)) => Literal::String(str),
            (Kind::Ident, token::Literal::Ident(iden)) => Literal::Ident(iden),
            (Kind::True, _) => Literal::Bool(true),
            (Kind::False, _) => Literal::Bool(false),
            (Kind::Nil, _) => Literal::Nil,
            _ => unreachable!(),
        };
        Expr::Literal(literal)
    }

    pub fn parse(&mut self) -> ParseResult<()> {
        while let Some(token) = self.peek() {
            match token.kind {
                Kind::Print => self.stmt_print()?,
                _ => self.stmt_expr()?,
            }
        }
        Ok(())
    }
}
pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
    //print!("{:?}", &tokens);
    let mut parser = Parser::new(tokens);
    parser.parse()?;
    //  dbg!(&parser.ast);
    Ok(parser.ast)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::*;
    use crate::lexer::{lex, LexerError};

    fn tokenize(source: &str) -> Result<Vec<Token>, LexerError> {
        lex(source)
    }

    #[test]
    fn parse_expr() {
        let parsed = parse(tokenize("1 + 2 + 3 ;").unwrap());
        assert!(parsed.is_ok());
        assert_eq!(
            parsed.unwrap()[0],
            Stmt::Expr(Expr::Bin(Bin {
                left: Box::new(Expr::Bin(Bin {
                    left: Box::new(Expr::Literal(Literal::Number(1))),
                    right: Box::new(Expr::Literal(Literal::Number(2))),
                    op: Kind::Plus
                })),
                right: Box::new(Expr::Literal(Literal::Number(3))),
                op: Kind::Plus
            }))
        );
    }

    #[test]
    fn parse_print() {
        let parsed = parse(tokenize("print 1;").unwrap());
        assert!(parsed.is_ok());
        assert_eq!(
            parsed.unwrap()[0],
            Stmt::Print(Expr::Literal(Literal::Number(1)))
        );
    }
}
