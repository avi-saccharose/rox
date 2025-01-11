#![allow(dead_code)]

use crate::{
    expr::{Assign, Bin, Expr, Literal, Stmt, Unary, Var},
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

    fn consume(&mut self, kind: Kind, msg: &str) -> ParseResult<Token> {
        if !self.check(&kind) {
            return Err(msg.to_string());
        }
        Ok(self.advance().unwrap())
    }

    pub fn parse(&mut self) -> ParseResult<()> {
        while self.peek().is_some() {
            let stmt = self.stmt_declaration()?;
            self.ast.push(stmt);
        }
        Ok(())
    }

    pub fn stmt_declaration(&mut self) -> ParseResult<Stmt> {
        match self.peek().unwrap().kind {
            Kind::Var => self.stmt_var(),
            _ => self.stmt(),
        }
    }

    pub fn stmt_var(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let name = self.consume(Kind::Ident, "Expect variable name")?;
        let mut initializer: Option<Expr> = None;

        if self.check(&Kind::Eq) {
            self.advance();
            initializer = self.expr().ok();
        }
        self.consume(Kind::Semicolon, "Expected ';' after var declaration")?;
        Ok(Stmt::VarDecl(Var { name, initializer }))
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        match self.peek().unwrap().kind {
            Kind::Print => self.stmt_print(),
            Kind::LBrace => self.stmt_block(),
            _ => self.stmt_expr(),
        }
    }

    fn stmt_print(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "EXpect ';' after expression")?;
        Ok(Stmt::Print(expr))
    }

    fn stmt_expr(&mut self) -> ParseResult<Stmt> {
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "Expect ';' after expression")?;
        Ok(Stmt::Expr(expr))
    }

    fn stmt_block(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let mut stmts = Vec::new();
        while !self.eof() && !self.check(&Kind::RBrace) {
            stmts.push(self.stmt_declaration()?);
        }
        self.consume(Kind::RBrace, "Expect '}' after block")?;
        Ok(Stmt::Block(stmts))
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.equality()?;
        if self.check(&Kind::Eq) {
            let op = self.advance().unwrap();
            let value = self.assignment()?;
            match expr {
                Expr::Var(name) => {
                    return Ok(Expr::Assign(Assign {
                        name,
                        value: Box::new(value),
                    }));
                }
                _ => return Err("Invalid assignment".to_string()),
            }
        }
        Ok(expr)
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
        let literal = match token.kind {
            Kind::True => Literal::Bool(true),
            Kind::False => Literal::Bool(false),
            Kind::Nil => Literal::Nil,
            _ => match token.literal.unwrap() {
                token::Literal::String(str) => Literal::String(str),
                token::Literal::Number(int) => Literal::Number(int),
                token::Literal::Ident(iden) => {
                    return Expr::Var(iden);
                }
                _ => unreachable!(),
            },
        };
        Expr::Literal(literal)
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

    #[test]
    fn parse_var_decl() {
        let parsed = parse(tokenize("var x = 1; var y = true;").unwrap());
        assert!(parsed.is_ok());
        assert_eq!(
            parsed.unwrap()[0],
            Stmt::VarDecl(Var {
                name: Token {
                    kind: Kind::Ident,
                    line: 1,
                    literal: Some(token::Literal::Ident("x".to_string()))
                },
                initializer: Some(Expr::Literal(Literal::Number(1)))
            })
        );
    }

    #[test]
    fn parse_assign() {
        let parsed = parse(tokenize("x = 1;").unwrap());
        assert!(parsed.is_ok());
    }

    #[test]
    fn parse_invalid_assignment() {
        let parsed = parse(tokenize("1 = 1;").unwrap());
        assert!(parsed.is_err());
    }

    #[test]
    fn parse_block() {
        let parsed = parse(tokenize("{ var x = 1; }").unwrap());
        assert!(parsed.is_ok());
        assert!(matches!(parsed.unwrap()[0], Stmt::Block(..)));
    }
}
