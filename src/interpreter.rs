use core::fmt;
use std::{collections::HashMap, fmt::write};

use crate::{
    error::RoxError,
    expr::{Bin, Expr, Literal, Stmt, Var},
    token::Kind,
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Value {
    Number(i64),
    String(String),
    Bool(bool),
    Ident(String),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(val) => write!(f, "{val}"),
            Self::Bool(val) => write!(f, "{val}"),
            Self::Nil => write!(f, "nil"),
            Self::Ident(str) | Self::String(str) => write!(f, "{str}"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Env {
    values: HashMap<String, Value>,
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub(crate) fn get(&mut self, name: &str) -> IntpResult<&Value> {
        if let Some(value) = self.values.get(name) {
            return Ok(value);
        }
        Err(RoxError::RuntimeError)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Interpreter {
    pub(crate) env: Env,
}

type IntpResult<T> = Result<T, RoxError>;

impl Interpreter {
    pub(crate) fn new() -> Self {
        Self { env: Env::new() }
    }

    fn stmt_print(&mut self, expr: Expr) -> IntpResult<()> {
        let value = self.eval_expr(expr)?;
        println!("{value}");
        Ok(())
    }

    fn stmt_var(&mut self, var: Var) -> IntpResult<()> {
        let name = var.name().clone();
        let value = match var.initializer {
            Some(expr) => self.eval_expr(expr)?,
            None => Value::Nil,
        };

        self.env.define(name, value);

        Ok(())
    }

    fn eval_expr(&mut self, expr: Expr) -> IntpResult<Value> {
        match expr {
            Expr::Bin(expr) => self.expr_binary(expr),
            Expr::Literal(literal) => self.expr_literal(literal),
            Expr::Var(str) => {
                let value = self.env.get(&str)?;
                Ok(value.clone())
            }
            _ => todo!(),
        }
    }

    fn expr_binary(&mut self, expr: Bin) -> IntpResult<Value> {
        let left = self.eval_expr(*expr.left)?;
        let right = self.eval_expr(*expr.right)?;
        let op = expr.op;
        match op {
            Kind::Plus => {
                self.check_affinity(&left, &right)?;
                match (left, right) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}"))),
                    (_, _) => unreachable!(),
                }
            }

            Kind::Minus => match (left, right) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                (_, _) => Err(RoxError::RuntimeError),
            },
            Kind::Star => match (left, right) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                (_, _) => Err(RoxError::RuntimeError),
            },
            Kind::Slash => match (left, right) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                (_, _) => Err(RoxError::RuntimeError),
            },
            _ => unreachable!(),
        }
    }

    fn expr_literal(&mut self, literal: Literal) -> IntpResult<Value> {
        let res = match literal {
            Literal::String(str) => Value::String(str),
            Literal::Bool(bool) => Value::Bool(bool),
            Literal::Number(number) => Value::Number(number),
            Literal::Nil => Value::Nil,
            Literal::Ident(ident) => Value::Ident(ident),
        };
        Ok(res)
    }

    pub(crate) fn run(&mut self, program: Vec<Stmt>) -> IntpResult<()> {
        for stmt in program {
            match stmt {
                Stmt::VarDecl(var) => self.stmt_var(var)?,
                Stmt::Print(expr) => self.stmt_print(expr)?,
                Stmt::Expr(expr) => {
                    self.eval_expr(expr)?;
                }
                _ => todo!(),
            }
        }
        Ok(())
    }

    fn check_affinity(&self, a: &Value, b: &Value) -> IntpResult<()> {
        match (a, b) {
            (Value::Number(_), Value::Number(_)) => Ok(()),
            (Value::String(_), Value::String(_)) => Ok(()),
            (_, _) => Err(RoxError::RuntimeError),
        }
    }
}

fn interpret(program: Vec<Stmt>) -> Result<(), RoxError> {
    let mut interpreter = Interpreter::new();
    interpreter.run(program)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser, token::Token};

    fn lex(source: &str) -> Result<Vec<Token>, RoxError> {
        lexer::lex(source).map_err(|_| RoxError::LexerError)
    }

    fn parse(source: &str) -> Result<Vec<Stmt>, RoxError> {
        let tokens = lex(source)?;
        parser::parse(tokens).map_err(|_| RoxError::ParseError)
    }

    #[test]
    fn eval_expr() {
        let source = "print 1 + 1;";
        let ast = parse(source);
        assert!(ast.is_ok());
        let result = interpret(ast.unwrap());

        assert!(result.is_ok());
    }

    #[test]
    fn eval_var_decl() {
        let source = "var x = true;";
        let ast = parse(source);
        assert!(ast.is_ok());
        let mut interpreter = Interpreter::new();
        assert!(interpreter.run(ast.unwrap()).is_ok());
        assert_eq!(interpreter.env.get("x"), Ok(&Value::Bool(true)));
    }

    #[test]
    fn eval_var() {
        let source = "var x = true; print x;";
        let ast = parse(source);
        assert!(ast.is_ok());
        let result = interpret(ast.unwrap());

        assert!(result.is_ok());
    }
}
