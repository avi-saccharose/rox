use core::fmt;
use std::{cell::RefCell, collections::HashMap, fmt::write, ops::Deref, rc::Rc};

use crate::{
    error::RoxError,
    expr::{Bin, Expr, If, Literal, Stmt, Var},
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
    enclosing: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn get(&self, name: &str) -> IntpResult<Value> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        if self.enclosing.is_none() {
            return Err(RoxError::RuntimeError);
        }
        let enclosing = self.enclosing.as_ref().unwrap().borrow();
        enclosing.get(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Interpreter {
    pub(crate) env: Rc<RefCell<Env>>,
}

type IntpResult<T> = Result<T, RoxError>;

impl Interpreter {
    pub(crate) fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.env.borrow_mut().define(name, value);
    }

    fn get(&self, name: &str) -> IntpResult<Value> {
        self.env.borrow().get(name)
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Bool(bool) => *bool,
            _ => true,
        }
    }

    pub(crate) fn eval_stmt(&mut self, stmt: Stmt) -> IntpResult<()> {
        match stmt {
            Stmt::VarDecl(var) => self.stmt_var(var)?,
            Stmt::Print(expr) => self.stmt_print(expr)?,
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
            }
            Stmt::Block(stmts) => self.stmt_block(stmts, Env::new())?,
            _ => todo!(),
        }
        Ok(())
    }

    fn stmt_if(&mut self, stmt: If) -> IntpResult<()> {
        let result = self.eval_expr(stmt.cond)?;
        let result = self.is_truthy(&result);
        if result {
            self.eval_stmt(*stmt.then_branch)?;
        } else if let Some(branch) = stmt.else_branch {
            self.eval_stmt(*branch)?;
        }
        Ok(())
    }
    fn stmt_block(&mut self, block: Vec<Stmt>, _: Env) -> IntpResult<()> {
        let previous = std::mem::replace(&mut self.env, Rc::new(RefCell::new(Env::new())));
        self.env.borrow_mut().enclosing = Some(Rc::clone(&previous));
        for stmt in block {
            self.eval_stmt(stmt)?;
        }
        let _ = std::mem::replace(&mut self.env, previous);
        Ok(())
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

        self.define(name, value);

        Ok(())
    }

    fn eval_expr(&mut self, expr: Expr) -> IntpResult<Value> {
        match expr {
            Expr::Bin(expr) => self.expr_binary(expr),
            Expr::Literal(literal) => self.expr_literal(literal),
            Expr::Var(str) => {
                let value = self.get(&str)?;
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
                Stmt::If(stmt) => self.stmt_if(stmt)?,
                Stmt::VarDecl(var) => self.stmt_var(var)?,
                Stmt::Print(expr) => self.stmt_print(expr)?,
                Stmt::Expr(expr) => {
                    self.eval_expr(expr)?;
                }
                Stmt::Block(stmts) => self.stmt_block(stmts, Env::new())?,
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

    fn wrap_stmt(stmt: Stmt) -> Vec<Stmt> {
        vec![stmt]
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
        assert_eq!(interpreter.get("x"), Ok(Value::Bool(true)));
    }

    #[test]
    fn eval_var() {
        let source = "var x = true; print x;";
        let ast = parse(source);
        assert!(ast.is_ok());
        let result = interpret(ast.unwrap());

        assert!(result.is_ok());
    }

    #[test]
    fn eval_block() {
        let source = "{ var b = 1; print b; } 
                        print b;";
        let ast = parse(source);
        assert!(&ast.is_ok());
        let ast = ast.unwrap();
        let mut interpreter = Interpreter::new();
        let block = wrap_stmt(ast[0].clone());
        let expr = wrap_stmt(ast[1].clone());
        assert!(interpreter.run(block).is_ok());
        assert!(interpreter.run(expr).is_err());
    }

    #[test]
    fn eval_if() {
        let source = "if(false) print true;";
        let ast = parse(source);
        assert!(ast.is_ok());
        let mut interpreter = Interpreter::new();
        assert!(interpreter.run(ast.unwrap()).is_ok());
    }
}
