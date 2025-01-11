use crate::token::{self, Kind, Token};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Stmt {
    Print(Expr),
    Expr(Expr),
    VarDecl(Var),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) struct Var {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl Var {
    pub fn name(&self) -> &String {
        match self.name.literal.as_ref().unwrap() {
            token::Literal::Ident(name) => name,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Expr {
    Bin(Bin),
    Unary(Unary),
    Grouping(Box<Expr>),
    Literal(Literal),
    Var(String),
    Assign(Assign),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) struct Bin {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: Kind,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) struct Unary {
    pub right: Box<Expr>,
    pub op: Kind,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Literal {
    Number(i64),
    String(String),
    Bool(bool),
    Ident(String),
    Nil,
}
