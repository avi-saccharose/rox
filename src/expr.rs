use crate::token::Kind;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Stmt {
    Print(Expr),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Expr {
    Bin(Bin),
    Unary(Unary),
    Grouping(Box<Expr>),
    Literal(Literal),
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
