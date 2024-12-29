#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum Kind {
    Lparen,
    Rparen,
    Nil,
    True,
    False,
    Semicolon,
    Num,
    If,
    Else,
    Def,
    Var,
    Ident,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Err,
    String,
    Bang,
    Eq,
    EqEq,
    Gt,
    GtEq,
    LtEq,
    Lt,
    NtEq,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Number(i64),
    String(String),
    Ident(String),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) line: usize,
    pub(crate) literal: Option<Literal>,
}
