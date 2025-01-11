#[derive(Debug, Clone, Eq, PartialEq, PartialOrd)]
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
    String,
    Bang,
    Eq,
    EqEq,
    Gt,
    GtEq,
    LtEq,
    Lt,
    NtEq,
    Print,
    LBrace,
    RBrace,
    While,
    For,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) enum Literal {
    Number(i64),
    String(String),
    Ident(String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) line: usize,
    pub(crate) literal: Option<Literal>,
}
