use logos::{Lexer, Logos};
use std::fmt; // to implement the Display trait

fn literal(lex: &mut Lexer<Token>) -> String {
    let mut res: String = lex.slice().chars().skip(1).collect();
    res.pop(); // pop closing "
    res
}

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token {
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().parse())]
    Identifier(String),

    #[regex("\"([^\"]|(\\\\\"))*\"", literal)]
    Literal(String),

    #[regex(r"\d+", |lex| lex.slice().parse())]
    Integer(i128),

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,

    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,

    #[token("++")]
    Inc,
    #[token("--")]
    Dec,

    #[token("&")]
    Ref,
    #[token("~")]
    Inversion,
    #[token("!")]
    Negation,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("%")]
    OperatorMod,

    #[token("<")]
    LT,
    #[token(">")]
    GT,
    #[token("<=")]
    LE,
    #[token(">=")]
    GE,
    #[token("==")]
    EQ,
    #[token("!=")]
    NE,

    #[token("&&")]
    LAND,
    #[token("||")]
    LOR,

    #[token("?")]
    Question,
    #[token(":")]
    Colon,

    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    ModAssign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,

    #[token("void")]
    Void,
    #[token("char")]
    Char,
    #[token("short")]
    Short,
    #[token("int")]
    Int,
    #[token("long")]
    Long,
    #[token("float")]
    Float,
    #[token("double")]
    Double,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("switch")]
    Switch,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("do")]
    Do,

    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("return")]
    Return,

    #[regex(r"#.*\n?", logos::skip)]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
