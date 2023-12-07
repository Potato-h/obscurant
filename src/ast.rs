use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constant {
    Integer(i128),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident(pub String);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Pointer(Rc<Ty>),
    Array(Rc<Ty>, Option<Rc<Expression>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Inc,
    Dec,
    Ref,
    Deref,
    Plus,
    Minus,
    Inversion,
    Negation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PostOp {
    Index(Rc<Expression>),
    Call(Vec<Expression>),
    Dot(Ident),
    Arrow(Ident),
    Inc,
    Dec,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    LAND,
    LOR,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp {
    Mov,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(Ident),
    Constant(Constant),
    Literal(String),
    Cast(Ty, Rc<Expression>),
    Binary(Rc<Expression>, BinOp, Rc<Expression>),
    Conditional(Rc<Expression>, Rc<Expression>, Rc<Expression>),
    Unary(UnOp, Rc<Expression>),
    Postfix(Rc<Expression>, PostOp),
    Assignment(Rc<Expression>, AssignOp, Rc<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Selection {
    If(Expression, Statement),
    IfElse(Expression, Statement, Statement),
    Switch(Expression, Statement),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Iteration {
    While(Expression, Statement),
    Do(Statement, Expression),
    For(Statement, Statement, Expression, Statement),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Jump {
    Continue,
    Break,
    Return(Option<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Option<Expression>),
    Jump(Rc<Jump>),
    Selection(Rc<Selection>),
    Iteration(Rc<Iteration>),
    Declaration(Rc<Declaration>),
    Compound(Vec<Statement>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    pub id: Ident,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub var: Variable,
    pub init: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub id: Ident,
    pub args: Vec<Variable>,
    pub ret_ty: Ty,
    pub body: Statement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Item {
    Function(Function),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Unit {
    pub items: Vec<Item>,
}
