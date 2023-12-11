use std::{fmt::Display, rc::Rc};

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
pub struct Compound {
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Option<Expression>),
    Jump(Rc<Jump>),
    Selection(Rc<Selection>),
    Iteration(Rc<Iteration>),
    Declaration(Vec<Declaration>),
    Compound(Compound),
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

struct Punctuated<'a, T, Sep>(&'a [T], Sep);

impl<'a, T: Display, Sep: Display> Display for Punctuated<'a, T, Sep> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            write!(f, "{v}")?;
            if i != self.0.len() - 1 {
                write!(f, "{}", self.1)?;
            }
        }

        Ok(())
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Integer(v) => write!(f, "{}", v),
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Ty {
    /// Unwraps pointer and array layers to get primitive type
    fn primitive(&self) -> Ty {
        match self {
            Ty::Pointer(ty) => ty.primitive(),
            Ty::Array(ty, _) => ty.primitive(),
            _ => self.clone(),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Void => write!(f, "void"),
            Ty::Char => write!(f, "char"),
            Ty::Short => write!(f, "short"),
            Ty::Int => write!(f, "int"),
            Ty::Long => write!(f, "long"),
            Ty::Float => write!(f, "float"),
            Ty::Double => write!(f, "double"),
            Ty::Pointer(ty) => write!(f, "{ty}*"),
            Ty::Array(ty, _) => write!(f, "{ty}[]"),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Inc => write!(f, "++"),
            UnOp::Dec => write!(f, "--"),
            UnOp::Ref => write!(f, "&"),
            UnOp::Deref => write!(f, "*"),
            UnOp::Plus => write!(f, "+"),
            UnOp::Minus => write!(f, "-"),
            UnOp::Inversion => write!(f, "~"),
            UnOp::Negation => write!(f, "!"),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::LT => write!(f, "<"),
            BinOp::GT => write!(f, ">"),
            BinOp::LE => write!(f, "<="),
            BinOp::GE => write!(f, ">="),
            BinOp::EQ => write!(f, "=="),
            BinOp::NE => write!(f, "!="),
            BinOp::LAND => write!(f, "&&"),
            BinOp::LOR => write!(f, "||"),
        }
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOp::Mov => write!(f, "="),
            AssignOp::Mul => write!(f, "*="),
            AssignOp::Div => write!(f, "/="),
            AssignOp::Mod => write!(f, "%="),
            AssignOp::Add => write!(f, "+="),
            AssignOp::Sub => write!(f, "-="),
        }
    }
}

impl Display for PostOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PostOp::Index(idx) => write!(f, "[{idx}]"),
            PostOp::Call(exprs) => write!(f, "({})", Punctuated(exprs, ", ")),
            PostOp::Dot(id) => write!(f, ".{id}"),
            PostOp::Arrow(id) => write!(f, "->{id}"),
            PostOp::Inc => write!(f, "++"),
            PostOp::Dec => write!(f, "--"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{id}"),
            Expression::Constant(c) => write!(f, "{c}"),
            Expression::Literal(lit) => write!(f, "{lit:?}"),
            Expression::Cast(ty, expr) => write!(f, "({ty})({expr})"),
            Expression::Binary(lhs, op, rhs) => write!(f, "({lhs} {op} {rhs})"),
            Expression::Conditional(cond, lhs, rhs) => write!(f, "({cond} ? {lhs} : {rhs})"),
            Expression::Unary(op, expr) => write!(f, "{op}{expr}"),
            Expression::Postfix(expr, op) => write!(f, "{expr}{op}"),
            Expression::Assignment(lhs, op, rhs) => write!(f, "{lhs} {op} {rhs}"),
        }
    }
}

impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Jump::Continue => write!(f, "continue;"),
            Jump::Break => write!(f, "break;"),
            Jump::Return(Some(expr)) => write!(f, "return {expr};"),
            Jump::Return(None) => write!(f, "return;"),
        }
    }
}

impl Display for Selection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Selection::If(cond, stat) => writeln!(f, "if ({cond})\n{stat}"),
            Selection::IfElse(cond, stat1, stat2) => {
                writeln!(f, "if ({cond})\n{stat1}\nelse\n{stat2}")
            }
            Selection::Switch(_, _) => todo!(),
        }
    }
}

impl Variable {
    /// Wrap all pointers and arrays around identifier
    fn declaration(&self) -> String {
        fn helper(ty: &Ty, done: String) -> String {
            match ty {
                Ty::Array(ty, Some(expr)) => helper(ty, format!("({done}[{expr}])")),
                Ty::Array(ty, None) => helper(ty, format!("({done}[])")),
                Ty::Pointer(ty) => helper(ty, format!("(*{done})")),
                _ => format!("{done}"),
            }
        }

        helper(&self.ty, self.id.0.clone())
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn helper(ty: &Ty, done: String) -> String {
            match ty {
                Ty::Array(ty, Some(expr)) => helper(ty, format!("({done}[{expr}])")),
                Ty::Array(ty, None) => helper(ty, format!("({done}[])")),
                Ty::Pointer(ty) => helper(ty, format!("(*{done})")),
                _ => format!("{ty} {done}"),
            }
        }

        write!(f, "{}", helper(&self.ty, self.id.0.clone()))
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.init {
            Some(expr) => write!(f, "{} = {expr}", self.var.declaration()),
            None => write!(f, "{}", self.var.declaration()),
        }
    }
}

impl Display for Iteration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Iteration::While(expr, stat) => write!(f, "while ({expr}) {stat}"),
            Iteration::For(init, cond, step, stat) => {
                write!(f, "for ({init} {cond} {step}) {stat}")
            }
            Iteration::Do(_, _) => todo!(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(Some(expr)) => write!(f, "{expr};"),
            Statement::Expr(None) => write!(f, ";"),
            Statement::Jump(jmp) => write!(f, "{jmp}"),
            Statement::Selection(selection) => write!(f, "{selection}"),
            Statement::Iteration(iteration) => write!(f, "{iteration}"),
            Statement::Declaration(decls) => write!(
                f,
                "{} {};",
                decls[0].var.ty.primitive(),
                Punctuated(decls, ", ")
            ),
            Statement::Compound(stats) => write!(f, "{{\n{}\n}}", Punctuated(&stats.body, "\n")),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{ty} {id}({args}) {body}",
            ty = self.ret_ty,
            id = self.id,
            args = Punctuated(&self.args, ", "),
            body = self.body
        )
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Function(v) => write!(f, "{v}"),
        }
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in self.items.iter() {
            write!(f, "{item}")?;
        }

        Ok(())
    }
}

pub trait Obfuscate {
    fn obfuscate(&self) -> Self;
}

fn encode_char(buf: &mut String, ch: char) {
    let mut num: u32 = ch.into();

    loop {
        match num % 4 {
            0 => buf.push('0'),
            1 => buf.push('1'),
            2 => buf.push('O'),
            _ => buf.push('I'),
        }

        num /= 4;

        if num == 0 {
            break;
        }
    }
}

impl Obfuscate for Ident {
    fn obfuscate(&self) -> Self {
        let mut buf = String::from('O');
        self.0.chars().for_each(|ch| encode_char(&mut buf, ch));
        Ident(buf)
    }
}

impl Obfuscate for PostOp {
    fn obfuscate(&self) -> Self {
        match self {
            PostOp::Index(idx) => PostOp::Index(Rc::new(idx.obfuscate())),
            PostOp::Call(args) => PostOp::Call(args.iter().map(|arg| arg.obfuscate()).collect()),
            _ => self.clone(),
        }
    }
}

impl Obfuscate for Expression {
    fn obfuscate(&self) -> Expression {
        match self {
            Expression::Identifier(id) => Expression::Identifier(id.obfuscate()),
            Expression::Constant(cons) => Expression::Constant(cons.clone()),
            Expression::Literal(lit) => Expression::Literal(lit.clone()),
            Expression::Cast(ty, expr) => Expression::Cast(ty.clone(), Rc::new(expr.obfuscate())),
            Expression::Binary(lhs, op, rhs) => Expression::Binary(
                Rc::new(lhs.obfuscate()),
                op.clone(),
                Rc::new(rhs.obfuscate()),
            ),
            Expression::Conditional(cond, lhs, rhs) => Expression::Conditional(
                Rc::new(cond.obfuscate()),
                Rc::new(lhs.obfuscate()),
                Rc::new(rhs.obfuscate()),
            ),
            Expression::Unary(op, expr) => Expression::Unary(op.clone(), Rc::new(expr.obfuscate())),
            Expression::Postfix(expr, op) => {
                if let PostOp::Call(_) = op {
                    Expression::Postfix(expr.clone(), op.obfuscate())
                } else {
                    Expression::Postfix(Rc::new(expr.obfuscate()), op.obfuscate())
                }
            }
            Expression::Assignment(expr, op, val) => Expression::Assignment(
                Rc::new(expr.obfuscate()),
                op.clone(),
                Rc::new(val.obfuscate()),
            ),
        }
    }
}

impl Obfuscate for Variable {
    fn obfuscate(&self) -> Self {
        Variable {
            id: self.id.obfuscate(),
            ty: self.ty.clone(),
        }
    }
}

impl Obfuscate for Selection {
    fn obfuscate(&self) -> Self {
        match self {
            Selection::If(cond, stat) => Selection::If(cond.obfuscate(), stat.obfuscate()),
            Selection::IfElse(cond, then, el) => {
                Selection::IfElse(cond.obfuscate(), then.obfuscate(), el.obfuscate())
            }
            Selection::Switch(_, _) => todo!(),
        }
    }
}

impl Obfuscate for Iteration {
    fn obfuscate(&self) -> Self {
        match self {
            Iteration::While(cond, stat) => Iteration::While(cond.obfuscate(), stat.obfuscate()),
            Iteration::Do(_, _) => todo!(),
            Iteration::For(init, cond, step, stat) => Iteration::For(
                init.obfuscate(),
                cond.obfuscate(),
                step.obfuscate(),
                stat.obfuscate(),
            ),
        }
    }
}

impl Obfuscate for Declaration {
    fn obfuscate(&self) -> Self {
        Declaration {
            var: self.var.obfuscate(),
            init: self.init.as_ref().map(|expr| expr.obfuscate()),
        }
    }
}

impl Obfuscate for Compound {
    fn obfuscate(&self) -> Self {
        Compound {
            body: self.body.iter().map(|stat| stat.obfuscate()).collect(),
        }
    }
}

impl Obfuscate for Statement {
    fn obfuscate(&self) -> Self {
        match self {
            Statement::Expr(expr) => Statement::Expr(expr.as_ref().map(|expr| expr.obfuscate())),
            Statement::Jump(_) => self.clone(),
            Statement::Selection(sel) => Statement::Selection(Rc::new(sel.obfuscate())),
            Statement::Iteration(iter) => Statement::Iteration(Rc::new(iter.obfuscate())),
            Statement::Declaration(decls) => {
                Statement::Declaration(decls.iter().map(|decl| decl.obfuscate()).collect())
            }
            Statement::Compound(comp) => Statement::Compound(comp.obfuscate()),
        }
    }
}

impl Obfuscate for Function {
    fn obfuscate(&self) -> Self {
        Function {
            id: self.id.clone(),
            args: self.args.iter().map(|arg| arg.obfuscate()).collect(),
            ret_ty: self.ret_ty.clone(),
            body: self.body.obfuscate(),
        }
    }
}

impl Obfuscate for Item {
    fn obfuscate(&self) -> Self {
        match self {
            Item::Function(fun) => Item::Function(fun.obfuscate()),
        }
    }
}

impl Obfuscate for Unit {
    fn obfuscate(&self) -> Self {
        Unit {
            items: self.items.iter().map(|item| item.obfuscate()).collect(),
        }
    }
}
