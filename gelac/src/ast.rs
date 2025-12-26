use crate::lex::{NameKind, Token};

//> Statement
#[derive(Debug)]
pub enum Statement {
    Let(Name, ExpressionRef),
    Struct(Name, StructFields),
    Enum(Name, EnumVariants),
    Import(
        Name,
        Option<Name>, /* alias */
        Option<NamesWithAliases>,
    ),
}

pub struct Name(pub String, pub NameKind);

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Name({:?}, {:?})", self.0, self.1)
    }
}

#[derive(Debug)]
pub struct StructFields(pub Vec<(Name, Name)>);

#[derive(Debug)]
pub struct EnumVariants(pub Vec<Name>);

#[derive(Debug)]
pub struct NamesWithAliases(pub Vec<(Name, Option<Name>)>);
//< Statement

//> Expression
pub struct ExpressionRef(u32);

impl std::fmt::Debug for ExpressionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(ref {})", self.0)
    }
}

pub struct ExpressionArena(pub Vec<Expression>);

impl ExpressionArena {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn allocate(&mut self, node: Expression) -> ExpressionRef {
        let i = self.0.len();
        self.0.push(node);
        ExpressionRef(i as u32)
    }
}

#[derive(Debug)]
pub enum Expression {
    Atom(Atom),
    Abstraction(Name, ExpressionRef),
    Bind(BindVariable, ExpressionRef /* body */),
    When(WhenBranches),
    If(
        ExpressionRef,
        ExpressionRef, /* then */
        ExpressionRef, /* else */
    ),
    New(Name, NewFields),
    TypeCast(ExpressionRef, Name),
    Binary(
        BinaryOperator,
        ExpressionRef, /* lhs */
        ExpressionRef, /* rhs */
    ),
    Application(ExpressionRef, Vec<ExpressionRef> /* arguments */),
    Property(Atom, Vec<Atom> /* properties */),
}

#[derive(Debug)]
pub struct BindVariable(pub Name, pub ExpressionRef);

#[derive(Debug)]
pub struct BinaryOperator(pub Token);

#[derive(Debug)]
pub struct WhenBranches(
    pub Vec<(ExpressionRef, ExpressionRef)>,
    pub ExpressionRef, /* default */
);

#[derive(Debug)]
pub struct NewFields(pub Vec<(Name, ExpressionRef)>);

pub enum Atom {
    Variable(Name),
    Integer(i128),
    String(String),
    Parenthesized(ExpressionRef),
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Variable(name) => write!(f, "Variable({:?})", name.0),
            Self::Integer(value) => write!(f, "Integer({value})"),
            Self::String(text) => write!(f, "String({text:?})"),
            Self::Parenthesized(inner) => {
                f.debug_tuple("Parenthesized").field(inner).finish()
            }
        }
    }
}
//< Expression
