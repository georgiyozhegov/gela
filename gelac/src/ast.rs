use crate::lex::Token;
use crate::parse::ParserError;

//> Statement
#[derive(Debug)]
pub enum Statement {
    Let(Name, Expression),
    Struct(Name, StructFields),
    Enum(Name, EnumVariants),
    Import(Name, Option<NamesWithAliases>),
}

pub struct Name(pub String);

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Name({:?})", self.0)
    }
}

#[derive(Debug)]
pub struct StructFields(pub Vec<(Name, Name)>);

impl StructFields {
    pub fn check(&self) -> Result<(), ParserError> {
        if self.0.is_empty() {
            Err(ParserError::EmptyStruct)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct EnumVariants(pub Vec<Name>);

impl EnumVariants {
    pub fn check(&self) -> Result<(), ParserError> {
        if self.0.is_empty() {
            Err(ParserError::EmptyEnum)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct NamesWithAliases(pub Vec<(Name, Option<Name>)>);
//< Statement

//> Expression
#[derive(Debug)]
pub enum Expression {
    Atom(Atom),
    Abstraction(Name, Box<Expression>),
    Bind(
        BindVariable,
        Box<Expression>, /* body */
    ),
    When(WhenBranches),
    If(
        Box<Expression>,
        Box<Expression>, /* then */
        Box<Expression>, /* else */
    ),
    New(Name, NewFields),
    TypeCast(Box<Expression>, Name),
    Binary(
        BinaryOperator,
        Box<Expression>, /* lhs */
        Box<Expression>, /* rhs */
    ),
    Application(Atom, Vec<Atom>),
}

#[derive(Debug)]
pub struct BindVariable(pub Name, pub Box<Expression>);

#[derive(Debug)]
pub struct BinaryOperator(pub Token);

#[derive(Debug)]
pub struct WhenBranches(
    pub Vec<(Expression, Expression)>,
    pub Box<Expression>, /* default */
);

#[derive(Debug)]
pub struct NewFields(pub Vec<(Name, Expression)>);

pub enum Atom {
    Variable(Name),
    Integer(i128),
    String(String),
    Parenthesized(Box<Expression>),
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
