use std::{iter::{Peekable}, vec::IntoIter};

use crate::lex::Token;

pub fn parse(tokens: Vec<Token>) -> Vec<Result<Statement, ParserError>> {
    let mut parser = Parser::new(tokens);
    std::iter::from_fn(move || {
        (parser.peek().is_some()).then(|| parser.parse_statement())
    })
    .collect()
}

#[derive(Debug)]
pub enum ParserError {
    Unexpected { actual: Token, expected: Token, context: String },
    UnexpectedWithMessage { actual: Token, message: String, context: String },
    UnexpectedEof { expected: String, context: String }, // Location can be restored
    EmptyStruct,
    EmptyEnum,
}

impl ParserError {
    pub fn unexpected(actual: Option<Token>, expected: Token, context: impl ToString) -> Self {
        if let Some(actual) = actual {
            Self::Unexpected { actual, expected, context: context.to_string() }
        } else {
            Self::UnexpectedEof { expected: expected.to_string(), context: context.to_string() }
        }
    }

    pub fn unexpected_with_message(actual: Option<Token>, message: impl ToString, context: impl ToString) -> Self {
        if let Some(actual) = actual {
            Self::UnexpectedWithMessage { actual, message: message.to_string(), context: context.to_string() }
        } else {
            Self::UnexpectedEof { expected: message.to_string(), context: context.to_string() }
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    #[must_use]
    fn eat(&mut self, expected: Token, context: impl ToString) -> Result<(), ParserError> {
        match self.next() {
            Some(actual) if actual == expected => Ok(()),
            other => Err(ParserError::unexpected(other, expected, context)),
        }
    }

    #[must_use]
    fn eat_name(&mut self, context: impl ToString) -> Result<Name, ParserError> {
        match self.next() {
            Some(Token::Name(name)) => Ok(Name(name)),
            other => Err(ParserError::unexpected_with_message(other, "name", context)),
        }
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.peek().cloned()?;
        self.cursor += 1;
        Some(token)
    }

    fn peek(&self) -> Option<&Token> {
        self.nth(0)
    }

    fn nth(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + offset)
    }

    fn is_open_curly(&mut self) -> bool {
        matches!(self.peek(), Some(Token::OpenCurly))
    }

    fn is_close_curly(&mut self) -> bool {
        matches!(self.peek(), Some(Token::CloseCurly))
    }

    fn is_as(&mut self) -> bool {
        matches!(self.peek(), Some(Token::As))
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(Name, Expression),
    Struct(Name, StructFields),
    Enum(Name, EnumVariants),
    Import(
        Name,
        Option<NamesWithAliases>,
    ),
}

pub struct Name(String);

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Name({:?})", self.0)
    }
}

#[derive(Debug)]
pub struct StructFields(Vec<(Name, Name)>);

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
pub struct EnumVariants(Vec<Name>);

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
pub struct NamesWithAliases(Vec<(Name, Option<Name>)>);

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::Struct) => self.parse_struct(),
            Some(Token::Enum) => self.parse_enum(),
            Some(Token::Import) => self.parse_import(),
            _ => Err(ParserError::unexpected_with_message(self.next(), "statement", "global scope")),
        }
    }

    //> Let
    pub fn parse_let(&mut self) -> Result<Statement, ParserError> {
        let context = "let statement";
        self.eat(Token::Let, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::Equals, &context)?;
        let value = self.parse_expression()?;
        Ok(Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(&mut self) -> Result<Statement, ParserError> {
        let context = "struct definition";
        self.eat(Token::Struct, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let fields = self.parse_struct_fields()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(Statement::Struct(name, fields))
    }

    pub fn parse_struct_fields(&mut self) -> Result<StructFields, ParserError> {
        let context = "struct fields";
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(&context)?;
            self.eat(Token::Colon, &context)?;
            let ty = self.eat_name(&context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, &context)?;
            }
            let field = (name, ty);
            fields.push(field);
        }
        let fields = StructFields(fields);
        fields.check()?;
        Ok(fields)
    }
    //< Struct

    //> Enum
    pub fn parse_enum(&mut self) -> Result<Statement, ParserError> {
        let context = "enum definition";
        self.eat(Token::Enum, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let variants = self.parse_enum_variants()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(Statement::Enum(name, variants))
    }

    pub fn parse_enum_variants(&mut self) -> Result<EnumVariants, ParserError> {
        let context = "enum variants";
        let mut variants = Vec::new();
        while !self.is_close_curly() {
            let variant = self.eat_name(&context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, &context)?;
            }
            variants.push(variant);
        }
        let variants = EnumVariants(variants);
        variants.check()?;
        Ok(variants)
    }
    //< Enum

    //> Import
    pub fn parse_import(&mut self) -> Result<Statement, ParserError> {
        let context = "import statement";
        self.eat(Token::Import, &context)?;
        let module = self.eat_name(&context)?;
        if !self.is_open_curly() {
            return Ok(Statement::Import(module, None));
        }
        self.eat(Token::OpenCurly, &context)?;
        let names_with_aliases = self.parse_names_with_aliases()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(Statement::Import(module, Some(names_with_aliases)))
    }

    pub fn parse_names_with_aliases(&mut self) -> Result<NamesWithAliases, ParserError> {
        let context = "imported names with aliases";
        let mut names_with_aliases = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(&context)?;
            let alias: Option<Name> = if self.is_as() {
                self.eat(Token::As, &context)?;
                Some(self.eat_name(&context)?)
            } else {
                None
            };
            if !self.is_close_curly() {
                self.eat(Token::Comma, &context)?;
            }
            let name_with_alias = (name, alias);
            names_with_aliases.push(name_with_alias);
        }
        Ok(NamesWithAliases(names_with_aliases))
    }
    //< Import
}

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let context = "statement body";
        match self.peek() {
            Some(Token::Name(_)) if matches!(self.nth(1), Some(Token::Arrow)) => self.parse_abstraction(),
            Some(_) => self.parse_infix_lowest(),
            _ => Err(ParserError::unexpected_with_message(self.next(), "expression", &context)),
        }
    }

    pub fn parse_abstraction(&mut self) -> Result<Expression, ParserError> {
        let context = "abstraction";
        let parameter = self.eat_name(&context)?;
        self.eat(Token::Arrow, &context)?;
        let body = self.parse_expression()?;
        Ok(Expression::Abstraction(parameter, Box::new(body)))
    }

    pub fn parse_type_cast(&mut self) -> Result<Expression, ParserError> {
        let context = "type cast";
        let mut value = self.parse_infix_lowest()?;
        if let Some(Token::ColonColon) = self.peek() {
            self.next();
            let ty = self.eat_name(&context)?;
            value = Expression::TypeCast(Box::new(value), ty);
        }
        Ok(value)
    }

    //> InfixLowest
    pub fn parse_infix_lowest(&mut self) -> Result<Expression, ParserError> {
        let context = "infix lowest";
        let mut lhs = self.parse_infix_lower()?;
        if self.is_infix_lowest_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_lowest_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Dollar))
    }
    //< InfixLowest

    //> InfixLower
    pub fn parse_infix_lower(&mut self) -> Result<Expression, ParserError> {
        let context = "infix lower";
        let mut lhs = self.parse_infix_low()?;
        if self.is_infix_lowest_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_lower_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::And | Token::Or))
    }
    //< InfixLower

    //> InfixLow
    pub fn parse_infix_low(&mut self) -> Result<Expression, ParserError> {
        let context = "infix low";
        let mut lhs = self.parse_infix_high()?;
        if self.is_infix_low_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_low()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_low_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::EqualEqual | Token::NotEqual | Token::LessEqual | Token::GreaterEqual | Token::Less | Token::Greater))
    }
    //< InfixLow

    //> InfixHigh
    pub fn parse_infix_high(&mut self) -> Result<Expression, ParserError> {
        let context = "infix high";
        let mut lhs = self.parse_infix_higher()?;
        if self.is_infix_higher_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_high()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_high_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Plus | Token::Minus))
    }
    //< InfixHigh

    //> InfixHigher
    pub fn parse_infix_higher(&mut self) -> Result<Expression, ParserError> {
        let context = "infix higher";
        let mut lhs = self.parse_infix_highest()?;
        if self.is_infix_higher_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_higher()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_higher_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Asterisk | Token::Slash | Token::Percent))
    }
    //< InfixHigher

    //> InfixHighest
    pub fn parse_infix_highest(&mut self) -> Result<Expression, ParserError> {
        let context = "infix highest";
        let mut lhs = self.parse_application()?;
        if self.is_infix_highest_operator() {
            let operator = BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_highest()?;
            lhs = Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_highest_operator(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Caret))
    }
    //< InfixHighest

    pub fn parse_application(&mut self) -> Result<Expression, ParserError> {
        self.next();
        Ok(Expression::Application(Atom::Variable(Name("dummy".into())), vec![]))
    }
}

#[derive(Debug)]
pub enum Expression {
    Atom(Atom),
    Abstraction(Name, Box<Expression>),
    Bind(Name, Box<Expression> /* value */, Box<Expression> /* body */),
    When(WhenBranches),
    If(Box<Expression>, Box<Expression> /* then */, Box<Expression> /* else */),
    New(Name, NewFields),
    TypeCast(Box<Expression>, Name),
    Binary(BinaryOperator, Box<Expression> /* lhs */, Box<Expression> /* rhs */),
    Application(Atom, Vec<Atom>),
}

#[derive(Debug)]
pub struct BinaryOperator(Token);

#[derive(Debug)]
pub struct WhenBranches(Vec<(Expression, Expression)>, Box<Expression> /* default */);

#[derive(Debug)]
pub struct NewFields(Vec<(Name, Expression)>);

#[derive(Debug)]
pub enum Atom {
    Variable(Name),
    Integer(i128),
    String(String),
    Parenthesized(Box<Expression>),
}
