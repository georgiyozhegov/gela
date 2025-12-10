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

#[derive(Debug)]
pub struct Name(String);

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
        let context = format!("let statement");
        self.eat(Token::Let, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::Equals, &context)?;
        let value = self.parse_expression()?;
        Ok(Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(&mut self) -> Result<Statement, ParserError> {
        let context = format!("struct definition");
        self.eat(Token::Struct, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let fields = self.parse_struct_fields()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(Statement::Struct(name, fields))
    }

    pub fn parse_struct_fields(&mut self) -> Result<StructFields, ParserError> {
        let context = format!("struct fields");
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
        let context = format!("enum definition");
        self.eat(Token::Enum, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let variants = self.parse_enum_variants()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(Statement::Enum(name, variants))
    }

    pub fn parse_enum_variants(&mut self) -> Result<EnumVariants, ParserError> {
        let context = format!("enum variants");
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
        let context = format!("import statement");
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
        let context = format!("imported names with aliases");
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
        todo!()
    }
}


fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    match tokens.peek() {
        Some(Token::New) => parse_new(tokens),
        _ => parse_binary(tokens, 0),
    }
}

fn parse_new(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    eat(tokens, Token::New);
    let ty = eat_name(tokens);
    eat(tokens, Token::OpenCurly);
    let mut fields = Vec::new();
    while tokens
        .peek()
        .is_some_and(|tk| !matches!(tk, Token::CloseCurly))
    {
        let name = eat_name(tokens);
        eat(tokens, Token::Colon);
        let value = parse_expression(tokens);
        if tokens
            .peek()
            .is_none_or(|tk| !matches!(tk, Token::CloseCurly))
        {
            eat(tokens, Token::Comma);
        }
        fields.push((name, value));
    }
    eat(tokens, Token::CloseCurly);
    Expression::New(ty, fields)
}

fn parse_binary(
    tokens: &mut Peekable<IntoIter<Token>>,
    min_precedence: u8,
) -> Expression {
    let mut left = parse_application(tokens);
    while let Some(token) = tokens.peek() {
        let precedence = current_precedence(token);
        // If precedence equals to 0, then the token isn't an operator
        if precedence == 0 || precedence < min_precedence {
            break;
        }
        // Peeked token is an operator, because its precedence > 0
        let operator = tokens.next().unwrap(); // Checked: peeked token is some
        let right =
            parse_binary(tokens, next_precedence(&operator, precedence));
        left = match operator {
            Token::Arrow => {
                // Lambda definition
                let Expression::Name(parameter) = left else {
                    panic!("Expected lambda parameter, got: {right:?}");
                };
                Expression::Lambda(parameter, Box::new(right))
            }
            // A normal binary expression
            _ => Expression::Binary(operator, Box::new(left), Box::new(right)),
        }
    }
    left
}

fn current_precedence(token: &Token) -> u8 {
    match token {
        Token::Asterisk | Token::Slash => 4,
        Token::Plus | Token::Minus => 3,
        Token::Arrow => 2,
        Token::Dollar => 1,
        _ => 0, // Not a binary operator
    }
}

fn next_precedence(token: &Token, current: u8) -> u8 {
    match token {
        Token::Plus
        | Token::Minus
        | Token::Asterisk
        | Token::Slash
        | Token::Dollar => current + 1, // Right-associative
        _ => current, // Left-associative
    }
}

fn parse_application(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut left = parse_atom(tokens);
    while let Some(token) = tokens.peek() {
        match token {
            Token::Name(_)
            | Token::Integer(_)
            | Token::String(_)
            | Token::OpenRound => {
                let right = parse_atom(tokens);
                left = Expression::Application(Box::new(left), Box::new(right));
            }
            _ => break,
        }
    }
    left
}

fn parse_atom(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    match tokens.peek() {
        Some(Token::Name(_)) => parse_name(tokens),
        Some(Token::Integer(_)) => parse_integer(tokens),
        Some(Token::String(_)) => parse_string(tokens),
        Some(Token::OpenRound) => parse_parenthesized(tokens),
        Some(Token::Var) => parse_var_in(tokens),
        Some(Token::If) => parse_if(tokens),
        Some(tk) => panic!("Expected atom, got: {tk}"),
        None => panic!("Unexpected EOF"),
    }
}

fn parse_if(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    if matches!(tokens.peek(), Some(Token::If)) {
        tokens.next();
        let condition = parse_expression(tokens);
        eat(tokens, Token::Then);
        let then = parse_expression(tokens);
        eat(tokens, Token::Else);
        let otherwise = parse_expression(tokens);
        Expression::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise: Box::new(otherwise),
        }
    } else {
        parse_binary(tokens, 0)
    }
}

fn parse_var_in(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    if matches!(tokens.peek(), Some(&Token::Var)) {
        tokens.next();
        let name = eat_name(tokens);
        eat(tokens, Token::Equals);
        let value = parse_expression(tokens);
        eat(tokens, Token::In);
        let body = parse_expression(tokens);
        Expression::VarIn {
            name,
            value: Box::new(value),
            body: Box::new(body),
        }
    } else {
        parse_binary(tokens, 0)
    }
}

fn parse_parenthesized(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    eat(tokens, Token::OpenRound);
    let inner = parse_expression(tokens);
    eat(tokens, Token::CloseRound);
    inner // There is no "parenthesized" node
}

fn parse_name(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    Expression::Name(eat_name(tokens))
}

fn eat_name(tokens: &mut Peekable<IntoIter<Token>>) -> String {
    match tokens.next() {
        Some(Token::Name(name)) => name,
        next => panic!("Expected name, got {next:?}"), // Todo: error handling
    }
}

fn parse_integer(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    match tokens.next() {
        Some(Token::Integer(value)) => Expression::Integer(value),
        next => panic!("Expected name, got {next:?}"), // Todo: error handling
    }
}

fn parse_string(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    match tokens.next() {
        Some(Token::String(text)) => Expression::String(text),
        next => panic!("Expected string, got {next:?}"), // Todo: error handling
    }
}

fn eat(tokens: &mut Peekable<IntoIter<Token>>, token: Token) {
    match tokens.next() {
        Some(tk) if tk == token => {} // Ok
        next => {
            panic!("Expected {token:?}, got {next:?}"); // Todo: error handling
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Name(String),
    Integer(i128),
    String(String),
    Binary(Token /* operator */, Box<Expression>, Box<Expression>),
    Lambda(String /* parameter */, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    VarIn {
        name: String,
        value: Box<Expression>,
        body: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },
    New(String, Vec<(String, Expression)> /* fields */),
}
