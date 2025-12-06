use std::{iter::Peekable, vec::IntoIter};

use crate::lex::Token;

pub fn parse(tokens: Vec<Token>) -> Vec<Statement> {
    let mut tokens = tokens.into_iter().peekable();
    std::iter::from_fn(move || {
        (tokens.peek().is_some()).then(|| parse_statement(&mut tokens))
    })
    .collect()
}

fn parse_statement(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    match tokens.peek().unwrap() /* Checked in "parse" */ {
        Token::Let => parse_let(tokens),
        Token::Struct => parse_struct(tokens),
        Token::Enum => parse_enum(tokens),
        Token::Import => parse_import(tokens),
        tk => panic!("Expected statement, got {tk:?}"),
    }
}

fn parse_let(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    eat(tokens, Token::Let);
    let name = eat_name(tokens);
    eat(tokens, Token::Equals);
    let value = parse_expression(tokens);
    Statement::Let(name, value)
}

fn parse_struct(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    eat(tokens, Token::Struct);
    let name = eat_name(tokens);
    eat(tokens, Token::OpenCurly);
    let mut fields = Vec::new();
    while tokens
        .peek()
        .is_some_and(|tk| !matches!(tk, Token::CloseCurly))
    {
        let name = eat_name(tokens);
        eat(tokens, Token::Colon);
        let ty = eat_name(tokens);
        if tokens
            .peek()
            .is_none_or(|tk| !matches!(tk, Token::CloseCurly))
        {
            eat(tokens, Token::Comma);
        }
        fields.push((name, ty));
    }
    eat(tokens, Token::CloseCurly);
    Statement::Struct(name, fields)
}

fn parse_enum(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    eat(tokens, Token::Enum);
    let name = eat_name(tokens);
    eat(tokens, Token::OpenCurly);
    let mut variants = Vec::new();
    while tokens
        .peek()
        .is_some_and(|tk| !matches!(tk, Token::CloseCurly))
    {
        let name = eat_name(tokens);
        if tokens
            .peek()
            .is_none_or(|tk| !matches!(tk, Token::CloseCurly))
        {
            eat(tokens, Token::Comma);
        }
        variants.push(name);
    }
    eat(tokens, Token::CloseCurly);
    Statement::Enum(name, variants)
}

fn parse_import(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    eat(tokens, Token::Import);
    let module = eat_name(tokens);
    if !tokens
        .peek()
        .is_some_and(|tk| matches!(tk, Token::OpenCurly))
    {
        return Statement::Import(module, vec![]);
    }
    eat(tokens, Token::OpenCurly);
    let mut names_with_aliases = Vec::new();
    while tokens.peek().is_some_and(|tk| matches!(tk, Token::Name(_))) {
        let name = eat_name(tokens);
        let alias = match tokens.peek() {
            Some(Token::As) => {
                tokens.next(); // Skip "as"
                Some(eat_name(tokens))
            }
            _ => None,
        };
        if tokens
            .peek()
            .is_none_or(|tk| !matches!(tk, Token::CloseCurly))
        {
            eat(tokens, Token::Comma);
        }
        names_with_aliases.push((name, alias));
    }
    eat(tokens, Token::CloseCurly);
    Statement::Import(module, names_with_aliases)
}

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_binary(tokens, 0)
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
        tk => panic!("Expected atom, got: {tk:?}"),
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
pub enum Statement {
    Let(String, Expression),
    // Note: use hash map for reproducibility
    Struct(String, Vec<(String, String)> /* fields */),
    Enum(String, Vec<String> /* variants */),
    Import(
        String,
        Vec<(String, Option<String>)>, /* names with aliases */
    ),
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
}
