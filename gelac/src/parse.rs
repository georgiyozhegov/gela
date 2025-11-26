use std::{iter::Peekable, vec::IntoIter};

use crate::lex::Token;

pub fn parse(tokens: Vec<Token>) -> Vec<Declaration> {
    let mut tokens = tokens.into_iter().peekable();
    std::iter::from_fn(move || {
        (tokens.peek().is_some()).then(|| parse_declaration(&mut tokens))
    })
    .collect()
}

fn parse_declaration(tokens: &mut Peekable<IntoIter<Token>>) -> Declaration {
    eat(tokens, Token::Let);
    let name = eat_name(tokens);
    eat(tokens, Token::Equals);
    let value = parse_expression(tokens);
    Declaration { name, value }
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
        let precedence = this_precedence(token);
        // If precedence equals to 0, then the token isn't an operator
        if precedence == 0 || precedence < min_precedence {
            break;
        }
        // Peeked token is an operator, because its precedence > 0
        let operator = tokens.next().unwrap(); // Checked: peeked token is some
        let right =
            parse_binary(tokens, next_precedence(&operator, precedence));
        left = Expression::Binary(operator, Box::new(left), Box::new(right));
    }
    left
}

fn this_precedence(token: &Token) -> u8 {
    match token {
        Token::Asterisk | Token::Slash => 3,
        Token::Plus | Token::Minus => 2,
        Token::Arrow => 1,
        _ => 0, // Not a binary operator
    }
}

fn next_precedence(token: &Token, this: u8) -> u8 {
    match token {
        Token::Plus => this + 1,
        Token::Arrow => this, // Parse right-hand side first
        _ => this,
    }
}

fn parse_application(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut left = parse_atom(tokens);
    while let Some(token) = tokens.peek() {
        match token {
            Token::Name(_) | Token::Integer(_) | Token::String(_) => {
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
        tk => panic!("Expected atom, got: {tk:?}"),
    }
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
pub struct Declaration {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Name(String),
    Integer(i128),
    String(String),
    Binary(Token /* operator */, Box<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
}
