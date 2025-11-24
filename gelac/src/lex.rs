use std::{iter::Peekable, str::Chars};

pub fn lex(source: String) -> Vec<Token> {
    let mut chars = source.chars().peekable();
    std::iter::from_fn(move || token(&mut chars)).collect()
}

fn token(chars: &mut Peekable<Chars>) -> Option<Token> {
    match this_and_next(chars)? {
        ('a'..='z' | 'A'..='Z' | '_', _) => {
            let lexeme =
                eat(chars, |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_'));
            match lexeme.as_str() {
                "let" => Some(Token::Let),
                _ => Some(Token::Name(lexeme)), // If it's not a keyword, then it's a name
            }
        }
        ('0'..='9', _) => {
            let lexeme = eat(chars, |ch| matches!(ch, '0'..='9'));
            let value: i128 = lexeme.parse().unwrap(); // Todo: error handling
            Some(Token::Integer(value))
        }
        ('"', _) => {
            chars.next(); // Skip the leading quote
            let lexeme = eat(chars, |ch| *ch != '"');
            if chars.peek().is_none() {
                panic!("Unterminated string"); // Todo: error handling
            }
            chars.next(); // Skip the trailing quote
            Some(Token::String(lexeme))
        }
        ('#', _) => {
            eat(chars, |ch| *ch != '\n'); // Skip until the next line
            token(chars)
        }
        ('+', _) => {
            chars.next();
            Some(Token::Plus)
        }
        ('=', _) => {
            chars.next();
            Some(Token::Equals)
        }
        ('-', Some('>')) => {
            chars.next();
            chars.next();
            Some(Token::Arrow)
        }
        (' ' | '\t' | '\n' | '\r', _) => {
            eat(chars, |ch| matches!(ch, ' ' | '\t' | '\n' | '\r')); // Just skip whitespace
            token(chars) // And lex an actual token
        }
        (ch, _) => {
            panic!("Unknown character: {ch:?}"); // Todo: error handling
        }
    }
}

fn this_and_next(chars: &Peekable<Chars>) -> Option<(char, Option<char>)> {
    let mut chars = chars.clone(); // Cheap: trust me
    let this = chars.next()?;
    let next = chars.next();
    Some((this, next))
}

fn eat(chars: &mut Peekable<Chars>, condition: fn(&char) -> bool) -> String {
    let mut buffer = String::new();
    while chars.peek().is_some_and(condition) {
        buffer.push(chars.next().unwrap()); // Checked: peeked char is some
    }
    buffer
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Integer(i128),
    String(String),
    Let,
    Plus,
    Equals,
    Arrow,
}
