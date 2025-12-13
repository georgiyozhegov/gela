use std::{iter::Peekable, str::Chars};

pub fn lex(source: String) -> Vec<Token> {
    let mut chars = source.chars().peekable();
    std::iter::from_fn(move || token(&mut chars)).collect()
}

fn token(chars: &mut Peekable<Chars>) -> Option<Token> {
    match this_and_next(chars)? {
        ('a'..='z' | 'A'..='Z' | '_', _) => {
            let mut lexeme =
                eat(chars, |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'));
            // Peek suffix: either "?" or "!"
            if chars.peek().is_some_and(|ch| matches!(ch, '?' | '!')) {
                lexeme.push(chars.next().unwrap());
            }
            match lexeme.as_str() {
                "let" => Some(Token::Let),
                "struct" => Some(Token::Struct),
                "enum" => Some(Token::Enum),
                "import" => Some(Token::Import),
                "as" => Some(Token::As),
                "var" => Some(Token::Var),
                "in" => Some(Token::In),
                "if" => Some(Token::If),
                "else" => Some(Token::Else),
                "then" => Some(Token::Then),
                "when" => Some(Token::When),
                "new" => Some(Token::New),
                "and" => Some(Token::And),
                "or" => Some(Token::Or),
                _ => Some(Token::Name(lexeme)), // If it's not a keyword, then it's a name
            }
        }
        ('0'..='9', _) => {
            let lexeme = eat(chars, |ch| matches!(ch, '0'..='9' | '_'));
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
        ('*', _) => {
            chars.next();
            Some(Token::Asterisk)
        }
        ('/', _) => {
            chars.next();
            Some(Token::Slash)
        }
        ('=', _) => {
            chars.next();
            Some(Token::Equals)
        }
        ('$', _) => {
            chars.next();
            Some(Token::Dollar)
        }
        ('-', Some('>')) => {
            chars.next(); // "-"
            chars.next(); // ">"
            Some(Token::Arrow)
        }
        ('-', _) => {
            // Put this branch after the "->" one
            chars.next();
            Some(Token::Minus)
        }
        ('(', _) => {
            chars.next();
            Some(Token::OpenRound)
        }
        (')', _) => {
            chars.next();
            Some(Token::CloseRound)
        }
        ('{', _) => {
            chars.next();
            Some(Token::OpenCurly)
        }
        ('}', _) => {
            chars.next();
            Some(Token::CloseCurly)
        }
        (':', Some(':')) => {
            chars.next();
            Some(Token::ColonColon)
        }
        (':', _) => {
            // Put this branch after the "::" one
            chars.next();
            Some(Token::Colon)
        }
        (',', _) => {
            chars.next();
            Some(Token::Comma)
        }
        ('.', _) => {
            chars.next();
            Some(Token::Dot)
        }
        ('|', _) => {
            chars.next();
            Some(Token::Pipe)
        }
        ('^', _) => {
            chars.next();
            Some(Token::Caret)
        }
        ('%', _) => {
            chars.next();
            Some(Token::Percent)
        }
        ('<', Some('=')) => {
            chars.next(); // "<"
            chars.next(); // "="
            Some(Token::LessEqual)
        }
        ('<', _) => {
            chars.next();
            Some(Token::Less)
        }
        ('>', Some('=')) => {
            chars.next(); // ">"
            chars.next(); // "="
            Some(Token::GreaterEqual)
        }
        ('>', _) => {
            chars.next();
            Some(Token::Greater)
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Name(String),
    Integer(i128),
    String(String),
    Let,
    Struct,
    Enum,
    Import,
    As,
    Var,
    In,
    If,
    Then,
    Else,
    New,
    When,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equals,
    Dollar,
    Arrow,
    OpenRound,
    CloseRound,
    OpenCurly,
    CloseCurly,
    Colon,
    ColonColon,
    Comma,
    Dot,
    Pipe,
    Caret,
    Percent,

    EqualEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,

    And,
    Or,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Name(name) => write!(f, "name `{name}`"),
            Self::Integer(value) => write!(f, "integer `{value}`"),
            Self::String(text) => write!(f, "string `{text}`"),
            Self::Let => write!(f, "`let`"),
            Self::Struct => write!(f, "`struct`"),
            Self::Enum => write!(f, "`enum`"),
            Self::Import => write!(f, "`import`"),
            Self::As => write!(f, "`as`"),
            Self::Var => write!(f, "`var`"),
            Self::In => write!(f, "`in`"),
            Self::If => write!(f, "`if`"),
            Self::Then => write!(f, "`then`"),
            Self::Else => write!(f, "`else`"),
            Self::New => write!(f, "`new`"),
            Self::When => write!(f, "`when`"),
            Self::Plus => write!(f, "`+`"),
            Self::Minus => write!(f, "`-`"),
            Self::Asterisk => write!(f, "`*`"),
            Self::Slash => write!(f, "`/`"),
            Self::Equals => write!(f, "`=`"),
            Self::Dollar => write!(f, "`$`"),
            Self::Arrow => write!(f, "`->`"),
            Self::OpenRound => write!(f, "`(`"),
            Self::CloseRound => write!(f, "`)`"),
            Self::OpenCurly => write!(f, "`{{`"),
            Self::CloseCurly => write!(f, "`}}`"),
            Self::Colon => write!(f, "`:`"),
            Self::ColonColon => write!(f, "`::`"),
            Self::Comma => write!(f, "`,`"),
            Self::Dot => write!(f, "`.`"),
            Self::Pipe => write!(f, "`|`"),
            Self::Caret => write!(f, "`^`"),
            Self::Percent => write!(f, "`%`"),
            Self::EqualEqual => write!(f, "`==`"),
            Self::NotEqual => write!(f, "`!=`"),
            Self::LessEqual => write!(f, "`<=`"),
            Self::GreaterEqual => write!(f, "`>=`"),
            Self::Less => write!(f, "`<`"),
            Self::Greater => write!(f, "`>`"),
            Self::And => write!(f, "`and`"),
            Self::Or => write!(f, "`or`"),
        }
    }
}
