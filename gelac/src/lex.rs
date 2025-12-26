use std::{iter::Peekable, str::Chars};

pub fn lex(source: String) -> Result<Vec<Token>, LexerError> {
    let mut chars = source.chars().peekable();
    std::iter::from_fn(move || token(&mut chars)).collect()
}

#[derive(Debug)]
pub enum LexerError {
    UnknownChar(char),
    UnterminatedString,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnknownChar(ch) => write!(f, "Unknown character: `{ch}`"),
            Self::UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}

macro_rules! one_ch {
    ($chars:expr, $token:expr) => {{
        $chars.next();
        Some(Ok($token))
    }};
}

macro_rules! two_ch {
    ($chars:expr, $token:expr) => {{
        $chars.next();
        $chars.next();
        Some(Ok($token))
    }};
}

fn token(chars: &mut Peekable<Chars>) -> Option<Result<Token, LexerError>> {
    match this_and_next(chars)? {
        ('a'..='z' | 'A'..='Z' | '_', _) => {
            let mut lexeme = eat(
                chars,
                |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'),
            );
            // Peek suffix: either "?" or "!"
            if chars.peek().is_some_and(|ch| matches!(ch, '?' | '!')) {
                lexeme.push(chars.next().unwrap());
            }
            Some(Ok(name_or_keyword(lexeme)))
        }
        ('0'..='9', _) => {
            let lexeme = eat(chars, |ch| matches!(ch, '0'..='9' | '_'));
            let value: i128 = lexeme.parse().unwrap(); // Todo: error handling
            Some(Ok(Token::Integer(value)))
        }
        ('"', _) => {
            chars.next(); // Skip the leading quote
            let lexeme = eat(chars, |ch| *ch != '"');
            if chars.peek().is_none() {
                return Some(Err(LexerError::UnterminatedString));
            }
            chars.next(); // Skip the trailing quote
            Some(Ok(Token::String(lexeme)))
        }
        ('#', Some('>')) => {
            while chars.peek().is_some() {
                eat(chars, |ch| *ch != '<');
                chars.next();
                if matches!(chars.peek(), Some('#')) {
                    chars.next();
                    break;
                }
            }
            token(chars)
        }
        ('#', _) => {
            eat(chars, |ch| *ch != '\n'); // Skip until the next line
            token(chars)
        }
        ('=', Some('=')) => two_ch!(chars, Token::EqualEqual),
        ('=', _) => one_ch!(chars, Token::Equal),
        ('{', _) => one_ch!(chars, Token::OpenCurly),
        ('}', _) => one_ch!(chars, Token::CloseCurly),
        ('(', _) => one_ch!(chars, Token::OpenRound),
        (')', _) => one_ch!(chars, Token::CloseRound),
        (',', _) => one_ch!(chars, Token::Comma),
        ('-', Some('>')) => two_ch!(chars, Token::Arrow),
        ('|', _) => one_ch!(chars, Token::Pipe),
        (':', Some(':')) => two_ch!(chars, Token::ColonColon),
        (':', _) => one_ch!(chars, Token::Colon),
        ('$', _) => one_ch!(chars, Token::Dollar),
        ('!', Some('=')) => two_ch!(chars, Token::NotEqual),
        ('<', Some('=')) => two_ch!(chars, Token::LessEqual),
        ('<', _) => one_ch!(chars, Token::Less),
        ('>', Some('=')) => two_ch!(chars, Token::GreaterEqual),
        ('>', _) => one_ch!(chars, Token::Greater),
        ('+', _) => one_ch!(chars, Token::Plus),
        ('-', _) => one_ch!(chars, Token::Minus),
        ('*', _) => one_ch!(chars, Token::Asterisk),
        ('/', _) => one_ch!(chars, Token::Slash),
        ('%', _) => one_ch!(chars, Token::Percent),
        ('^', _) => one_ch!(chars, Token::Caret),
        ('.', _) => one_ch!(chars, Token::Dot),
        (' ' | '\t' | '\n' | '\r', _) => {
            eat(chars, |ch| matches!(ch, ' ' | '\t' | '\n' | '\r')); // Just skip whitespace
            token(chars) // And lex an actual token
        }
        (ch, _) => Some(Err(LexerError::UnknownChar(ch))),
    }
}

fn name_or_keyword(lexeme: String) -> Token {
    match lexeme.as_str() {
        "let" => Token::Let,
        "struct" => Token::Struct,
        "enum" => Token::Enum,
        "import" => Token::Import,
        "as" => Token::As,
        "var" => Token::Var,
        "in" => Token::In,
        "when" => Token::When,
        "then" => Token::Then,
        "else" => Token::Else,
        "if" => Token::If,
        "new" => Token::New,
        "and" => Token::And,
        "or" => Token::Or,
        _ => {
            let kind = name_kind(&lexeme);
            Token::Name(lexeme, kind)
        } // If it's not a keyword, then it's a name
    }
}

fn name_kind(lexeme: &str) -> NameKind {
    if matches!(lexeme.chars().next(), Some('A'..='Z')) {
        NameKind::Type
    } else {
        NameKind::Variable
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
    //> Literals
    Name(String, NameKind),
    Integer(i128),
    String(String),
    //< Literals

    //> Keywords
    Let,
    Struct,
    Enum,
    Import,
    As,
    Var,
    In,
    When,
    Then,
    Else,
    If,
    New,
    And,
    Or,
    //< Keywords

    //> Operators
    Equal,
    OpenCurly,
    CloseCurly,
    OpenRound,
    CloseRound,
    Comma,
    Arrow,
    Pipe,
    Colon,
    ColonColon,
    Dollar,
    EqualEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Caret,
    Dot,
    //< Operators
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameKind {
    Variable,
    Type,
}

macro_rules! write_token {
    ($f:expr, $token:expr) => {
        write!($f, "`{}`", $token)
    };
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            //> Literals
            Self::Name(name, kind) => match kind {
                NameKind::Variable => write!(f, "name `{name}`"),
                NameKind::Type => write!(f, "type `{name}`"),
            },
            Self::Integer(value) => write!(f, "integer `{value}`"),
            Self::String(text) => write!(f, "string `{text}`"),
            //< Literals

            //> Keywords
            Self::Let => write_token!(f, "let"),
            Self::Struct => write_token!(f, "struct"),
            Self::Enum => write_token!(f, "enum"),
            Self::Import => write_token!(f, "import"),
            Self::As => write_token!(f, "as"),
            Self::Var => write_token!(f, "var"),
            Self::In => write_token!(f, "in"),
            Self::When => write_token!(f, "when"),
            Self::Then => write_token!(f, "then"),
            Self::Else => write_token!(f, "else"),
            Self::If => write_token!(f, "if"),
            Self::New => write_token!(f, "new"),
            Self::And => write_token!(f, "and"),
            Self::Or => write_token!(f, "or"),
            //< Keywords

            //> Operators
            Self::Equal => write_token!(f, "="),
            Self::OpenCurly => write_token!(f, "{{"),
            Self::CloseCurly => write_token!(f, "}}"),
            Self::OpenRound => write_token!(f, "("),
            Self::CloseRound => write_token!(f, ")"),
            Self::Comma => write_token!(f, ","),
            Self::Arrow => write_token!(f, "->"),
            Self::Pipe => write_token!(f, "|"),
            Self::Colon => write_token!(f, ":"),
            Self::ColonColon => write_token!(f, "::"),
            Self::Dollar => write_token!(f, "$"),
            Self::EqualEqual => write_token!(f, "=="),
            Self::NotEqual => write_token!(f, "!="),
            Self::LessEqual => write_token!(f, "<="),
            Self::GreaterEqual => write_token!(f, ">="),
            Self::Less => write_token!(f, "<"),
            Self::Greater => write_token!(f, ">"),
            Self::Plus => write_token!(f, "+"),
            Self::Minus => write_token!(f, "-"),
            Self::Asterisk => write_token!(f, "*"),
            Self::Slash => write_token!(f, "/"),
            Self::Percent => write_token!(f, "%"),
            Self::Caret => write_token!(f, "^"),
            Self::Dot => write_token!(f, "."),
            //< Operators
        }
    }
}
