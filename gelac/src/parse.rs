use crate::ast;
use crate::lex::Token;

pub fn parse(tokens: Vec<Token>) -> Vec<Result<ast::Statement, ParserError>> {
    let mut parser = Parser::new(tokens);
    std::iter::from_fn(move || {
        (parser.peek().is_some()).then(|| parser.parse_statement())
    })
    .collect()
}

#[derive(Debug)]
pub enum ParserError {
    Unexpected {
        actual: Token,
        expected: Token,
        trace: Vec<String>,
    },
    UnexpectedWithStr {
        actual: Token,
        expected: String,
        trace: Vec<String>,
    },
    UnexpectedEof {
        expected: String,
        trace: Vec<String>,
    }, // Location can be restored
    EmptyStruct,
    EmptyEnum,
}

impl ParserError {
    pub fn unexpected(
        actual: Option<Token>,
        expected: Token,
        trace: &mut Vec<&'static str>,
    ) -> Self {
        if let Some(actual) = actual {
            Self::Unexpected {
                actual,
                expected,
                trace: trace.iter().map(|m| m.to_string()).collect(),
            }
        } else {
            Self::UnexpectedEof {
                expected: expected.to_string(),
                trace: trace.iter().map(|m| m.to_string()).collect(),
            }
        }
    }

    pub fn unexpected_with_str(
        actual: Option<Token>,
        expected: impl ToString,
        trace: &mut Vec<&'static str>,
    ) -> Self {
        if let Some(actual) = actual {
            Self::UnexpectedWithStr {
                actual,
                expected: expected.to_string(),
                trace: trace.iter().map(|m| m.to_string()).collect(),
            }
        } else {
            Self::UnexpectedEof {
                expected: expected.to_string(),
                trace: trace.iter().map(|m| m.to_string()).collect(),
            }
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

    fn eat(
        &mut self,
        expected: Token,
        trace: &mut Vec<&'static str>,
    ) -> Result<(), ParserError> {
        match self.next() {
            Some(actual) if actual == expected => Ok(()),
            other => Err(ParserError::unexpected(other, expected, trace)),
        }
    }

    fn eat_name(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Name, ParserError> {
        match self.next() {
            Some(Token::Name(name)) => Ok(ast::Name(name)),
            other => {
                Err(ParserError::unexpected_with_str(other, "name", trace))
            }
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

    fn is_colon_colon(&mut self) -> bool {
        matches!(self.peek(), Some(Token::ColonColon))
    }

    fn step<T>(
        &mut self,
        f: impl FnOnce(&mut Self, &mut Vec<&'static str>) -> Result<T, ParserError>,
        trace: &mut Vec<&'static str>,
        name: &'static str,
    ) -> Result<T, ParserError> {
        trace.push(name);
        let node = f(self, trace)?;
        assert_eq!(trace.pop(), Some(name));
        Ok(node)
    }
}

//> Statement
impl Parser {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_statement_wo_mark, &mut vec![], "Statement")
    }

    fn parse_statement_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(trace),
            Some(Token::Struct) => self.parse_struct(trace),
            Some(Token::Enum) => self.parse_enum(trace),
            Some(Token::Import) => self.parse_import(trace),
            _ => Err(ParserError::unexpected_with_str(
                self.next(),
                "statement",
                trace,
            )),
        }
    }

    //> Let
    pub fn parse_let(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_let_wo_mark, trace, "Let")
    }

    pub fn parse_let_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Let, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::Equals, trace)?;
        let value = self.parse_expression(trace)?;
        Ok(ast::Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_struct_wo_mark, trace, "Struct")
    }

    pub fn parse_struct_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Struct, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::OpenCurly, trace)?;
        let fields = self.parse_struct_fields(trace)?;
        self.eat(Token::CloseCurly, trace)?;
        Ok(ast::Statement::Struct(name, fields))
    }

    pub fn parse_struct_fields(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::StructFields, ParserError> {
        self.step(Self::parse_struct_fields_wo_mark, trace, "StructFields")
    }

    pub fn parse_struct_fields_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::StructFields, ParserError> {
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(trace)?;
            self.eat(Token::Colon, trace)?;
            let ty = self.eat_name(trace)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, trace)?;
            }
            let field = (name, ty);
            fields.push(field);
        }
        let fields = ast::StructFields(fields);
        fields.check()?;
        Ok(fields)
    }
    //< Struct

    //> Enum
    pub fn parse_enum(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_enum_wo_mark, trace, "Enum")
    }

    pub fn parse_enum_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Enum, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::OpenCurly, trace)?;
        let variants = self.parse_enum_variants(trace)?;
        self.eat(Token::CloseCurly, trace)?;
        Ok(ast::Statement::Enum(name, variants))
    }

    pub fn parse_enum_variants(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::EnumVariants, ParserError> {
        self.step(Self::parse_enum_variants_wo_mark, trace, "EnumVariants")
    }

    pub fn parse_enum_variants_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::EnumVariants, ParserError> {
        let mut variants = Vec::new();
        while !self.is_close_curly() {
            let variant = self.eat_name(trace)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, trace)?;
            }
            variants.push(variant);
        }
        let variants = ast::EnumVariants(variants);
        variants.check()?;
        Ok(variants)
    }
    //< Enum

    //> Import
    pub fn parse_import(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_import_wo_mark, trace, "Import")
    }

    pub fn parse_import_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Import, trace)?;
        let module = self.eat_name(trace)?;
        let module_alias = if self.is_as() {
            self.next();
            Some(self.eat_name(trace)?)
        } else {
            None
        };
        if !self.is_open_curly() {
            return Ok(ast::Statement::Import(module, module_alias, None));
        }
        self.eat(Token::OpenCurly, trace)?;
        let names_with_aliases = self.parse_names_with_aliases(trace)?;
        self.eat(Token::CloseCurly, trace)?;
        Ok(ast::Statement::Import(
            module,
            module_alias,
            Some(names_with_aliases),
        ))
    }

    pub fn parse_names_with_aliases(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::NamesWithAliases, ParserError> {
        self.step(
            Self::parse_names_with_aliases_wo_mark,
            trace,
            "NamesWithAliases",
        )
    }

    pub fn parse_names_with_aliases_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::NamesWithAliases, ParserError> {
        let mut names_with_aliases = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(trace)?;
            let alias: Option<ast::Name> = if self.is_as() {
                self.eat(Token::As, trace)?;
                Some(self.eat_name(trace)?)
            } else {
                None
            };
            if !self.is_close_curly() {
                self.eat(Token::Comma, trace)?;
            }
            let name_with_alias = (name, alias);
            names_with_aliases.push(name_with_alias);
        }
        Ok(ast::NamesWithAliases(names_with_aliases))
    }
    //< Import
}
//< Statement

//> Expression
impl Parser {
    pub fn parse_expression(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_expression_wo_mark, trace, "Expression")
    }

    pub fn parse_expression_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        match self.peek() {
            Some(Token::Name(_))
                if matches!(self.nth(1), Some(Token::Arrow)) =>
            {
                self.parse_abstraction(trace)
            }
            Some(Token::Var) => self.parse_bind(trace),
            Some(Token::New) => self.parse_new(trace),
            Some(_) => self.parse_type_cast(trace),
            _ => Err(ParserError::unexpected_with_str(
                self.next(),
                "expression",
                trace,
            )),
        }
    }

    //> Abstraction
    pub fn parse_abstraction(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_abstraction_wo_mark, trace, "Abstraction")
    }

    pub fn parse_abstraction_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let parameter = self.eat_name(trace)?;
        self.eat(Token::Arrow, trace)?;
        let body = self.parse_expression(trace)?;
        Ok(ast::Expression::Abstraction(parameter, Box::new(body)))
    }

    //> Bind
    pub fn parse_bind(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_bind_wo_mark, trace, "Bind")
    }

    pub fn parse_bind_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.eat(Token::Var, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::Equals, trace)?;
        let value = self.parse_expression(trace)?;
        self.eat(Token::In, trace)?;
        let body = self.parse_expression(trace)?;
        let variable = ast::BindVariable(name, Box::new(value));
        Ok(ast::Expression::Bind(variable, Box::new(body)))
    }
    //< Bind

    //> When
    //< When

    //> If
    //< If

    //> New
    pub fn parse_new(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_new_wo_mark, trace, "New")
    }

    pub fn parse_new_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.eat(Token::New, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::OpenCurly, trace)?;
        let fields = self.parse_new_fields(trace)?;
        self.eat(Token::CloseCurly, trace)?;
        Ok(ast::Expression::New(name, fields))
    }

    pub fn parse_new_fields(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::NewFields, ParserError> {
        self.step(Self::parse_new_fields_wo_mark, trace, "NewFields")
    }

    pub fn parse_new_fields_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::NewFields, ParserError> {
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(trace)?;
            self.eat(Token::Colon, trace)?;
            let value = self.parse_expression(trace)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, trace)?;
            }
            let field = (name, value);
            fields.push(field);
        }
        Ok(ast::NewFields(fields))
    }
    //< New

    //> TypeCast
    pub fn parse_type_cast(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_type_cast_wo_mark, trace, "TypeCast")
    }

    pub fn parse_type_cast_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut value = self.parse_infix_lowest(trace)?;
        while self.is_colon_colon() {
            self.next();
            let ty = self.eat_name(trace)?;
            value = ast::Expression::TypeCast(Box::new(value), ty);
        }
        Ok(value)
    }
    //< TypeCast

    //> InfixLowest
    pub fn parse_infix_lowest(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_lowest_wo_mark, trace, "InfixLowest")
    }

    pub fn parse_infix_lowest_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_lower(trace)?;
        if self.is_infix_lowest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_lowest_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Dollar))
    }
    //< InfixLowest

    //> InfixLower
    pub fn parse_infix_lower(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_lower_wo_mark, trace, "InfixLower")
    }

    pub fn parse_infix_lower_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_low(trace)?;
        if self.is_infix_lower_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_lower_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::And | Token::Or))
    }
    //< InfixLower

    //> InfixLow
    pub fn parse_infix_low(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_low_wo_mark, trace, "InfixLow")
    }
    pub fn parse_infix_low_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_high(trace)?;
        if self.is_infix_low_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_low(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_low_operator(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::EqualEqual
                    | Token::NotEqual
                    | Token::LessEqual
                    | Token::GreaterEqual
                    | Token::Less
                    | Token::Greater
            )
        )
    }
    //< InfixLow

    //> InfixHigh
    pub fn parse_infix_high(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_high_wo_mark, trace, "InfixHigh")
    }
    pub fn parse_infix_high_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_higher(trace)?;
        if self.is_infix_high_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_high(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_high_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Plus | Token::Minus))
    }
    //< InfixHigh

    //> InfixHigher
    pub fn parse_infix_higher(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_higher_wo_mark, trace, "InfixHigher")
    }

    pub fn parse_infix_higher_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_highest(trace)?;
        if self.is_infix_higher_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_higher(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_higher_operator(&self) -> bool {
        matches!(
            self.peek(),
            Some(Token::Asterisk | Token::Slash | Token::Percent)
        )
    }
    //< InfixHigher

    //> InfixHighest
    pub fn parse_infix_highest(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_highest_wo_mark, trace, "InfixHighest")
    }

    pub fn parse_infix_highest_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_application(trace)?;
        if self.is_infix_highest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_highest(trace)?;
            lhs =
                ast::Expression::Binary(operator, Box::new(lhs), Box::new(rhs));
        }
        Ok(lhs)
    }

    fn is_infix_highest_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Caret))
    }
    //< InfixHighest

    //> Application
    pub fn parse_application(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_application_wo_mark, trace, "Application")
    }

    pub fn parse_application_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let f = self.parse_atom(trace)?;
        let mut arguments = Vec::new();
        while self.is_atom() {
            arguments.push(self.parse_atom(trace)?);
        }
        if arguments.is_empty() {
            return Ok(ast::Expression::Atom(f));
        }
        Ok(ast::Expression::Application(f, arguments))
    }

    fn is_atom(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Name(_)
                    | Token::Integer(_)
                    | Token::String(_)
                    | Token::OpenRound
            )
        )
    }
    //< Application

    //> Atom
    pub fn parse_atom(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        self.step(Self::parse_atom_wo_mark, trace, "Atom")
    }

    pub fn parse_atom_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        match self.next() {
            Some(Token::Name(identifier)) => {
                Ok(ast::Atom::Variable(ast::Name(identifier)))
            }
            Some(Token::Integer(value)) => Ok(ast::Atom::Integer(value)),
            Some(Token::String(text)) => Ok(ast::Atom::String(text)),
            Some(Token::OpenRound) => self.parse_parenthesized(trace),
            actual => Err(ParserError::unexpected_with_str(
                actual,
                format!("variable, integer, string or {}", Token::OpenRound),
                trace,
            )),
        }
    }

    //> Parenthesized
    pub fn parse_parenthesized(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        self.step(Self::parse_parenthesized_wo_mark, trace, "Parenthesized")
    }

    pub fn parse_parenthesized_wo_mark(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        // "(" was consumed
        let inner = self.parse_expression(trace)?;
        self.eat(Token::CloseRound, trace)?;
        Ok(ast::Atom::Parenthesized(Box::new(inner)))
    }
    //< Parenthesized
    //< Atom
}
//< Expression
