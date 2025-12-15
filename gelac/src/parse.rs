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
        context: Vec<String>,
    },
    UnexpectedWithStr {
        actual: Token,
        expected: String,
        context: Vec<String>,
    },
    UnexpectedEof {
        expected: String,
        context: Vec<String>,
    }, // Location can be restored
    EmptyStruct,
    EmptyEnum,
}

impl ParserError {
    pub fn unexpected(
        actual: Option<Token>,
        expected: Token,
        context: &mut Vec<&'static str>,
    ) -> Self {
        if let Some(actual) = actual {
            Self::Unexpected {
                actual,
                expected,
                context: context.iter().map(|m| m.to_string()).collect(),
            }
        } else {
            Self::UnexpectedEof {
                expected: expected.to_string(),
                context: context.iter().map(|m| m.to_string()).collect(),
            }
        }
    }

    pub fn unexpected_with_str(
        actual: Option<Token>,
        expected: impl ToString,
        context: &mut Vec<&'static str>,
    ) -> Self {
        if let Some(actual) = actual {
            Self::UnexpectedWithStr {
                actual,
                expected: expected.to_string(),
                context: context.iter().map(|m| m.to_string()).collect(),
            }
        } else {
            Self::UnexpectedEof {
                expected: expected.to_string(),
                context: context.iter().map(|m| m.to_string()).collect(),
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
        context: &mut Vec<&'static str>,
    ) -> Result<(), ParserError> {
        match self.next() {
            Some(actual) if actual == expected => Ok(()),
            other => Err(ParserError::unexpected(other, expected, context)),
        }
    }

    fn eat_name(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Name, ParserError> {
        match self.next() {
            Some(Token::Name(name)) => Ok(ast::Name(name)),
            other => {
                Err(ParserError::unexpected_with_str(other, "name", context))
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
        context: &mut Vec<&'static str>,
        name: &'static str,
    ) -> Result<T, ParserError> {
        context.push(name);
        let node = f(self, context)?;
        assert_eq!(context.pop(), Some(name));
        Ok(node)
    }
}

//> Statement
impl Parser {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_statement_with_context, &mut vec![], "Statement")
    }

    fn parse_statement_with_context(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Statement, ParserError> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(context),
            Some(Token::Struct) => self.parse_struct(context),
            Some(Token::Enum) => self.parse_enum(context),
            Some(Token::Import) => self.parse_import(context),
            _ => Err(ParserError::unexpected_with_str(
                self.next(),
                "statement",
                context,
            )),
        }
    }

    //> Let
    pub fn parse_let(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_let_with_context, context, "Let")
    }

    pub fn parse_let_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Let, context)?;
        let name = self.eat_name(context)?;
        self.eat(Token::Equals, context)?;
        let value = self.parse_expression(context)?;
        Ok(ast::Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_struct_with_context, context, "Struct")
    }

    pub fn parse_struct_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Struct, context)?;
        let name = self.eat_name(context)?;
        self.eat(Token::OpenCurly, context)?;
        let fields = self.parse_struct_fields(context)?;
        self.eat(Token::CloseCurly, context)?;
        Ok(ast::Statement::Struct(name, fields))
    }

    pub fn parse_struct_fields(&mut self, context: &mut Vec<&'static str>) -> Result<ast::StructFields, ParserError> {
        self.step(Self::parse_struct_fields_with_context, context, "StructFields")
    }

    pub fn parse_struct_fields_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::StructFields, ParserError> {
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(context)?;
            self.eat(Token::Colon, context)?;
            let ty = self.eat_name(context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, context)?;
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
    pub fn parse_enum(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_enum_with_context, context, "Enum")
    }

    pub fn parse_enum_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Enum, context)?;
        let name = self.eat_name(context)?;
        self.eat(Token::OpenCurly, context)?;
        let variants = self.parse_enum_variants(context)?;
        self.eat(Token::CloseCurly, context)?;
        Ok(ast::Statement::Enum(name, variants))
    }

    pub fn parse_enum_variants(&mut self, context: &mut Vec<&'static str>) -> Result<ast::EnumVariants, ParserError> {
        self.step(Self::parse_enum_variants_with_context, context, "EnumVariants")
    }

    pub fn parse_enum_variants_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::EnumVariants, ParserError> {
        let mut variants = Vec::new();
        while !self.is_close_curly() {
            let variant = self.eat_name(context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, context)?;
            }
            variants.push(variant);
        }
        let variants = ast::EnumVariants(variants);
        variants.check()?;
        Ok(variants)
    }
    //< Enum

    //> Import
    pub fn parse_import(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Statement, ParserError> {
        self.step(Self::parse_import_with_context, context, "Import")
    }

    pub fn parse_import_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Import, context)?;
        let module = self.eat_name(context)?;
        let module_alias = if self.is_as() {
            self.next();
            Some(self.eat_name(context)?)
        } else {
            None
        };
        if !self.is_open_curly() {
            return Ok(ast::Statement::Import(module, module_alias, None));
        }
        self.eat(Token::OpenCurly, context)?;
        let names_with_aliases = self.parse_names_with_aliases(context)?;
        self.eat(Token::CloseCurly, context)?;
        Ok(ast::Statement::Import(
            module,
            module_alias,
            Some(names_with_aliases),
        ))
    }

    pub fn parse_names_with_aliases(&mut self, context: &mut Vec<&'static str>) -> Result<ast::NamesWithAliases, ParserError> {
        self.step(Self::parse_names_with_aliases_with_context, context, "NamesWithAliases")
    }

    pub fn parse_names_with_aliases_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::NamesWithAliases, ParserError> {
        let mut names_with_aliases = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(context)?;
            let alias: Option<ast::Name> = if self.is_as() {
                self.eat(Token::As, context)?;
                Some(self.eat_name(context)?)
            } else {
                None
            };
            if !self.is_close_curly() {
                self.eat(Token::Comma, context)?;
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
    pub fn parse_expression(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_expression_with_context, context, "Expression")
    }

    pub fn parse_expression_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        match self.peek() {
            Some(Token::Name(_))
                if matches!(self.nth(1), Some(Token::Arrow)) =>
            {
                self.parse_abstraction(context)
            }
            Some(Token::Var) => self.parse_bind(context),
            Some(Token::New) => self.parse_new(context),
            Some(_) => self.parse_type_cast(context),
            _ => Err(ParserError::unexpected_with_str(
                self.next(),
                "expression",
                context,
            )),
        }
    }

    //> Abstraction
    pub fn parse_abstraction(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_abstraction_with_context, context, "Abstraction")
    }

    pub fn parse_abstraction_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let parameter = self.eat_name(context)?;
        self.eat(Token::Arrow, context)?;
        let body = self.parse_expression(context)?;
        Ok(ast::Expression::Abstraction(parameter, Box::new(body)))
    }

    //> Bind
    pub fn parse_bind(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_bind_with_context, context, "Bind")
    }

    pub fn parse_bind_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.eat(Token::Var, context)?;
        let name = self.eat_name(context)?;
        self.eat(Token::Equals, context)?;
        let value = self.parse_expression(context)?;
        self.eat(Token::In, context)?;
        let body = self.parse_expression(context)?;
        let variable = ast::BindVariable(name, Box::new(value));
        Ok(ast::Expression::Bind(variable, Box::new(body)))
    }
    //< Bind

    //> When
    //< When

    //> If
    //< If

    //> New
    pub fn parse_new(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_new_with_context, context, "New")
    }

    pub fn parse_new_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        self.eat(Token::New, context)?;
        let name = self.eat_name(context)?;
        self.eat(Token::OpenCurly, context)?;
        let fields = self.parse_new_fields(context)?;
        self.eat(Token::CloseCurly, context)?;
        Ok(ast::Expression::New(name, fields))
    }

    pub fn parse_new_fields(&mut self, context: &mut Vec<&'static str>) -> Result<ast::NewFields, ParserError> {
        self.step(Self::parse_new_fields_with_context, context, "NewFields")
    }

    pub fn parse_new_fields_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::NewFields, ParserError> {
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(context)?;
            self.eat(Token::Colon, context)?;
            let value = self.parse_expression(context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, context)?;
            }
            let field = (name, value);
            fields.push(field);
        }
        Ok(ast::NewFields(fields))
    }
    //< New

    //> TypeCast
    pub fn parse_type_cast(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_type_cast_with_context, context, "TypeCast")
    }

    pub fn parse_type_cast_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut value = self.parse_infix_lowest(context)?;
        while self.is_colon_colon() {
            self.next();
            let ty = self.eat_name(context)?;
            value = ast::Expression::TypeCast(Box::new(value), ty);
        }
        Ok(value)
    }
    //< TypeCast

    //> InfixLowest
    pub fn parse_infix_lowest(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_lowest_with_context, context, "InfixLowest")
    }

    pub fn parse_infix_lowest_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_lower(context)?;
        if self.is_infix_lowest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest(context)?;
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
    pub fn parse_infix_lower(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_lower_with_context, context, "InfixLower")
    }

    pub fn parse_infix_lower_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_low(context)?;
        if self.is_infix_lower_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest(context)?;
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
    pub fn parse_infix_low(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_low_with_context, context, "InfixLow")
    }
    pub fn parse_infix_low_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_high(context)?;
        if self.is_infix_low_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_low(context)?;
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
    pub fn parse_infix_high(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_high_with_context, context, "InfixHigh")
    }
    pub fn parse_infix_high_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_higher(context)?;
        if self.is_infix_high_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_high(context)?;
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
    pub fn parse_infix_higher(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_higher_with_context, context, "InfixHigher")
    }

    pub fn parse_infix_higher_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_infix_highest(context)?;
        if self.is_infix_higher_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_higher(context)?;
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
    pub fn parse_infix_highest(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_infix_highest_with_context, context, "InfixHighest")
    }

    pub fn parse_infix_highest_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let mut lhs = self.parse_application(context)?;
        if self.is_infix_highest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_highest(context)?;
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
    pub fn parse_application(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Expression, ParserError> {
        self.step(Self::parse_application_with_context, context, "Application")
    }

    pub fn parse_application_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Expression, ParserError> {
        let f = self.parse_atom(context)?;
        let mut arguments = Vec::new();
        while self.is_atom() {
            arguments.push(self.parse_atom(context)?);
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
    pub fn parse_atom(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Atom, ParserError> {
        self.step(Self::parse_atom_with_context, context, "Atom")
    }

    pub fn parse_atom_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        match self.next() {
            Some(Token::Name(identifier)) => {
                Ok(ast::Atom::Variable(ast::Name(identifier)))
            }
            Some(Token::Integer(value)) => Ok(ast::Atom::Integer(value)),
            Some(Token::String(text)) => Ok(ast::Atom::String(text)),
            Some(Token::OpenRound) => self.parse_parenthesized(context),
            actual => Err(ParserError::unexpected_with_str(
                actual,
                format!("name, integer, string or {}", Token::OpenRound),
                context,
            )),
        }
    }

    //> Parenthesized
    pub fn parse_parenthesized(&mut self, context: &mut Vec<&'static str>) -> Result<ast::Atom, ParserError> {
        self.step(Self::parse_parenthesized_with_context, context, "Parenthesized")
    }

    pub fn parse_parenthesized_with_context(
        &mut self,
        context: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        // "(" was consumed
        let inner = self.parse_expression(context)?;
        self.eat(Token::CloseRound, context)?;
        Ok(ast::Atom::Parenthesized(Box::new(inner)))
    }
    //< Parenthesized
    //< Atom
}
//< Expression
