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
        context: String,
    },
    UnexpectedWithMessage {
        actual: Token,
        message: String,
        context: String,
    },
    UnexpectedEof {
        expected: String,
        context: String,
    }, // Location can be restored
    EmptyStruct,
    EmptyEnum,
}

impl ParserError {
    pub fn unexpected(
        actual: Option<Token>,
        expected: Token,
        context: impl ToString,
    ) -> Self {
        if let Some(actual) = actual {
            Self::Unexpected {
                actual,
                expected,
                context: context.to_string(),
            }
        } else {
            Self::UnexpectedEof {
                expected: expected.to_string(),
                context: context.to_string(),
            }
        }
    }

    pub fn unexpected_with_message(
        actual: Option<Token>,
        message: impl ToString,
        context: impl ToString,
    ) -> Self {
        if let Some(actual) = actual {
            Self::UnexpectedWithMessage {
                actual,
                message: message.to_string(),
                context: context.to_string(),
            }
        } else {
            Self::UnexpectedEof {
                expected: message.to_string(),
                context: context.to_string(),
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
        context: impl ToString,
    ) -> Result<(), ParserError> {
        match self.next() {
            Some(actual) if actual == expected => Ok(()),
            other => Err(ParserError::unexpected(other, expected, context)),
        }
    }

    fn eat_name(
        &mut self,
        context: impl ToString,
    ) -> Result<ast::Name, ParserError> {
        match self.next() {
            Some(Token::Name(name)) => Ok(ast::Name(name)),
            other => Err(ParserError::unexpected_with_message(
                other, "name", context,
            )),
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

//> Statement
impl Parser {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match self.peek() {
            Some(Token::Let) => self.parse_let(),
            Some(Token::Struct) => self.parse_struct(),
            Some(Token::Enum) => self.parse_enum(),
            Some(Token::Import) => self.parse_import(),
            _ => Err(ParserError::unexpected_with_message(
                self.next(),
                "statement",
                "global scope",
            )),
        }
    }

    //> Let
    pub fn parse_let(&mut self) -> Result<ast::Statement, ParserError> {
        let context = "let statement";
        self.eat(Token::Let, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::Equals, &context)?;
        let value = self.parse_expression()?;
        Ok(ast::Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(&mut self) -> Result<ast::Statement, ParserError> {
        let context = "struct definition";
        self.eat(Token::Struct, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let fields = self.parse_struct_fields()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(ast::Statement::Struct(name, fields))
    }

    pub fn parse_struct_fields(
        &mut self,
    ) -> Result<ast::StructFields, ParserError> {
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
        let fields = ast::StructFields(fields);
        fields.check()?;
        Ok(fields)
    }
    //< Struct

    //> Enum
    pub fn parse_enum(&mut self) -> Result<ast::Statement, ParserError> {
        let context = "enum definition";
        self.eat(Token::Enum, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let variants = self.parse_enum_variants()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(ast::Statement::Enum(name, variants))
    }

    pub fn parse_enum_variants(
        &mut self,
    ) -> Result<ast::EnumVariants, ParserError> {
        let context = "enum variants";
        let mut variants = Vec::new();
        while !self.is_close_curly() {
            let variant = self.eat_name(&context)?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, &context)?;
            }
            variants.push(variant);
        }
        let variants = ast::EnumVariants(variants);
        variants.check()?;
        Ok(variants)
    }
    //< Enum

    //> Import
    pub fn parse_import(&mut self) -> Result<ast::Statement, ParserError> {
        let context = "import statement";
        self.eat(Token::Import, &context)?;
        let module = self.eat_name(&context)?;
        let module_alias = if self.is_as() {
            self.next();
            Some(self.eat_name(&context)?)
        } else {
            None
        };
        if !self.is_open_curly() {
            return Ok(ast::Statement::Import(module, module_alias, None));
        }
        self.eat(Token::OpenCurly, &context)?;
        let names_with_aliases = self.parse_names_with_aliases()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(ast::Statement::Import(
            module,
            module_alias,
            Some(names_with_aliases),
        ))
    }

    pub fn parse_names_with_aliases(
        &mut self,
    ) -> Result<ast::NamesWithAliases, ParserError> {
        let context = "imported names with aliases";
        let mut names_with_aliases = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(&context)?;
            let alias: Option<ast::Name> = if self.is_as() {
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
        Ok(ast::NamesWithAliases(names_with_aliases))
    }
    //< Import
}
//< Statement

//> Expression
impl Parser {
    pub fn parse_expression(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "statement body";
        match self.peek() {
            Some(Token::Name(_))
                if matches!(self.nth(1), Some(Token::Arrow)) =>
            {
                self.parse_abstraction()
            }
            Some(Token::Var) => self.parse_bind(),
            Some(Token::New) => self.parse_new(),
            Some(_) => self.parse_infix_lowest(),
            _ => Err(ParserError::unexpected_with_message(
                self.next(),
                "expression",
                &context,
            )),
        }
    }

    //> Abstraction
    pub fn parse_abstraction(
        &mut self,
    ) -> Result<ast::Expression, ParserError> {
        let context = "abstraction";
        let parameter = self.eat_name(&context)?;
        self.eat(Token::Arrow, &context)?;
        let body = self.parse_expression()?;
        Ok(ast::Expression::Abstraction(parameter, Box::new(body)))
    }

    //> Bind
    pub fn parse_bind(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "var-in expression";
        self.eat(Token::Var, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::Equals, &context)?;
        let value = self.parse_expression()?;
        self.eat(Token::In, &context)?;
        let body = self.parse_expression()?;
        let variable = ast::BindVariable(name, Box::new(value));
        Ok(ast::Expression::Bind(variable, Box::new(body)))
    }
    //< Bind

    //> When
    //< When

    //> If
    //< If

    //> New
    pub fn parse_new(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "struct initialization";
        self.eat(Token::New, &context)?;
        let name = self.eat_name(&context)?;
        self.eat(Token::OpenCurly, &context)?;
        let fields = self.parse_new_fields()?;
        self.eat(Token::CloseCurly, &context)?;
        Ok(ast::Expression::New(name, fields))
    }

    pub fn parse_new_fields(&mut self) -> Result<ast::NewFields, ParserError> {
        let context = "struct initialization";
        let mut fields = Vec::new();
        while !self.is_close_curly() {
            let name = self.eat_name(&context)?;
            self.eat(Token::Colon, &context)?;
            let value = self.parse_expression()?;
            if !self.is_close_curly() {
                self.eat(Token::Comma, &context)?;
            }
            let field = (name, value);
            fields.push(field);
        }
        Ok(ast::NewFields(fields))
    }
    //< New

    //> TypeCast
    pub fn parse_type_cast(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "type cast";
        let mut value = self.parse_infix_lowest()?;
        if let Some(Token::ColonColon) = self.peek() {
            self.next();
            let ty = self.eat_name(&context)?;
            value = ast::Expression::TypeCast(Box::new(value), ty);
        }
        Ok(value)
    }
    //< TypeCast

    //> InfixLowest
    pub fn parse_infix_lowest(
        &mut self,
    ) -> Result<ast::Expression, ParserError> {
        let context = "infix lowest";
        let mut lhs = self.parse_infix_lower()?;
        if self.is_infix_lowest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest()?;
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
    ) -> Result<ast::Expression, ParserError> {
        let context = "infix lower";
        let mut lhs = self.parse_infix_low()?;
        if self.is_infix_lower_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_lowest()?;
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
    pub fn parse_infix_low(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "infix low";
        let mut lhs = self.parse_infix_high()?;
        if self.is_infix_low_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_low()?;
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
    pub fn parse_infix_high(&mut self) -> Result<ast::Expression, ParserError> {
        let context = "infix high";
        let mut lhs = self.parse_infix_higher()?;
        if self.is_infix_high_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_high()?;
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
    ) -> Result<ast::Expression, ParserError> {
        let context = "infix higher";
        let mut lhs = self.parse_infix_highest()?;
        if self.is_infix_higher_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_higher()?;
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
    ) -> Result<ast::Expression, ParserError> {
        let context = "infix highest";
        let mut lhs = self.parse_application()?;
        if self.is_infix_highest_operator() {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = self.parse_infix_highest()?;
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
    ) -> Result<ast::Expression, ParserError> {
        let f = self.parse_atom()?;
        let mut arguments = Vec::new();
        while self.is_atom() {
            arguments.push(self.parse_atom()?);
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
    pub fn parse_atom(&mut self) -> Result<ast::Atom, ParserError> {
        let context = "expression";
        match self.next() {
            Some(Token::Name(identifier)) => {
                Ok(ast::Atom::Variable(ast::Name(identifier)))
            }
            Some(Token::Integer(value)) => Ok(ast::Atom::Integer(value)),
            Some(Token::String(text)) => Ok(ast::Atom::String(text)),
            Some(Token::OpenRound) => self.parse_parenthesized(),
            actual => Err(ParserError::unexpected_with_message(
                actual,
                format!("name, integer, string or {}", Token::OpenRound),
                &context,
            )),
        }
    }

    pub fn parse_parenthesized(&mut self) -> Result<ast::Atom, ParserError> {
        // "(" was consumed
        let context = "parenthesized";
        let inner = self.parse_expression()?;
        self.eat(Token::CloseRound, &context)?;
        Ok(ast::Atom::Parenthesized(Box::new(inner)))
    }
    //< Atom
}
//< Expression
