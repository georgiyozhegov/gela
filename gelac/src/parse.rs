use crate::ast;
use crate::lex::Token;

pub fn parse(
    tokens: Vec<Token>,
) -> (
    Vec<Result<ast::Statement, ParserError>>,
    ast::ExpressionArena,
) {
    let mut parser = Parser::new(tokens);
    let mut program = Vec::new();
    while parser.peek().is_some() {
        let statement = parser.parse_statement();
        program.push(statement);
    }
    (program, parser.ea)
}

fn expected_atom_message() -> String {
    format!("variable, integer, string or {}", Token::OpenRound)
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

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unexpected {
                actual,
                expected,
                trace,
            } => {
                write!(f, "Expected {expected}, got {actual}")?;
                fmt_trace(f, trace)
            }
            Self::UnexpectedWithStr {
                actual,
                expected,
                trace,
            } => {
                write!(f, "Expected {expected}, got {actual}")?;
                fmt_trace(f, trace)
            }
            Self::UnexpectedEof { expected, trace } => {
                write!(f, "Expected {expected}, got end-of-file")?;
                fmt_trace(f, trace)
            }
            Self::EmptyStruct => {
                write!(f, "Struct must have at least one field")
            }
            Self::EmptyEnum => {
                write!(f, "Enum must have at least one variant")
            }
        }
    }
}

fn fmt_trace(
    f: &mut std::fmt::Formatter,
    trace: &[String],
) -> std::fmt::Result {
    writeln!(f)?;
    write!(f, "[trace] ")?;
    for (i, step) in trace.iter().enumerate() {
        write!(f, "{step}")?;
        if i + 1 < trace.len() {
            write!(f, " > ")?;
        }
    }
    Ok(())
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
    ea: ast::ExpressionArena,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            ea: ast::ExpressionArena::new(),
        }
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
            Some(Token::Name(name, kind)) => Ok(ast::Name(name, kind)),
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

    fn is_pipe(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Pipe))
    }

    fn is_dot(&mut self) -> bool {
        matches!(self.peek(), Some(Token::Dot))
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
        self.step(Self::_parse_statement, &mut vec![], "Statement")
    }

    fn _parse_statement(
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
        self.step(Self::_parse_let, trace, "Let")
    }

    pub fn _parse_let(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.eat(Token::Let, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::Equal, trace)?;
        let value = self.parse_expression(trace)?;
        Ok(ast::Statement::Let(name, value))
    }
    //< Let

    //> Struct
    pub fn parse_struct(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Statement, ParserError> {
        self.step(Self::_parse_struct, trace, "Struct")
    }

    pub fn _parse_struct(
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
        self.step(Self::_parse_struct_fields, trace, "StructFields")
    }

    pub fn _parse_struct_fields(
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
        self.step(Self::_parse_enum, trace, "Enum")
    }

    pub fn _parse_enum(
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
        self.step(Self::_parse_enum_variants, trace, "EnumVariants")
    }

    pub fn _parse_enum_variants(
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
        self.step(Self::_parse_import, trace, "Import")
    }

    pub fn _parse_import(
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
            Self::_parse_names_with_aliases,
            trace,
            "NamesWithAliases",
        )
    }

    pub fn _parse_names_with_aliases(
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
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_expression, trace, "Expression")
    }

    pub fn _parse_expression(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        match self.peek() {
            Some(Token::Name(_, _))
                if matches!(self.nth(1), Some(Token::Arrow)) =>
            {
                self.parse_abstraction(trace)
            }
            Some(Token::Var) => self.parse_bind(trace),
            Some(Token::When) => self.parse_when(trace),
            Some(Token::If) => self.parse_if(trace),
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
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_abstraction, trace, "Abstraction")
    }

    pub fn _parse_abstraction(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        let parameter = self.eat_name(trace)?;
        self.eat(Token::Arrow, trace)?;
        let body = self.parse_expression(trace)?;
        Ok(self
            .ea
            .allocate(ast::Expression::Abstraction(parameter, body)))
    }

    //> Bind
    pub fn parse_bind(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_bind, trace, "Bind")
    }

    pub fn _parse_bind(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.eat(Token::Var, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::Equal, trace)?;
        let value = self.parse_expression(trace)?;
        self.eat(Token::In, trace)?;
        let body = self.parse_expression(trace)?;
        let variable = ast::BindVariable(name, value);
        Ok(self.ea.allocate(ast::Expression::Bind(variable, body)))
    }
    //< Bind

    //> When
    pub fn parse_when(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_when, trace, "When")
    }

    pub fn _parse_when(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.eat(Token::When, trace)?;
        let mut branches = Vec::new();
        // Skip optional `|`
        if self.is_pipe() {
            self.next();
        }
        loop {
            let condition = self.parse_expression(trace)?;
            self.eat(Token::Then, trace)?;
            let then = self.parse_expression(trace)?;
            branches.push((condition, then));
            match self.peek() {
                Some(Token::Pipe) => {
                    self.next();
                }
                Some(Token::Else) => break, // `else` is required to break the loop
                _ => {
                    return Err(ParserError::unexpected_with_str(
                        self.next(),
                        format!("{} or {}", Token::Pipe, Token::Else),
                        trace,
                    ));
                }
            }
        }
        self.eat(Token::Else, trace)?;
        let default = self.parse_expression(trace)?;
        let branches = ast::WhenBranches(branches, default);
        Ok(self.ea.allocate(ast::Expression::When(branches)))
    }
    //< When

    //> If
    pub fn parse_if(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_if, trace, "If")
    }

    pub fn _parse_if(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.eat(Token::If, trace)?;
        let condition = self.parse_expression(trace)?;
        self.eat(Token::Then, trace)?;
        let then = self.parse_expression(trace)?;
        self.eat(Token::Else, trace)?;
        let otherwise = self.parse_expression(trace)?;
        Ok(self
            .ea
            .allocate(ast::Expression::If(condition, then, otherwise)))
    }
    //< If

    //> New
    pub fn parse_new(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_new, trace, "New")
    }

    pub fn _parse_new(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.eat(Token::New, trace)?;
        let name = self.eat_name(trace)?;
        self.eat(Token::OpenCurly, trace)?;
        let fields = self.parse_new_fields(trace)?;
        self.eat(Token::CloseCurly, trace)?;
        Ok(self.ea.allocate(ast::Expression::New(name, fields)))
    }

    pub fn parse_new_fields(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::NewFields, ParserError> {
        self.step(Self::_parse_new_fields, trace, "NewFields")
    }

    pub fn _parse_new_fields(
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
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_type_cast, trace, "TypeCast")
    }

    pub fn _parse_type_cast(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        let mut value = self.parse_infix_lowest(trace)?;
        while self.is_colon_colon() {
            self.next();
            let ty = self.eat_name(trace)?;
            value = self.ea.allocate(ast::Expression::TypeCast(value, ty));
        }
        Ok(value)
    }
    //< TypeCast

    //> Infix
    fn _parse_infix(
        &mut self,
        is_operator: impl Fn(&Self) -> bool,
        parse_lhs: impl Fn(
            &mut Self,
            &mut Vec<&'static str>,
        ) -> Result<ast::ExpressionRef, ParserError>,
        parse_rhs: impl Fn(
            &mut Self,
            &mut Vec<&'static str>,
        ) -> Result<ast::ExpressionRef, ParserError>,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        let mut lhs = parse_lhs(self, trace)?;
        if is_operator(self) {
            let operator = ast::BinaryOperator(self.next().unwrap()); // Checked
            let rhs = parse_rhs(self, trace)?;
            lhs = self
                .ea
                .allocate(ast::Expression::Binary(operator, lhs, rhs));
        }
        Ok(lhs)
    }

    //> InfixLowest
    pub fn parse_infix_lowest(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_lowest, trace, "InfixLowest")
    }

    pub fn _parse_infix_lowest(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_lowest_operator,
            Self::parse_infix_lower,
            Self::parse_infix_lowest,
            trace,
        )
    }

    fn is_infix_lowest_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Dollar))
    }
    //< InfixLowest

    //> InfixLower
    pub fn parse_infix_lower(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_lower, trace, "InfixLower")
    }

    pub fn _parse_infix_lower(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_lower_operator,
            Self::parse_infix_low,
            Self::parse_infix_lower,
            trace,
        )
    }

    fn is_infix_lower_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::And | Token::Or))
    }
    //< InfixLower

    //> InfixLow
    pub fn parse_infix_low(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_low, trace, "InfixLow")
    }
    pub fn _parse_infix_low(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_low_operator,
            Self::parse_infix_high,
            Self::parse_infix_low,
            trace,
        )
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
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_high, trace, "InfixHigh")
    }
    pub fn _parse_infix_high(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_high_operator,
            Self::parse_infix_higher,
            Self::parse_infix_high,
            trace,
        )
    }

    fn is_infix_high_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Plus | Token::Minus))
    }
    //< InfixHigh

    //> InfixHigher
    pub fn parse_infix_higher(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_higher, trace, "InfixHigher")
    }

    pub fn _parse_infix_higher(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_higher_operator,
            Self::parse_infix_highest,
            Self::parse_infix_higher,
            trace,
        )
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
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_infix_highest, trace, "InfixHighest")
    }

    pub fn _parse_infix_highest(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self._parse_infix(
            Self::is_infix_highest_operator,
            Self::parse_application,
            Self::parse_infix_highest,
            trace,
        )
    }

    fn is_infix_highest_operator(&self) -> bool {
        matches!(self.peek(), Some(Token::Caret))
    }
    //< InfixHighest
    //< Infix

    //> Application
    pub fn parse_application(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_application, trace, "Application")
    }

    pub fn _parse_application(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        let f = self.parse_property(trace)?;
        let mut arguments = Vec::new();
        while self.is_atom() {
            arguments.push(self.parse_property(trace)?);
        }
        if arguments.is_empty() {
            return Ok(f);
        }
        Ok(self.ea.allocate(ast::Expression::Application(f, arguments)))
    }

    fn is_atom(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Name(_, _)
                    | Token::Integer(_)
                    | Token::String(_)
                    | Token::OpenRound
            )
        )
    }
    //< Application

    //> Property
    pub fn parse_property(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        self.step(Self::_parse_property, trace, "Property")
    }

    pub fn _parse_property(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::ExpressionRef, ParserError> {
        let a = self.parse_atom(trace)?;
        let mut properties = Vec::new();
        while self.is_dot() {
            self.next(); // Skip `.`
            if self.is_atom() {
                properties.push(self.parse_atom(trace)?);
            } else {
                return Err(ParserError::unexpected_with_str(
                    self.next(),
                    expected_atom_message(),
                    trace,
                ));
            }
        }
        if properties.is_empty() {
            return Ok(self.ea.allocate(ast::Expression::Atom(a)));
        }
        Ok(self.ea.allocate(ast::Expression::Property(a, properties)))
    }
    //< Property

    //> Atom
    pub fn parse_atom(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        self.step(Self::_parse_atom, trace, "Atom")
    }

    pub fn _parse_atom(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        match self.next() {
            Some(Token::Name(identifier, kind)) => {
                Ok(ast::Atom::Variable(ast::Name(identifier, kind)))
            }
            Some(Token::Integer(value)) => Ok(ast::Atom::Integer(value)),
            Some(Token::String(text)) => Ok(ast::Atom::String(text)),
            Some(Token::OpenRound) => self.parse_parenthesized(trace),
            actual => Err(ParserError::unexpected_with_str(
                actual,
                expected_atom_message(),
                trace,
            )),
        }
    }

    //> Parenthesized
    pub fn parse_parenthesized(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        self.step(Self::_parse_parenthesized, trace, "Parenthesized")
    }

    pub fn _parse_parenthesized(
        &mut self,
        trace: &mut Vec<&'static str>,
    ) -> Result<ast::Atom, ParserError> {
        // "(" was consumed
        let inner = self.parse_expression(trace)?;
        self.eat(Token::CloseRound, trace)?;
        Ok(ast::Atom::Parenthesized(inner))
    }
    //< Parenthesized
    //< Atom
}
//< Expression
