use crate::{lexer::Lexer, token::Token};

use self::ast::{
    Expression, Identifier, Infix, Literal, Precedence, Prefix, Statement, Statements,
};

pub mod ast;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Statements {
        let mut statements = vec![];

        while self.current_token != Token::Eof {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        statements
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {token}, got {} instead",
            self.peek_token
        ));
    }

    fn no_prefix_parse_error(&mut self) {
        self.errors.push(format!(
            "no prefix parse function found for {}",
            self.current_token
        ));
    }

    fn parse_block_statement(&mut self) -> Statements {
        self.next_token();

        let mut statements = vec![];

        while self.current_token != Token::Eof && self.current_token != Token::RBrace {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }

            self.next_token();
        }

        statements
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let identifier = match self.peek_token.clone() {
            Token::Ident(ident) => {
                self.next_token();
                Some(Identifier(ident))
            }
            _ => {
                self.peek_error(Token::Ident(String::from("")));
                return None;
            }
        }?;

        self.expect_peek(Token::Assign)?;
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(identifier, expression))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(expression))
    }

    fn expect_peek(&mut self, token: Token) -> Option<()> {
        if self.peek_token == token {
            self.next_token();
            return Some(());
        }

        self.peek_error(token);
        None
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_expression = match self.current_token.clone() {
            Token::Ident(ident) => Some(Expression::Identifier(Identifier(ident))),
            Token::Int(int) => Some(Expression::Literal(Literal::Int(int))),
            Token::String(string) => Some(Expression::Literal(Literal::String(string))),
            Token::True => Some(Expression::Literal(Literal::Bool(true))),
            Token::False => Some(Expression::Literal(Literal::Bool(false))),
            Token::LBracket => Some(Expression::Literal(Literal::Array(
                self.parse_expression_list(Token::RBracket)?,
            ))),
            Token::Minus => self.parse_prefix_expression(Prefix::Minus),
            Token::Bang => self.parse_prefix_expression(Prefix::Bang),
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_expression(),
            _ => {
                self.no_prefix_parse_error();
                None
            }
        }?;

        while self.peek_token != Token::Semicolon
            && precedence < self.get_precedence(&self.peek_token)
        {
            left_expression = match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::LesserThan
                | Token::GreaterThan
                | Token::Equal
                | Token::NotEqual => {
                    self.next_token();
                    self.parse_infix_expression(left_expression)?
                }
                Token::LParen => {
                    self.next_token();
                    self.parse_call_expression(left_expression)?
                }
                Token::LBracket => {
                    self.next_token();
                    self.parse_index_expression(left_expression)?
                }
                _ => return Some(left_expression),
            };
        }

        Some(left_expression)
    }

    fn parse_prefix_expression(&mut self, prefix: Prefix) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(prefix, Box::new(expression)))
    }

    fn get_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LesserThan | Token::GreaterThan => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(&mut self, left_expression: Expression) -> Option<Expression> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Asterisk,
            Token::Slash => Infix::Slash,
            Token::LesserThan => Infix::LesserThan,
            Token::GreaterThan => Infix::GreaterThan,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            _ => return None,
        };

        let precedence = self.get_precedence(&self.current_token);

        self.next_token();

        Some(Expression::Infix(
            infix,
            Box::new(left_expression),
            Box::new(self.parse_expression(precedence)?),
        ))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        self.expect_peek(Token::RParen)?;

        expression
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        self.expect_peek(Token::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;
        self.expect_peek(Token::LBrace)?;

        let consequence = self.parse_block_statement();
        let mut alternative = None;

        if self.peek_token == Token::Else {
            self.next_token();

            self.expect_peek(Token::LBrace)?;

            alternative = Some(self.parse_block_statement());
        }

        Some(Expression::If(
            Box::new(condition),
            consequence,
            alternative,
        ))
    }

    fn parse_function_expression(&mut self) -> Option<Expression> {
        self.expect_peek(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace)?;

        Some(Expression::Function(
            parameters,
            self.parse_block_statement(),
        ))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];

        if self.peek_token == Token::RParen {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        if let Token::Ident(ident) = self.current_token.clone() {
            identifiers.push(Identifier(ident));
        }

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            if let Token::Ident(ident) = self.current_token.clone() {
                identifiers.push(Identifier(ident));
            }
        }

        self.expect_peek(Token::RParen)?;

        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::Call(
            Box::new(function),
            self.parse_expression_list(Token::RParen)?,
        ))
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut list = vec![];

        if self.peek_token == end {
            self.next_token();
            return Some(list);
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();

            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Some(list)
    }

    fn parse_index_expression(&mut self, left_expression: Expression) -> Option<Expression> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RBracket)?;

        Some(Expression::Index(
            Box::new(left_expression),
            Box::new(index),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_parser(input: &str, tests: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let statements = parser.parse_program();
        check_parser_errors(parser);

        assert_eq!(statements, tests);
    }

    fn check_parser_errors(parser: Parser) {
        parser.errors.iter().for_each(|error| println!("{error}"));
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = true;
            let foobar = y;
        ";

        let tests = vec![
            Statement::Let(
                Identifier(String::from("x")),
                Expression::Literal(Literal::Int(5)),
            ),
            Statement::Let(
                Identifier(String::from("y")),
                Expression::Literal(Literal::Bool(true)),
            ),
            Statement::Let(
                Identifier(String::from("foobar")),
                Expression::Identifier(Identifier(String::from("y"))),
            ),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_return_statements() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let tests = vec![
            Statement::Return(Expression::Literal(Literal::Int(5))),
            Statement::Return(Expression::Literal(Literal::Int(10))),
            Statement::Return(Expression::Literal(Literal::Int(993322))),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";

        let tests = vec![Statement::Expression(Expression::Identifier(Identifier(
            String::from("foobar"),
        )))];

        test_parser(input, tests);
    }

    #[test]
    fn test_literal_expressions() {
        let input = "
            5;
            true;
            false;
            \"hello world\";
            [1, 2 * 2, 3 + 3];
        ";

        let tests = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Bool(true))),
            Statement::Expression(Expression::Literal(Literal::Bool(false))),
            Statement::Expression(Expression::Literal(Literal::String(String::from(
                "hello world",
            )))),
            Statement::Expression(Expression::Literal(Literal::Array(vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Literal(Literal::Int(2))),
                    Box::new(Expression::Literal(Literal::Int(2))),
                ),
                Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(3))),
                    Box::new(Expression::Literal(Literal::Int(3))),
                ),
            ]))),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_prefix_expressions() {
        let input = "
            !5;
            -15;
            !true;
            !false;
        ";

        let tests = vec![
            Statement::Expression(Expression::Prefix(
                Prefix::Bang,
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::Minus,
                Box::new(Expression::Literal(Literal::Int(15))),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::Bang,
                Box::new(Expression::Literal(Literal::Bool(true))),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::Bang,
                Box::new(Expression::Literal(Literal::Bool(false))),
            )),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 == 5;
            5 != 5;
            true == true;
            true != false;
            false == false;
        ";

        let tests = vec![
            Statement::Expression(Expression::Infix(
                Infix::Plus,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Minus,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Asterisk,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Slash,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::GreaterThan,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::LesserThan,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Equal,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::NotEqual,
                Box::new(Expression::Literal(Literal::Int(5))),
                Box::new(Expression::Literal(Literal::Int(5))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Equal,
                Box::new(Expression::Literal(Literal::Bool(true))),
                Box::new(Expression::Literal(Literal::Bool(true))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::NotEqual,
                Box::new(Expression::Literal(Literal::Bool(true))),
                Box::new(Expression::Literal(Literal::Bool(false))),
            )),
            Statement::Expression(Expression::Infix(
                Infix::Equal,
                Box::new(Expression::Literal(Literal::Bool(false))),
                Box::new(Expression::Literal(Literal::Bool(false))),
            )),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let tests = vec![Statement::Expression(Expression::If(
            Box::new(Expression::Infix(
                Infix::LesserThan,
                Box::new(Expression::Identifier(Identifier(String::from("x")))),
                Box::new(Expression::Identifier(Identifier(String::from("y")))),
            )),
            vec![Statement::Expression(Expression::Identifier(Identifier(
                String::from("x"),
            )))],
            None,
        ))];

        test_parser(input, tests);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let tests = vec![Statement::Expression(Expression::If(
            Box::new(Expression::Infix(
                Infix::LesserThan,
                Box::new(Expression::Identifier(Identifier(String::from("x")))),
                Box::new(Expression::Identifier(Identifier(String::from("y")))),
            )),
            vec![Statement::Expression(Expression::Identifier(Identifier(
                String::from("x"),
            )))],
            Some(vec![Statement::Expression(Expression::Identifier(
                Identifier(String::from("y")),
            ))]),
        ))];

        test_parser(input, tests);
    }

    #[test]
    fn test_function_expression() {
        let input = "fn(x, y) { x + y; }";

        let tests = vec![Statement::Expression(Expression::Function(
            vec![Identifier(String::from("x")), Identifier(String::from("y"))],
            vec![Statement::Expression(Expression::Infix(
                Infix::Plus,
                Box::new(Expression::Identifier(Identifier(String::from("x")))),
                Box::new(Expression::Identifier(Identifier(String::from("y")))),
            ))],
        ))];

        test_parser(input, tests);
    }

    #[test]
    fn test_function_parameters() {
        let input = "
            fn() {};
            fn(x) {};
            fn(x, y, z) {};
        ";

        let tests = vec![
            Statement::Expression(Expression::Function(vec![], vec![])),
            Statement::Expression(Expression::Function(
                vec![Identifier(String::from("x"))],
                vec![],
            )),
            Statement::Expression(Expression::Function(
                vec![
                    Identifier(String::from("x")),
                    Identifier(String::from("y")),
                    Identifier(String::from("z")),
                ],
                vec![],
            )),
        ];

        test_parser(input, tests);
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let tests = vec![Statement::Expression(Expression::Call(
            Box::new(Expression::Identifier(Identifier(String::from("add")))),
            vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Literal(Literal::Int(2))),
                    Box::new(Expression::Literal(Literal::Int(3))),
                ),
                Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Literal(Literal::Int(4))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                ),
            ],
        ))];

        test_parser(input, tests);
    }

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";

        let tests = vec![Statement::Expression(Expression::Index(
            Box::new(Expression::Identifier(Identifier(String::from("myArray")))),
            Box::new(Expression::Infix(
                Infix::Plus,
                Box::new(Expression::Literal(Literal::Int(1))),
                Box::new(Expression::Literal(Literal::Int(1))),
            )),
        ))];

        test_parser(input, tests);
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
            (
                "-a * b",
                Statement::Expression(Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("b")))),
                )),
            ),
            (
                "!-a",
                Statement::Expression(Expression::Prefix(
                    Prefix::Bang,
                    Box::new(Expression::Prefix(
                        Prefix::Minus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    )),
                )),
            ),
            (
                "a + b + c",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b - c",
                Statement::Expression(Expression::Infix(
                    Infix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b * c",
                Statement::Expression(Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Infix(
                        Infix::Asterisk,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a * b / c",
                Statement::Expression(Expression::Infix(
                    Infix::Slash,
                    Box::new(Expression::Infix(
                        Infix::Asterisk,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                )),
            ),
            (
                "a + b / c",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("a")))),
                    Box::new(Expression::Infix(
                        Infix::Slash,
                        Box::new(Expression::Identifier(Identifier(String::from("b")))),
                        Box::new(Expression::Identifier(Identifier(String::from("c")))),
                    )),
                )),
            ),
            (
                "a + b * c + d / e - f",
                Statement::Expression(Expression::Infix(
                    Infix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Identifier(Identifier(String::from("a")))),
                            Box::new(Expression::Infix(
                                Infix::Asterisk,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )),
                        )),
                        Box::new(Expression::Infix(
                            Infix::Slash,
                            Box::new(Expression::Identifier(Identifier(String::from("d")))),
                            Box::new(Expression::Identifier(Identifier(String::from("e")))),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("f")))),
                )),
            ),
            (
                "5 > 4 == 3 < 4",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expression::Infix(
                        Infix::LesserThan,
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Box::new(Expression::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Box::new(Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Box::new(Expression::Literal(Literal::Int(5))),
                        )),
                    )),
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Literal(Literal::Int(3))),
                            Box::new(Expression::Literal(Literal::Int(1))),
                        )),
                        Box::new(Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Box::new(Expression::Literal(Literal::Int(5))),
                        )),
                    )),
                )),
            ),
            (
                "true",
                Statement::Expression(Expression::Literal(Literal::Bool(true))),
            ),
            (
                "false",
                Statement::Expression(Expression::Literal(Literal::Bool(false))),
            ),
            (
                "3 > 5 == false",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::GreaterThan,
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Bool(false))),
                )),
            ),
            (
                "3 < 5 == true",
                Statement::Expression(Expression::Infix(
                    Infix::Equal,
                    Box::new(Expression::Infix(
                        Infix::LesserThan,
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Bool(true))),
                )),
            ),
            (
                "1 + (2 + 3) + 4",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(1))),
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Int(2))),
                            Box::new(Expression::Literal(Literal::Int(3))),
                        )),
                    )),
                    Box::new(Expression::Literal(Literal::Int(4))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Statement::Expression(Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expression::Literal(Literal::Int(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Statement::Expression(Expression::Infix(
                    Infix::Slash,
                    Box::new(Expression::Literal(Literal::Int(2))),
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "-(5 + 5)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Minus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Literal(Literal::Int(5))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "!(true == true)",
                Statement::Expression(Expression::Prefix(
                    Prefix::Bang,
                    Box::new(Expression::Infix(
                        Infix::Equal,
                        Box::new(Expression::Literal(Literal::Bool(true))),
                        Box::new(Expression::Literal(Literal::Bool(true))),
                    )),
                )),
            ),
            (
                "a + add(b * c) + d",
                Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Call(
                            Box::new(Expression::Identifier(Identifier(String::from("add")))),
                            vec![Expression::Infix(
                                Infix::Asterisk,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )],
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                )),
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    vec![
                        Expression::Identifier(Identifier(String::from("a"))),
                        Expression::Identifier(Identifier(String::from("b"))),
                        Expression::Literal(Literal::Int(1)),
                        Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Literal(Literal::Int(2))),
                            Box::new(Expression::Literal(Literal::Int(3))),
                        ),
                        Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Literal(Literal::Int(4))),
                            Box::new(Expression::Literal(Literal::Int(5))),
                        ),
                        Expression::Call(
                            Box::new(Expression::Identifier(Identifier(String::from("add")))),
                            vec![
                                Expression::Literal(Literal::Int(6)),
                                Expression::Infix(
                                    Infix::Asterisk,
                                    Box::new(Expression::Literal(Literal::Int(7))),
                                    Box::new(Expression::Literal(Literal::Int(8))),
                                ),
                            ],
                        ),
                    ],
                )),
            ),
            (
                "add(a + b + c * d / f + g)",
                Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    vec![Expression::Infix(
                        Infix::Plus,
                        Box::new(Expression::Infix(
                            Infix::Plus,
                            Box::new(Expression::Infix(
                                Infix::Plus,
                                Box::new(Expression::Identifier(Identifier(String::from("a")))),
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                            )),
                            Box::new(Expression::Infix(
                                Infix::Slash,
                                Box::new(Expression::Infix(
                                    Infix::Asterisk,
                                    Box::new(Expression::Identifier(Identifier(String::from("c")))),
                                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                                )),
                                Box::new(Expression::Identifier(Identifier(String::from("f")))),
                            )),
                        )),
                        Box::new(Expression::Identifier(Identifier(String::from("g")))),
                    )],
                )),
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                Statement::Expression(Expression::Infix(
                    Infix::Asterisk,
                    Box::new(Expression::Infix(
                        Infix::Asterisk,
                        Box::new(Expression::Identifier(Identifier(String::from("a")))),
                        Box::new(Expression::Index(
                            Box::new(Expression::Literal(Literal::Array(vec![
                                Expression::Literal(Literal::Int(1)),
                                Expression::Literal(Literal::Int(2)),
                                Expression::Literal(Literal::Int(3)),
                                Expression::Literal(Literal::Int(4)),
                            ]))),
                            Box::new(Expression::Infix(
                                Infix::Asterisk,
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Identifier(Identifier(String::from("c")))),
                            )),
                        )),
                    )),
                    Box::new(Expression::Identifier(Identifier(String::from("d")))),
                )),
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                Statement::Expression(Expression::Call(
                    Box::new(Expression::Identifier(Identifier(String::from("add")))),
                    vec![
                        Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Identifier(Identifier(String::from("a")))),
                            Box::new(Expression::Index(
                                Box::new(Expression::Identifier(Identifier(String::from("b")))),
                                Box::new(Expression::Literal(Literal::Int(2))),
                            )),
                        ),
                        Expression::Index(
                            Box::new(Expression::Identifier(Identifier(String::from("b")))),
                            Box::new(Expression::Literal(Literal::Int(1))),
                        ),
                        Expression::Infix(
                            Infix::Asterisk,
                            Box::new(Expression::Literal(Literal::Int(2))),
                            Box::new(Expression::Index(
                                Box::new(Expression::Literal(Literal::Array(vec![
                                    Expression::Literal(Literal::Int(1)),
                                    Expression::Literal(Literal::Int(2)),
                                ]))),
                                Box::new(Expression::Literal(Literal::Int(1))),
                            )),
                        ),
                    ],
                )),
            ),
        ];

        for (input, test) in tests {
            test_parser(input, vec![test]);
        }
    }
}
