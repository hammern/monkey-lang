use crate::{lexer::Lexer, token::Token};

use self::ast::{
    Expression, Identifier, Infix, Literal, Precedence, Prefix, Statement, Statements,
};

mod ast;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
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
        let mut statements: Statements = vec![];

        loop {
            if self.current_token == Token::Eof {
                break;
            }

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

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let(identifier, Expression::Temp))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        while self.current_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(Expression::Temp))
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
            Token::True => Some(Expression::Literal(Literal::Bool(true))),
            Token::False => Some(Expression::Literal(Literal::Bool(false))),
            Token::Minus => self.parse_prefix_expression(Prefix::Minus),
            Token::Bang => self.parse_prefix_expression(Prefix::Bang),
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
            _ => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(&mut self, left_expression: Expression) -> Option<Expression> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Asterisk => Infix::Asterisk,
            Token::Bang => Infix::Bang,
            Token::Slash => Infix::Slash,
            Token::LesserThan => Infix::LesserThan,
            Token::GreaterThan => Infix::GreaterThan,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            _ => return None,
        };

        let precedence = self.get_precedence(&self.current_token);

        self.next_token();

        let right_expression = self.parse_expression(precedence)?;

        Some(Expression::Infix(
            infix,
            Box::new(left_expression),
            Box::new(right_expression),
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
            let y = 10;
            let foobar = 838383;
        ";

        let tests = vec![
            Statement::Let(Identifier(String::from("x")), Expression::Temp),
            Statement::Let(Identifier(String::from("y")), Expression::Temp),
            Statement::Let(Identifier(String::from("foobar")), Expression::Temp),
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
            Statement::Return(Expression::Temp),
            Statement::Return(Expression::Temp),
            Statement::Return(Expression::Temp),
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
        ";

        let tests = vec![
            Statement::Expression(Expression::Literal(Literal::Int(5))),
            Statement::Expression(Expression::Literal(Literal::Bool(true))),
            Statement::Expression(Expression::Literal(Literal::Bool(false))),
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
        ];

        for (input, test) in tests {
            test_parser(input, vec![test]);
        }
    }
}
