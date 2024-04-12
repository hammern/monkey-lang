use crate::{lexer::Lexer, token::Token};

use self::ast::{Expression, Identifier, Statement, Statements};

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

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
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

        Some(Statement::LetStatement(identifier, Expression::Temp))
    }

    fn expect_peek(&mut self, token: Token) -> Option<()> {
        if self.peek_token == token {
            self.next_token();
            return Some(());
        }

        self.peek_error(token);
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let statements = parser.parse_program();
        check_parser_errors(parser);

        let tests = vec![
            Statement::LetStatement(Identifier(String::from("x")), Expression::Temp),
            Statement::LetStatement(Identifier(String::from("y")), Expression::Temp),
            Statement::LetStatement(Identifier(String::from("foobar")), Expression::Temp),
        ];

        assert_eq!(statements, tests);
    }

    fn check_parser_errors(parser: Parser) {
        parser.errors.iter().for_each(|error| println!("{error}"));
        assert_eq!(parser.errors.len(), 0);
    }
}
