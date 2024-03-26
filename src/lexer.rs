#![allow(dead_code)]
use crate::token::{lookup_identifier, Token};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut read_next_char = true;

        let token = match self.ch {
            b'=' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::Equal
                }
                _ => Token::Assign,
            },
            b'!' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::NotEqual
                }
                _ => Token::Bang,
            },
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'/' => Token::Slash,
            b'*' => Token::Asterisk,
            b'<' => Token::LesserThan,
            b'>' => Token::GreaterThan,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            0 => Token::Eof,
            _ if self.is_letter() => {
                read_next_char = false;
                self.read_identifier()
            }
            _ if self.is_digit() => {
                read_next_char = false;
                self.read_number()
            }
            _ => Token::Illegal,
        };

        if read_next_char {
            self.read_char();
        }

        token
    }

    fn skip_whitespace(&mut self) {
        while let b' ' | b'\t' | b'\n' | b'\r' = self.ch {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;

        while self.is_letter() {
            self.read_char();
        }

        lookup_identifier(&self.input[position..self.position])
    }

    fn is_letter(&self) -> bool {
        self.ch.is_ascii_alphabetic() || self.ch == b'_'
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;

        while self.is_digit() {
            self.read_char();
        }

        Token::Int(self.input[position..self.position].parse().unwrap())
    }

    fn is_digit(&self) -> bool {
        self.ch.is_ascii_digit()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token_first() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input);

        let tests = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        for expected_token in tests {
            let token = lexer.next_token();

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_next_token_second() {
        let input = "
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
        ";
        let mut lexer = Lexer::new(input);

        let tests = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        for expected_token in tests {
            let token = lexer.next_token();

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_next_token_third() {
        let input = "
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        ";
        let mut lexer = Lexer::new(input);

        let tests = vec![
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LesserThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::LesserThan,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEqual,
            Token::Int(9),
            Token::Semicolon,
        ];

        for expected_token in tests {
            let token = lexer.next_token();

            assert_eq!(expected_token, token);
        }
    }
}
