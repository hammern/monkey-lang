use monkey_lang::{lexer::Lexer, token::Token};
use std::io::Write;

fn main() {
    println!("Hello! This is the Monkey programming language REPL!");
    println!("Feel free to type in commands");

    loop {
        let mut input = String::new();

        print!(">> ");
        std::io::stdout().flush().unwrap();

        std::io::stdin().read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(&input);

        loop {
            let token = lexer.next_token();
            println!("{:?}", token);

            if token == Token::Eof {
                break;
            }
        }
    }
}
