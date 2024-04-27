use monkey_lang::{evaluator::eval, lexer::Lexer, parser::Parser};
use std::io::Write;

fn main() {
    println!("Hello! This is the Monkey programming language REPL!");
    println!("Feel free to type in commands");

    loop {
        let mut input = String::new();

        print!(">> ");
        std::io::stdout().flush().unwrap();

        std::io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let statements = parser.parse_program();

        if !parser.errors.is_empty() {
            parser.errors.iter().for_each(|error| println!("{error}"));
            continue;
        }

        let evaluated = eval(statements);
        println!("{evaluated:?}");
    }
}
