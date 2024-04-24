use crate::parser::ast::{Expression, Literal, Statement, Statements};

use self::object::Object;

mod object;

pub fn eval(statements: Statements) -> Option<Object> {
    let mut result = None;

    for statement in statements {
        result = match statement {
            Statement::Expression(expression) => eval_expression(expression),
            _ => None,
        };
    }

    result
}

fn eval_expression(expression: Expression) -> Option<Object> {
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Int(int) => Some(Object::Int(int)),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(tests: Vec<(&str, Object)>) {
        for (input, test) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            assert_eq!(test, eval(parser.parse_program()).unwrap());
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", Object::Int(5)), ("10", Object::Int(10))];

        test_eval(tests);
    }
}
