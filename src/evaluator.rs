use crate::parser::ast::{Expression, Literal, Prefix, Statement, Statements};

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
        Expression::Literal(literal) => Some(eval_literal_expression(literal)),
        Expression::Prefix(prefix, right) => {
            Some(eval_prefix_expression(prefix, eval_expression(*right)?))
        }
        _ => None,
    }
}

fn eval_literal_expression(literal: Literal) -> Object {
    match literal {
        Literal::Int(int) => Object::Int(int),
        Literal::Bool(bool) => Object::Bool(bool),
    }
}

fn eval_prefix_expression(operator: Prefix, right: Object) -> Object {
    match operator {
        Prefix::Bang => eval_bang_operator_expression(right),
        Prefix::Minus => eval_minus_prefix_operator_expression(right),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Bool(true) => Object::Bool(false),
        Object::Bool(false) => Object::Bool(true),
        Object::Null => Object::Bool(true),
        _ => Object::Bool(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Int(int) => Object::Int(-int),
        _ => Object::Null,
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
        let tests = vec![
            ("5", Object::Int(5)),
            ("10", Object::Int(10)),
            ("-5", Object::Int(-5)),
            ("-10", Object::Int(-10)),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true", Object::Bool(true)), ("false", Object::Bool(false))];

        test_eval(tests);
    }

    #[test]
    fn test_eval_bang_operator() {
        let tests = vec![
            ("!true", Object::Bool(false)),
            ("!false", Object::Bool(true)),
            ("!5", Object::Bool(false)),
            ("!!true", Object::Bool(true)),
            ("!!false", Object::Bool(false)),
            ("!!5", Object::Bool(true)),
        ];

        test_eval(tests);
    }
}
