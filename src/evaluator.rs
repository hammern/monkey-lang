use crate::parser::ast::{Expression, Infix, Literal, Prefix, Statement, Statements};

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
        Expression::Infix(infix, left, right) => Some(eval_infix_expression(
            infix,
            eval_expression(*left)?,
            eval_expression(*right)?,
        )),
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

fn eval_infix_expression(operator: Infix, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Int(left_int), Object::Int(right_int)) => {
            eval_integer_infix_expression(operator, left_int, right_int)
        }
        (Object::Bool(left_bool), Object::Bool(right_bool)) => {
            eval_boolean_infix_expression(operator, left_bool, right_bool)
        }
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(operator: Infix, left_int: i64, right_int: i64) -> Object {
    match operator {
        Infix::Plus => Object::Int(left_int + right_int),
        Infix::Minus => Object::Int(left_int - right_int),
        Infix::Asterisk => Object::Int(left_int * right_int),
        Infix::Slash => Object::Int(left_int / right_int),
        Infix::LesserThan => Object::Bool(left_int < right_int),
        Infix::GreaterThan => Object::Bool(left_int > right_int),
        Infix::Equal => Object::Bool(left_int == right_int),
        Infix::NotEqual => Object::Bool(left_int != right_int),
        _ => Object::Null,
    }
}

fn eval_boolean_infix_expression(operator: Infix, left_bool: bool, right_bool: bool) -> Object {
    match operator {
        Infix::Equal => Object::Bool(left_bool == right_bool),
        Infix::NotEqual => Object::Bool(left_bool != right_bool),
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
            ("5 + 5 + 5 + 5 -10", Object::Int(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Int(32)),
            ("-50 + 100 -50", Object::Int(0)),
            ("5 * 2 + 10", Object::Int(20)),
            ("5 + 2 * 10", Object::Int(25)),
            ("20 + 2 * -10", Object::Int(0)),
            ("50 / 2 * 2 + 10", Object::Int(60)),
            ("2 * (5 + 10)", Object::Int(30)),
            ("3 * 3 * 3 + 10", Object::Int(37)),
            ("3 * (3 * 3) + 10", Object::Int(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Int(50)),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", Object::Bool(true)),
            ("false", Object::Bool(false)),
            ("1 < 2", Object::Bool(true)),
            ("1 > 2", Object::Bool(false)),
            ("1 < 1", Object::Bool(false)),
            ("1 > 1", Object::Bool(false)),
            ("1 == 1", Object::Bool(true)),
            ("1 != 1", Object::Bool(false)),
            ("1 == 2", Object::Bool(false)),
            ("1 != 2", Object::Bool(true)),
            ("true == true", Object::Bool(true)),
            ("false == false", Object::Bool(true)),
            ("true == false", Object::Bool(false)),
            ("true != false", Object::Bool(true)),
            ("false != true", Object::Bool(true)),
            ("(1 < 2) == true", Object::Bool(true)),
            ("(1 < 2) == false", Object::Bool(false)),
            ("(1 > 2) == true", Object::Bool(false)),
            ("(1 > 2) == false", Object::Bool(true)),
        ];

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
