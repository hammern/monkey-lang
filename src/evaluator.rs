use crate::parser::ast::{Expression, Infix, Literal, Prefix, Statement, Statements};

use self::object::Object;

mod object;

pub fn eval(statements: Statements) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement);

        match result {
            Object::ReturnValue(result) => return *result,
            Object::Error(message) => return Object::Error(message),
            _ => (),
        };
    }

    result
}

fn eval_block_statement(statements: Statements) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement);

        match result {
            Object::ReturnValue(_) => return result,
            Object::Error(message) => return Object::Error(message),
            _ => (),
        };
    }

    result
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Return(expression) => Object::ReturnValue(Box::new(eval_expression(expression))),
        Statement::Expression(expression) => eval_expression(expression),
        _ => Object::Error(String::from("[TODO] statement not implemented")),
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::Literal(literal) => eval_literal_expression(literal),
        Expression::Prefix(prefix, right) => {
            eval_prefix_expression(prefix, eval_expression(*right))
        }
        Expression::Infix(infix, left, right) => {
            eval_infix_expression(infix, eval_expression(*left), eval_expression(*right))
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(*condition, consequence, alternative)
        }
        _ => Object::Error(String::from("[TODO] expression not implemented")),
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
        _ => Object::Error(format!("unknown operator: -{right:?}")),
    }
}

fn eval_infix_expression(operator: Infix, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone()) {
        (Object::Int(left_int), Object::Int(right_int)) => {
            eval_integer_infix_expression(operator, left_int, right_int)
        }
        (Object::Bool(left_bool), Object::Bool(right_bool)) => {
            eval_boolean_infix_expression(operator, left_bool, right_bool)
        }
        _ => Object::Error(format!("type mismatch: {left:?} {operator:?} {right:?}",)),
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
    }
}

fn eval_boolean_infix_expression(operator: Infix, left_bool: bool, right_bool: bool) -> Object {
    match operator {
        Infix::Equal => Object::Bool(left_bool == right_bool),
        Infix::NotEqual => Object::Bool(left_bool != right_bool),
        _ => Object::Error(format!(
            "unknown operator: Bool({left_bool:?}) {operator:?} Bool({right_bool:?})",
        )),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Vec<Statement>,
    alternative: Option<Vec<Statement>>,
) -> Object {
    eval_block_statement(match eval_expression(condition) {
        Object::Bool(false) | Object::Null => match alternative {
            Some(alternative) => alternative,
            _ => return Object::Null,
        },
        _ => consequence,
    })
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn test_eval(tests: Vec<(&str, Object)>) {
        for (input, test) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            assert_eq!(test, eval(parser.parse_program()));
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

    #[test]
    fn test_eval_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Int(10)),
            ("if (1 < 2) { 10 }", Object::Int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_return_statements() {
        let tests = vec![
            ("return 10;", Object::Int(10)),
            ("return 10; 9;", Object::Int(10)),
            ("return 2 * 5; 9;", Object::Int(10)),
            ("9; return 2 * 5; 9;", Object::Int(10)),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }",
                Object::Int(10),
            ),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                Object::Error(String::from("type mismatch: Int(5) Plus Bool(true)")),
            ),
            (
                "5 + true; 5;",
                Object::Error(String::from("type mismatch: Int(5) Plus Bool(true)")),
            ),
            (
                "-true;",
                Object::Error(String::from("unknown operator: -Bool(true)")),
            ),
            (
                "true + false;",
                Object::Error(String::from(
                    "unknown operator: Bool(true) Plus Bool(false)",
                )),
            ),
            (
                "5; true + false; 5;",
                Object::Error(String::from(
                    "unknown operator: Bool(true) Plus Bool(false)",
                )),
            ),
            (
                "if (10 > 1) { true + false; }",
                Object::Error(String::from(
                    "unknown operator: Bool(true) Plus Bool(false)",
                )),
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }",
                Object::Error(String::from(
                    "unknown operator: Bool(true) Plus Bool(false)",
                )),
            ),
        ];

        test_eval(tests);
    }
}
