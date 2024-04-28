use std::{cell::RefCell, rc::Rc};

use crate::parser::ast::{Expression, Identifier, Infix, Literal, Prefix, Statement, Statements};

use self::{
    enviroment::{Enviroment, EnviromentType},
    object::Object,
};

pub mod enviroment;
mod object;

pub struct Evaluator {
    enviroment: EnviromentType,
}

impl Evaluator {
    pub fn new(enviroment: Enviroment) -> Self {
        Self {
            enviroment: Rc::new(RefCell::new(enviroment)),
        }
    }

    pub fn eval(&mut self, statements: Statements) -> Object {
        let mut result = Object::Null;

        for statement in statements {
            result = self.eval_statement(statement);

            match result {
                Object::ReturnValue(result) => return *result,
                Object::Error(message) => return Object::Error(message),
                _ => (),
            };
        }

        result
    }

    fn eval_block_statement(&mut self, statements: Statements) -> Object {
        let mut result = Object::Null;

        for statement in statements {
            result = self.eval_statement(statement);

            match result {
                Object::ReturnValue(_) => return result,
                Object::Error(message) => return Object::Error(message),
                _ => (),
            };
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Object {
        match statement {
            Statement::Let(identifier, expression) => {
                self.eval_let_statement(identifier, expression)
            }
            Statement::Return(expression) => {
                Object::ReturnValue(Box::new(self.eval_expression(expression)))
            }
            Statement::Expression(expression) => self.eval_expression(expression),
        }
    }

    fn eval_let_statement(&mut self, identifier: Identifier, expression: Expression) -> Object {
        let Identifier(name) = identifier;

        match self.eval_expression(expression) {
            Object::Error(message) => Object::Error(message),
            object => self.enviroment.borrow_mut().set(name, object),
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Object {
        match expression {
            Expression::Literal(literal) => self.eval_literal_expression(literal),
            Expression::Prefix(prefix, right) => {
                let right_expression = self.eval_expression(*right);
                if let Object::Error(_) = right_expression {
                    return right_expression;
                }

                self.eval_prefix_expression(prefix, right_expression)
            }
            Expression::Infix(infix, left, right) => {
                let left_expression = self.eval_expression(*left);
                if let Object::Error(_) = left_expression {
                    return left_expression;
                }

                let right_expression = self.eval_expression(*right);
                if let Object::Error(_) = right_expression {
                    return right_expression;
                }

                self.eval_infix_expression(infix, left_expression, right_expression)
            }
            Expression::If(condition, consequence, alternative) => {
                self.eval_if_expression(*condition, consequence, alternative)
            }
            Expression::Identifier(identifier) => {
                let Identifier(name) = identifier;
                self.eval_identifier(name)
            }
            Expression::Function(parameters, body) => {
                Object::Function(parameters, body, self.enviroment.clone())
            }
            Expression::Call(function, arguments) => {
                self.eval_call_expression(*function, arguments)
            }
        }
    }

    fn eval_literal_expression(&self, literal: Literal) -> Object {
        match literal {
            Literal::Int(int) => Object::Int(int),
            Literal::Bool(bool) => Object::Bool(bool),
        }
    }

    fn eval_prefix_expression(&self, operator: Prefix, right: Object) -> Object {
        match operator {
            Prefix::Bang => self.eval_bang_operator_expression(right),
            Prefix::Minus => self.eval_minus_prefix_operator_expression(right),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Bool(true) => Object::Bool(false),
            Object::Bool(false) => Object::Bool(true),
            Object::Null => Object::Bool(true),
            _ => Object::Bool(false),
        }
    }

    fn eval_minus_prefix_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Int(int) => Object::Int(-int),
            _ => Object::Error(format!("unknown operator: -{right:?}")),
        }
    }

    fn eval_infix_expression(&self, operator: Infix, left: Object, right: Object) -> Object {
        match (left.clone(), right.clone()) {
            (Object::Int(left_int), Object::Int(right_int)) => {
                self.eval_integer_infix_expression(operator, left_int, right_int)
            }
            (Object::Bool(left_bool), Object::Bool(right_bool)) => {
                self.eval_boolean_infix_expression(operator, left_bool, right_bool)
            }
            _ => Object::Error(format!("type mismatch: {left:?} {operator:?} {right:?}",)),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: Infix,
        left_int: i64,
        right_int: i64,
    ) -> Object {
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

    fn eval_boolean_infix_expression(
        &self,
        operator: Infix,
        left_bool: bool,
        right_bool: bool,
    ) -> Object {
        match operator {
            Infix::Equal => Object::Bool(left_bool == right_bool),
            Infix::NotEqual => Object::Bool(left_bool != right_bool),
            _ => Object::Error(format!(
                "unknown operator: Bool({left_bool:?}) {operator:?} Bool({right_bool:?})",
            )),
        }
    }

    fn eval_if_expression(
        &mut self,
        condition: Expression,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    ) -> Object {
        let statements = match self.eval_expression(condition) {
            Object::Bool(false) | Object::Null => match alternative {
                Some(alternative) => alternative,
                _ => return Object::Null,
            },
            Object::Error(message) => return Object::Error(message),
            _ => consequence,
        };

        self.eval_block_statement(statements)
    }

    fn eval_identifier(&self, name: String) -> Object {
        match self.enviroment.borrow_mut().get(&name) {
            Some(object) => object,
            None => Object::Error(format!("identifier not found: {name}")),
        }
    }

    fn eval_call_expression(&mut self, function: Expression, arguments: Vec<Expression>) -> Object {
        let (parameters, body, function_enviroment) = match self.eval_expression(function) {
            Object::Function(parameters, body, enviroment) => (parameters, body, enviroment),
            Object::Error(message) => return Object::Error(message),
            _ => return Object::Null,
        };

        let mut evaluated_arguments = vec![];

        for argument in arguments {
            match self.eval_expression(argument) {
                Object::Error(message) => return Object::Error(message),
                object => evaluated_arguments.push(object),
            }
        }

        let mut extended_env = Enviroment::new_enclosed_enviroment(function_enviroment);

        for (identifier, object) in parameters.into_iter().zip(evaluated_arguments.into_iter()) {
            let Identifier(name) = identifier;
            extended_env.set(name, object);
        }

        let current_env = self.enviroment.clone();

        self.enviroment = Rc::new(RefCell::new(extended_env));
        let result = self.eval(body);
        self.enviroment = current_env;

        result
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
            let mut evaluator = Evaluator::new(Enviroment::default());

            assert_eq!(test, evaluator.eval(parser.parse_program()));
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
    fn test_eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Int(5)),
            ("let a = 5 * 5; a;", Object::Int(25)),
            ("let a = 5; let b = a; b;", Object::Int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Int(15),
            ),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_function_object() {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Function(
                vec![Identifier(String::from("x"))],
                vec![Statement::Expression(Expression::Infix(
                    Infix::Plus,
                    Box::new(Expression::Identifier(Identifier(String::from("x")))),
                    Box::new(Expression::Literal(Literal::Int(2))),
                ))],
                Rc::new(RefCell::new(Enviroment::default())),
            ),
        )];

        test_eval(tests);
    }

    #[test]
    fn test_eval_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::Int(5)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Int(5),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Int(10)),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Int(20),
            ),
            ("fn(x) { x; }(5);", Object::Int(5)),
            (
                "let newAdder = fn(x) {
                    fn(y) { x + y };
                };

                let addTwo = newAdder(2);
                addTwo(2);",
                Object::Int(4),
            ),
            (
                "let counter = fn(x) {
                    if (x > 100) {
                        return true;
                    } else {
                        let foobar = 9999;
                        counter(x + 1);
                    }
                };
                
                counter(5);",
                Object::Bool(true),
            ),
            (
                "let factorial = fn(n) {
                    if (n == 0) {
                        return 1;
                    }

                    return n * factorial(n - 1);
                };

                factorial(5);",
                Object::Int(120),
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
            (
                "foobar;",
                Object::Error(String::from("identifier not found: foobar")),
            ),
        ];

        test_eval(tests);
    }
}
