use std::{cell::RefCell, collections::HashMap, rc::Rc, usize};

use crate::parser::ast::{Expression, Identifier, Infix, Literal, Prefix, Statement, Statements};

use self::{
    builtins::{first, last, len, push, puts, rest},
    enviroment::{Enviroment, EnviromentType},
    object::{BuiltinFunction, Object},
};

mod builtins;
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
            Expression::Index(left_expression, index_expression) => {
                let left = self.eval_expression(*left_expression);
                if let Object::Error(_) = left {
                    return left;
                }

                let index = self.eval_expression(*index_expression);
                if let Object::Error(_) = index {
                    return index;
                }

                self.eval_index_expression(left, index)
            }
        }
    }

    fn eval_literal_expression(&mut self, literal: Literal) -> Object {
        match literal {
            Literal::Int(int) => Object::Int(int),
            Literal::Bool(bool) => Object::Bool(bool),
            Literal::String(string) => Object::String(string),
            Literal::Array(array) => {
                let mut elements = vec![];

                for element in array {
                    match self.eval_expression(element) {
                        Object::Error(message) => return Object::Error(message),
                        object => elements.push(object),
                    }
                }

                Object::Array(elements)
            }
            Literal::Hash(hash) => {
                let mut pairs = HashMap::new();

                for (key_expression, value_expression) in hash {
                    let key = self.eval_expression(key_expression);
                    if let Object::Error(_) = key {
                        return key;
                    }

                    let value = self.eval_expression(value_expression);
                    if let Object::Error(_) = value {
                        return value;
                    }

                    pairs.insert(key, value);
                }

                Object::Hash(pairs)
            }
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
            (Object::String(left_string), Object::String(right_string)) => {
                self.eval_string_infix_expression(operator, left_string, right_string)
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

    fn eval_string_infix_expression(
        &self,
        operator: Infix,
        left_string: String,
        right_string: String,
    ) -> Object {
        match operator {
            Infix::Plus => Object::String(left_string + &right_string),
            _ => Object::Error(format!(
                "unknown operator: String({left_string:?}) {operator:?} String({right_string:?})",
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
        if let Some(object) = self.enviroment.borrow_mut().get(&name) {
            return object;
        }

        match name.as_str() {
            "len" => Object::Builtin(len),
            "first" => Object::Builtin(first),
            "last" => Object::Builtin(last),
            "rest" => Object::Builtin(rest),
            "push" => Object::Builtin(push),
            "puts" => Object::Builtin(puts),
            _ => Object::Error(format!("identifier not found: {name}")),
        }
    }

    fn eval_call_expression(&mut self, function: Expression, arguments: Vec<Expression>) -> Object {
        let (parameters, body, function_enviroment) = match self.eval_expression(function.clone()) {
            Object::Function(parameters, body, enviroment) => (parameters, body, enviroment),
            Object::Builtin(builtin_fn) => return self.eval_builtin_call(builtin_fn, arguments),
            Object::Error(message) => return Object::Error(message),
            _ => return Object::Error(format!("not a function: {function:?}")),
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

    fn eval_builtin_call(
        &mut self,
        builtin_fn: BuiltinFunction,
        arguments: Vec<Expression>,
    ) -> Object {
        let mut evaluated_arguments = vec![];

        for argument in arguments {
            match self.eval_expression(argument) {
                Object::Error(message) => return Object::Error(message),
                object => evaluated_arguments.push(object),
            }
        }

        builtin_fn(evaluated_arguments)
    }

    fn eval_index_expression(&mut self, left: Object, index: Object) -> Object {
        match (left.clone(), index) {
            (Object::Array(array), Object::Int(index)) => {
                self.eval_array_index_expression(array, index)
            }
            (Object::Hash(hash), index) => self.eval_hash_index_expression(hash, index),
            _ => Object::Error(format!("index operator not supported: {left:?}")),
        }
    }

    fn eval_array_index_expression(&mut self, array: Vec<Object>, index: i64) -> Object {
        if index < 0 || index > array.len().try_into().unwrap() {
            return Object::Null;
        }

        match array.get(index as usize) {
            Some(element) => element.clone(),
            None => Object::Null,
        }
    }

    fn eval_hash_index_expression(&self, hash: HashMap<Object, Object>, index: Object) -> Object {
        match index {
            Object::String(_) | Object::Int(_) | Object::Bool(_) => match hash.get(&index) {
                Some(value) => value.clone(),
                None => Object::Null,
            },
            _ => Object::Error(format!("unusable as hash key: {index:?}")),
        }
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
    fn test_eval_array_expression() {
        let tests = vec![(
            "[1, 2 * 2, 3 + 3]",
            Object::Array(vec![Object::Int(1), Object::Int(4), Object::Int(6)]),
        )];

        test_eval(tests);
    }

    #[test]
    fn test_string_concatenation() {
        let tests = vec![(
            "\"Hello\" + \" \" + \"World!\"",
            Object::String(String::from("Hello World!")),
        )];

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
    fn test_builtin_functions() {
        let tests = vec![
            ("len(\"\")", Object::Int(0)),
            ("len(\"four\")", Object::Int(4)),
            ("len(\"hello world\")", Object::Int(11)),
            ("len([1, 2, 3])", Object::Int(3)),
            (
                "len(1)",
                Object::Error(String::from("argument to `len` not supported, got Int(1)")),
            ),
            (
                "len(\"one\", \"two\")",
                Object::Error(String::from("wrong number of arguments. got=2, want=1")),
            ),
            ("first([])", Object::Null),
            ("first([1, 2, 3])", Object::Int(1)),
            (
                "first(1)",
                Object::Error(String::from(
                    "argument to `first` not supported, got Int(1)",
                )),
            ),
            (
                "first([1], [2])",
                Object::Error(String::from("wrong number of arguments. got=2, want=1")),
            ),
            ("last([])", Object::Null),
            ("last([1, 2, 3])", Object::Int(3)),
            (
                "last(1)",
                Object::Error(String::from("argument to `last` not supported, got Int(1)")),
            ),
            (
                "last([1], [2])",
                Object::Error(String::from("wrong number of arguments. got=2, want=1")),
            ),
            ("rest([])", Object::Null),
            (
                "rest([1, 2, 3])",
                Object::Array(vec![Object::Int(2), Object::Int(3)]),
            ),
            (
                "rest(1)",
                Object::Error(String::from("argument to `rest` not supported, got Int(1)")),
            ),
            (
                "rest([1], [2])",
                Object::Error(String::from("wrong number of arguments. got=2, want=1")),
            ),
            ("push([], 1)", Object::Array(vec![Object::Int(1)])),
            (
                "push(1, [])",
                Object::Error(String::from("argument to `push` must be ARRAY, got Int(1)")),
            ),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Int(1)),
            ("[1, 2, 3][1]", Object::Int(2)),
            ("[1, 2, 3][2]", Object::Int(3)),
            ("let i = 0; [1][i]", Object::Int(1)),
            ("[1, 2, 3][1 + 1]", Object::Int(3)),
            ("let myArray = [1, 2, 3]; myArray[2]", Object::Int(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
                Object::Int(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Int(2),
            ),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
        ];

        test_eval(tests);
    }

    #[test]
    fn test_eval_hash_literal() {
        let tests = vec![(
            "let two = \"two\";
            {
                \"one\": 10 - 9,
                two: 1 + 1,
                \"thr\" + \"ee\": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }
            ",
            Object::Hash(HashMap::from([
                (Object::String(String::from("one")), Object::Int(1)),
                (Object::String(String::from("two")), Object::Int(2)),
                (Object::String(String::from("three")), Object::Int(3)),
                (Object::Int(4), Object::Int(4)),
                (Object::Bool(true), Object::Int(5)),
                (Object::Bool(false), Object::Int(6)),
            ])),
        )];

        test_eval(tests);
    }

    #[test]
    fn test_eval_hash_index_expressions() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Object::Int(5)),
            ("{\"foo\": 5}[\"bar\"]", Object::Null),
            ("let key = \"foo\";{\"foo\": 5}[key]", Object::Int(5)),
            ("{}[\"foo\"]", Object::Null),
            ("{5: 5}[5]", Object::Int(5)),
            ("{true: 5}[true]", Object::Int(5)),
            ("{false: 5}[false]", Object::Int(5)),
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
            (
                "\"Hello\" - \"World!\"",
                Object::Error(String::from(
                    "unknown operator: String(\"Hello\") Minus String(\"World!\")",
                )),
            ),
            (
                "1[2]",
                Object::Error(String::from("index operator not supported: Int(1)")),
            ),
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                Object::Error(String::from("unusable as hash key: Function([Identifier(\"x\")], [Expression(Identifier(Identifier(\"x\")))], RefCell { value: Enviroment { store: {}, outer: None } })")),
            ),
        ];

        test_eval(tests);
    }
}
