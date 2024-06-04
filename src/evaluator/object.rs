use crate::parser::ast::{Identifier, Statements};

use super::enviroment::EnviromentType;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Object>),
    Function(Vec<Identifier>, Statements, EnviromentType),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Builtin(BuiltinFunction),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Function(parameters, body, _) => {
                write!(f, "Function({parameters:?}, {body:?})")
            }
            _ => write!(f, "{self:?}"),
        }
    }
}
