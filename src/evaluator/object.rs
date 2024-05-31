use crate::parser::ast::{Identifier, Statements};

use super::enviroment::EnviromentType;

pub type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Function(Vec<Identifier>, Statements, EnviromentType),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Builtin(BuiltinFunction),
}
