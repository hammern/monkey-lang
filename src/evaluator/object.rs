use crate::parser::ast::{Identifier, Statements};

use super::enviroment::EnviromentType;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Function(Vec<Identifier>, Statements, EnviromentType),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}
