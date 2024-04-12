pub type Statements = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);
