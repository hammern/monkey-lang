pub type Statements = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(Identifier, Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Temp,
}

#[derive(Debug, PartialEq)]
pub struct Identifier(pub String);
