pub enum Token {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i32),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}

