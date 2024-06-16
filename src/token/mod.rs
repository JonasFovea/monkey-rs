use std::fmt;
use std::fmt::Formatter;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
#[allow(dead_code)]
pub enum TokenType {
    /// # TokenType Enumeration
    /// represents all kinds of tokens.

    ILLEGAL,
    EOF,
    
    IDENT,
    INT,
    STRING,

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,

    EQ,
    NEQ,
    LEQ,
    GEQ,
    
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    IF,
    ELSE,
    TRUE,
    FALSE,
    RETURN,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::ASSIGN => "ASSIGN",
            TokenType::PLUS => "PLUS",
            TokenType::MINUS => "MINUS",
            TokenType::BANG => "BANG",
            TokenType::ASTERISK => "ASTERISK",
            TokenType::SLASH => "SLASH",
            TokenType::LT => "LT",
            TokenType::GT => "GT",
            TokenType::EQ => "EQ",
            TokenType::NEQ => "NEQ",
            TokenType::LEQ => "LEQ",
            TokenType::GEQ => "GEQ",
            TokenType::COMMA => "COMMA",
            TokenType::SEMICOLON => "SEMICOLON",
            TokenType::LPAREN => "LPAREN",
            TokenType::RPAREN => "RPAREN",
            TokenType::LBRACE => "LBRACE",
            TokenType::RBRACE => "RBRACE",
            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::FALSE => "FALSE",
            TokenType::TRUE => "TRUE",
            TokenType::RETURN => "RETURN",
            TokenType::STRING => "STRING",
            TokenType::LBRACKET => "LBRACKET",
            TokenType::RBRACKET => "RBRACKET",
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.literal)
    }
}

impl Token {
    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Token { token_type, literal: literal.to_string() }
    }
}