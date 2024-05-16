use std::collections::HashMap;
use lazy_static::lazy_static;
use crate::token::{Token, TokenType};
use crate::token::TokenType::*;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

lazy_static! {
    static ref KEYS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("fn", FUNCTION);
        m.insert("let", LET);
        m.insert("if", IF);
        m.insert("else", ELSE);
        m.insert("return", RETURN);
        m.insert("true", TRUE);
        m.insert("false", FALSE);
        m
    };
}

impl Lexer {
    pub fn new(input: &str) -> Result<Self, &str> {
        if input.len() < 1 {
            Err("Input is empty!")
        } else {
            Ok(Lexer { input: input.chars().collect(), position: 0, read_position: 1, ch: input.chars().next().unwrap() })
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = char::from(0);
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => self.make_two_char_token(ASSIGN, '=', EQ),
            '+' => Token::new(PLUS, "+"),
            '-' => Token::new(MINUS, "-"),
            '!' => self.make_two_char_token(BANG, '=', NEQ),
            '/' => Token::new(SLASH, "/"),
            '*' => Token::new(ASTERISK, "*"),
            '<' => self.make_two_char_token(LT, '=', LEQ),
            '>' => self.make_two_char_token(GT, '=', GEQ),
            ';' => Token::new(SEMICOLON, ";"),
            '(' => Token::new(LPAREN, "("),
            ')' => Token::new(RPAREN, ")"),
            ',' => Token::new(COMMA, ","),
            '{' => Token::new(LBRACE, "{"),
            '}' => Token::new(RBRACE, "}"),
            '\0' => Token::new(EOF, ""),
            c => {
                if is_letter(c) {
                    let literal = self.read_identifier();
                    return Token::new(lookup_ident(&literal), &literal);
                } else if c.is_numeric() {
                    return Token::new(INT, &self.read_number());
                } else { Token::new(ILLEGAL, &c.to_string()) }
            }
        };
        self.read_char();
        return token;
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        String::from_iter(self.input[position..self.position].iter())
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }
        String::from_iter(self.input[position..self.position].iter())
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn make_two_char_token(&mut self, default_token: TokenType, expected_literal: char, two_char_token: TokenType) -> Token {
        let first = self.ch;
        if self.peek_char() == expected_literal {
            self.read_char();
            let second = self.ch;
            let mut literal = String::new();
            literal.push(first);
            literal.push(second);
            Token::new(two_char_token, &literal)
        } else { Token::new(default_token, &first.to_string()) }
    }
}

fn is_letter(c: char) -> bool {
    (c.is_alphabetic() || c == '_') && !c.is_whitespace()
}

fn lookup_ident(ident: &str) -> TokenType {
    if let Some(identified_token) = KEYS.get(ident) {
        return identified_token.clone();
    }
    IDENT
}


#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_next_token() {
        let input = 
            "let five = 5;\
            let ten = 10;\
            let add = fn(x, y) {\
                x + y;\
            };\
            let result = add(five, ten);\
            !-/*5;\
            5 < 10 > 5;\
            \
            if (5 < 10) {\
                return true;\
            } else {\
                return false;\
            }\
            \
            10 == 10;\
            10 != 9;\
            10 >= 8;\
            10 <= 11;";
        let tests = vec![
            Token::new(LET, "let"),
            Token::new(IDENT, "five"),
            Token::new(ASSIGN, "="),
            Token::new(INT, "5"),
            Token::new(SEMICOLON, ";"),
            Token::new(LET, "let"),
            Token::new(IDENT, "ten"),
            Token::new(ASSIGN, "="),
            Token::new(INT, "10"),
            Token::new(SEMICOLON, ";"),
            Token::new(LET, "let"),
            Token::new(IDENT, "add"),
            Token::new(ASSIGN, "="),
            Token::new(FUNCTION, "fn"),
            Token::new(LPAREN, "("),
            Token::new(IDENT, "x"),
            Token::new(COMMA, ","),
            Token::new(IDENT, "y"),
            Token::new(RPAREN, ")"),
            Token::new(LBRACE, "{"),
            Token::new(IDENT, "x"),
            Token::new(PLUS, "+"),
            Token::new(IDENT, "y"),
            Token::new(SEMICOLON, ";"),
            Token::new(RBRACE, "}"),
            Token::new(SEMICOLON, ";"),
            Token::new(LET, "let"),
            Token::new(IDENT, "result"),
            Token::new(ASSIGN, "="),
            Token::new(IDENT, "add"),
            Token::new(LPAREN, "("),
            Token::new(IDENT, "five"),
            Token::new(COMMA, ","),
            Token::new(IDENT, "ten"),
            Token::new(RPAREN, ")"),
            Token::new(SEMICOLON, ";"),
            Token::new(BANG, "!"),
            Token::new(MINUS, "-"),
            Token::new(SLASH, "/"),
            Token::new(ASTERISK, "*"),
            Token::new(INT, "5"),
            Token::new(SEMICOLON, ";"),
            Token::new(INT, "5"),
            Token::new(LT, "<"),
            Token::new(INT, "10"),
            Token::new(GT, ">"),
            Token::new(INT, "5"),
            Token::new(SEMICOLON, ";"),
            Token::new(IF, "if"),
            Token::new(LPAREN, "("),
            Token::new(INT, "5"),
            Token::new(LT, "<"),
            Token::new(INT, "10"),
            Token::new(RPAREN, ")"),
            Token::new(LBRACE, "{"),
            Token::new(RETURN, "return"),
            Token::new(TRUE, "true"),
            Token::new(SEMICOLON, ";"),
            Token::new(RBRACE, "}"),
            Token::new(ELSE, "else"),
            Token::new(LBRACE, "{"),
            Token::new(RETURN, "return"),
            Token::new(FALSE, "false"),
            Token::new(SEMICOLON, ";"),
            Token::new(RBRACE, "}"),
            Token::new(INT, "10"),
            Token::new(EQ, "=="),
            Token::new(INT, "10"),
            Token::new(SEMICOLON, ";"),
            Token::new(INT, "10"),
            Token::new(NEQ, "!="),
            Token::new(INT, "9"),
            Token::new(SEMICOLON, ";"),
            Token::new(INT, "10"),
            Token::new(GEQ, ">="),
            Token::new(INT, "8"),
            Token::new(SEMICOLON, ";"),
            Token::new(INT, "10"),
            Token::new(LEQ, "<="),
            Token::new(INT, "11"),
            Token::new(SEMICOLON, ";"),
            Token::new(EOF, ""),
        ];

        let mut lexer = Lexer::new(input).unwrap();

        for expected in tests {
            let token = lexer.next_token();

            // println!("Comparing: expected_{:?}\tactual_{:?}", &expected, &token);
            assert_eq!(expected, token);
        }
    }
}