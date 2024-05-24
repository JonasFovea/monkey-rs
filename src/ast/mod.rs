mod test;
mod ast_fmt;

use std::str::FromStr;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
struct LetStatement {
    token: Token,
    identifier: Identifier,
    value: Expression,
}

#[derive(Debug, Clone)]
struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

#[derive(Debug, Clone)]
struct ExpressionStatement {
    token: Token,
    expression: Expression,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}


#[derive(Debug, Clone)]
struct Identifier {
    token: Token,
    value: String,
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, dead_code)]
enum Expression {
    IDENT(Identifier),
    INT_LITERAL(Token, i64),
    BOOL_LITERAL(Token, bool),
    PREFIX(Token, Box<Expression>),
    INFIX(Box<Expression>, Token, Box<Expression>),
    IF_EXPRESSION(Token, Box<Expression>, BlockStatement, Option<BlockStatement>),
    FUNCTION(Token, Vec<Identifier>, BlockStatement),
    CALL(Token, Box<Expression>, Vec<Expression>),
    None,
}

#[derive(Debug, Clone)]
enum Statement {
    LET(LetStatement),
    RETURN(ReturnStatement),
    EXPRESSION(ExpressionStatement),
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: Vec::new() }
    }
}

pub struct Parser {
    lexer: Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    parsing_errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            parsing_errors: Vec::new(),
        };
        p.next_token();
        p.next_token();
        p
    }
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }
    pub fn parse_program(&mut self) -> Result<Program, &'static str> {
        let mut program = Program::new();
        while match &self.current_token {
            Some(tok) => tok.token_type != TokenType::EOF,
            _ => false
        } {
            let stmnt = self.parse_statement();
            if let Ok(s) = stmnt {
                program.statements.push(s);
            }
            self.next_token();
        }
        if program.statements.len() > 0 {
            Ok(program)
        } else { Err("Parsing failed!") }
    }
    fn parse_statement(&mut self) -> Result<Statement, &str> {
        if let Some(tok) = &self.current_token {
            match tok.token_type {
                TokenType::LET => { return self.parse_let_statement(); }
                TokenType::RETURN => { return self.parse_return_statement(); }
                _ => { return self.parse_expression_statement(); }
            }
        }
        Err("not able to parse statement ")
    }
    fn parse_let_statement(&mut self) -> Result<Statement, &'static str> {
        let let_token = self.current_token.clone().unwrap();
        if !self.expect_peek(TokenType::IDENT) {
            return Err("unable to find following identifier TokenType::IDENT");
        }

        let ident_token = self.current_token.clone().unwrap();

        if !self.expect_peek(TokenType::ASSIGN) {
            return Err("Missing assignment operator (=)!");
        }
        self.next_token();

        let right_expr = self.parse_expression(&Precedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::LET(LetStatement { 
            token: let_token, 
            identifier: Identifier { token: ident_token.clone(), value: ident_token.literal }, 
            value: right_expr }))
    }
    fn expect_peek(&mut self, expected_type: TokenType) -> bool {
        if let Some(token) = self.peek_token.as_ref() {
            if token.token_type == expected_type {
                self.next_token();
                return true;
            }
            self.parsing_errors.push(format!("Expected peek {:?}, but found {:?}", expected_type, token.token_type));
        } else {
            self.parsing_errors.push(format!("Expected peek {:?}, no next token found!", expected_type));
        }
        false
    }
    fn parse_return_statement(&mut self) -> Result<Statement, &'static str> {
        let return_token = self.current_token.clone().unwrap();
        self.next_token();

        let return_value = self.parse_expression(&Precedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(Statement::RETURN(ReturnStatement { token: return_token, return_value }));
    }
    fn parse_expression_statement(&mut self) -> Result<Statement, &str> {
        let expr_tok = self.current_token.clone().unwrap();
        let expr = self.parse_expression(&Precedence::LOWEST);
        if let Err(_) = expr {
            return Err("Failed to parse expression!");
        }


        let stmt = Statement::EXPRESSION(ExpressionStatement { token: expr_tok, expression: expr.unwrap() });
        if let Some(tok) = &self.peek_token {
            if tok.token_type == TokenType::SEMICOLON {
                self.next_token();
            }
        }
        Ok(stmt)
    }
    fn parse_expression(&mut self, precendence: &Precedence) -> Result<Expression, &'static str> {
        let prefix_fn = Parser::get_prefix_parser(&self.current_token);
        if prefix_fn.is_none() {
            return Err("Couldn't find prefix function");
        }
        let prefix_parse_fn = prefix_fn.unwrap();

        if let ParseFunction::PREFIX(func) = prefix_parse_fn {
            if let Ok(mut left_expr) = func(self) {
                while !self.peek_token_is(TokenType::SEMICOLON) && *precendence < self.peek_precedence() {
                    let infix_fn = Parser::get_infix_parser(&self.peek_token);
                    if infix_fn.is_none() {
                        return Ok(left_expr);
                    }
                    let infix_parse_fn = infix_fn.unwrap();
                    if let ParseFunction::INFIX(inf_func) = infix_parse_fn {
                        self.next_token();
                        left_expr = if let Ok(le) = inf_func(self, &left_expr) {
                            le
                        } else {
                            left_expr
                        }
                    }
                }
                return Ok(left_expr);
            }
        }
        Err("Couldn't parse expression")
    }
    fn peek_token_is(&self, token_type: TokenType) -> bool {
        if let Some(token) = &self.peek_token {
            if token.token_type == token_type {
                return true;
            }
        }
        false
    }
    fn get_prefix_parser(token: &Option<Token>) -> Option<ParseFunction> {
        if token.is_none() { return None; };
        let token_type = token.clone().unwrap().token_type;
        match token_type {
            TokenType::IDENT => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_identifier))) }
            TokenType::INT => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_integer_literal))) }
            TokenType::BANG => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_prefix_expression))) }
            TokenType::MINUS => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_prefix_expression))) }
            TokenType::TRUE | TokenType::FALSE => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_boolean))) }
            TokenType::LPAREN => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_grouped_expression))) }
            TokenType::IF => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_if_expression))) }
            TokenType::FUNCTION => { Some(ParseFunction::PREFIX(Box::new(Parser::parse_function_literal))) }
            _ => None
        }
    }
    fn parse_identifier(&mut self) -> Result<Expression, &'static str> {
        Ok(Expression::IDENT(
            Identifier {
                token: self.current_token.clone().unwrap(),
                value: self.current_token.clone().unwrap().literal,
            }))
    }
    fn parse_integer_literal(&mut self) -> Result<Expression, &'static str> {
        let val = i64::from_str(self.current_token.clone().unwrap().literal.as_str()).unwrap();
        Ok(Expression::INT_LITERAL(self.current_token.clone().unwrap(), val))
    }
    fn parse_boolean(&mut self) -> Result<Expression, &'static str> {
        if self.current_token.is_none() { return Err("No current token found"); }

        let token = self.current_token.clone().unwrap();
        let token_type = &token.token_type;
        match token_type {
            TokenType::FALSE => { Ok(Expression::BOOL_LITERAL(token, false)) }
            TokenType::TRUE => { Ok(Expression::BOOL_LITERAL(token, true)) }
            _ => { Err("Invalid token found") }
        }
    }
    fn parse_prefix_expression(&mut self) -> Result<Expression, &'static str> {
        let operator = self.current_token.clone().unwrap();
        self.next_token();
        if let Ok(expression) = self.parse_expression(&Precedence::PREFIX) {
            return Ok(Expression::PREFIX(operator, Box::new(expression)));
        }
        Err("Not able to parse inner expression!")
    }
    fn peek_precedence(&self) -> Precedence {
        if let Some(tok) = &self.peek_token {
            return Precedence::from_token(tok);
        }
        Precedence::LOWEST
    }
    fn current_precedence(&self) -> Precedence {
        if let Some(tok) = &self.current_token {
            return Precedence::from_token(tok);
        }
        Precedence::LOWEST
    }
    fn parse_infix_expression(&mut self, left_expression: &Expression) -> Result<Expression, &'static str> {
        let current_token = self.current_token.clone().unwrap();
        let precedence = self.current_precedence();
        self.next_token();
        return if let Ok(right_expression) = self.parse_expression(&precedence) {
            Ok(Expression::INFIX(Box::new(left_expression.clone()), current_token, Box::new(right_expression)))
        } else {
            Err("Failed to parse right side of infix expression")
        };
    }
    fn get_infix_parser(token: &Option<Token>) -> Option<ParseFunction> {
        if token.is_none() { return None; };
        let token_type = token.clone().unwrap().token_type;
        match token_type {
            TokenType::PLUS |
            TokenType::MINUS |
            TokenType::SLASH |
            TokenType::ASTERISK |
            TokenType::EQ |
            TokenType::NEQ |
            TokenType::GEQ |
            TokenType::LEQ |
            TokenType::GT |
            TokenType::LT => Some(ParseFunction::INFIX(Box::new(Parser::parse_infix_expression))),
            TokenType::LPAREN => Some(ParseFunction::INFIX(Box::new(Parser::parse_call_expression))),
            _ => None
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, &'static str> {
        self.next_token();
        let exp = self.parse_expression(&Precedence::LOWEST).clone();
        if !self.expect_peek(TokenType::RPAREN) {
            return Err("Missing right parenthesis!");
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Result<Expression, &'static str> {
        let token = self.current_token.clone().unwrap();
        if !self.expect_peek(TokenType::LPAREN) {
            return Err("Missing left parenthesis");
        }
        self.next_token();
        let condition = self.parse_expression(&Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::RPAREN) {
            return Err("Missing right parenthesis");
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return Err("Missing left brace");
        }

        let consequence = self.parse_block_statement()?;

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenType::LBRACE) {
                return Err("Missing left brace for else statement");
            }
            let alternative = self.parse_block_statement()?;
            return Ok(Expression::IF_EXPRESSION(token, Box::new(condition), consequence, Some(alternative)));
        }

        Ok(Expression::IF_EXPRESSION(token, Box::new(condition), consequence, None))
    }
    fn parse_block_statement(&mut self) -> Result<BlockStatement, &'static str> {
        let token = self.current_token.clone().unwrap();
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_token_is(&TokenType::RBRACE) && !self.current_token_is(&TokenType::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        Ok(BlockStatement { token, statements })
    }
    fn current_token_is(&self, token_type: &TokenType) -> bool {
        if let Some(tok) = &self.current_token {
            return tok.token_type == *token_type;
        }
        false
    }

    fn parse_function_literal(&mut self) -> Result<Expression, &'static str> {
        let tok = self.current_token.clone().unwrap();

        if !self.expect_peek(TokenType::LPAREN) {
            return Err("Missing left parenthesis of function parameters");
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBRACE) {
            return Err("Missing left brace of function block");
        }

        let body = self.parse_block_statement()?;

        Ok(Expression::FUNCTION(tok, parameters, body))
    }
    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, &'static str> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let first = Identifier { token: self.current_token.clone().unwrap(), value: self.current_token.clone().unwrap().literal };
        identifiers.push(first);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier { token: self.current_token.clone().unwrap(), value: self.current_token.clone().unwrap().literal };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return Err("Missing closing right parenthesis of parameter list");
        }

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, left: &Expression) -> Result<Expression, &'static str> {
        let tok = self.current_token.clone().unwrap();
        let args = self.parse_call_arguments()?;
        Ok(Expression::CALL(tok, Box::new(left.clone()), args))
    }
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, &'static str> {
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(Vec::new());
        }
        self.next_token();
        let mut args = Vec::new();
        let first = self.parse_expression(&Precedence::LOWEST)?;
        args.push(first);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let next = self.parse_expression(&Precedence::LOWEST)?;
            args.push(next);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return Err("Missing right parenthesis for function call!");
        }

        Ok(args)
    }
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST = 00,
    EQUALS = 10,
    LESSGREATER = 20,
    SUM = 30,
    PRODUCT = 40,
    PREFIX = 50,
    CALL = 60,
}

impl Precedence {
    fn from_token(token: &Token) -> Self {
        match token.token_type {
            TokenType::EQ | TokenType::NEQ | TokenType::LEQ | TokenType::GEQ => Precedence::EQUALS,
            TokenType::LT | TokenType::GT => Precedence::LESSGREATER,
            TokenType::PLUS | TokenType::MINUS => Precedence::SUM,
            TokenType::ASTERISK | TokenType::SLASH => Precedence::PRODUCT,
            TokenType::LPAREN => Precedence::CALL,
            _ => Precedence::LOWEST
        }
    }
}

enum ParseFunction {
    PREFIX(Box<dyn Fn(&mut Parser) -> Result<Expression, &'static str>>),
    INFIX(Box<dyn Fn(&mut Parser, &Expression) -> Result<Expression, &'static str>>),
}
