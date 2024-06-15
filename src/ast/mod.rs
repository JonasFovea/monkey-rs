mod test;
mod ast_fmt;

use std::fmt;
use std::fmt::Formatter;
use std::str::FromStr;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

use anyhow::{anyhow, bail, Context, Result};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub identifier: Identifier,
    pub value: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnStatement {
    token: Token,
    pub return_value: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub struct BlockStatement {
    token: Token,
    pub statements: Vec<Statement>,
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    token: Token,
    pub value: String,
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(non_camel_case_types, dead_code)]
pub enum Expression {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    LET(LetStatement),
    RETURN(ReturnStatement),
    EXPRESSION(ExpressionStatement),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
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
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            current_token: None,
            peek_token: None
        };
        p.next_token();
        p.next_token();
        p
    }
    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }
    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::new();
        while match &self.current_token {
            Some(tok) => tok.token_type != TokenType::EOF,
            _ => false
        } {
            let stmnt = self.parse_statement()
                .context("Parsing program statement.")?;

            program.statements.push(stmnt);

            self.next_token();
        }
        if program.statements.len() > 0 {
            Ok(program)
        } else { Err(anyhow!("Parsing failed, program does not contain statements: {:?}", &program.statements)) }
    }
    fn parse_statement(&mut self) -> Result<Statement> {
        let tok = self.current_token.clone()
            .with_context(|| format!("Current token not found: {:?}", &self.current_token))?;
        return match tok.token_type {
            TokenType::LET => { self.parse_let_statement() }
            TokenType::RETURN => { self.parse_return_statement() }
            _ => {
                self.parse_expression_statement()
                    .context("Parsing expression statement.")
            }
        };
    }
    fn parse_let_statement(&mut self) -> Result<Statement> {
        let let_token = self.current_token.clone().unwrap();
        self.expect_peek(TokenType::IDENT)
            .context("Missing identifier in let statement.")?;


        let ident_token = self.current_token.clone().unwrap();

        self.expect_peek(TokenType::ASSIGN)
            .context("Missing assign (=) operator in let statement.")?;

        self.next_token();

        let right_expr = self.parse_expression(&Precedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::LET(LetStatement {
            token: let_token,
            identifier: Identifier { token: ident_token.clone(), value: ident_token.literal },
            value: right_expr,
        }))
    }
    fn expect_peek(&mut self, expected_type: TokenType) -> Result<bool> {
        let token = self.peek_token.as_ref()
            .with_context(|| format!("Expected a peek token, but found {:?}", &self.peek_token))?;
        if token.token_type == expected_type {
            self.next_token();
            return Ok(true);
        }
        bail!("Expected peek token does not match: expected {:?}, got {:?}", expected_type, token);
    }
    fn parse_return_statement(&mut self) -> Result<Statement> {
        let return_token = self.current_token.clone().unwrap();
        self.next_token();

        let return_value = self.parse_expression(&Precedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        return Ok(Statement::RETURN(ReturnStatement { token: return_token, return_value }));
    }
    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr_tok = self.current_token.clone()
            .with_context(|| format!("Current token not found: {:?}", &self.current_token))?;
        let expr = self.parse_expression(&Precedence::LOWEST)
            .context("Parsing expression of expression statement.")?;


        let stmt = Statement::EXPRESSION(ExpressionStatement { token: expr_tok, expression: expr });
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Ok(stmt)
    }
    fn parse_expression(&mut self, precedence: &Precedence) -> Result<Expression> {
        let prefix_parse_fn = Parser::get_prefix_parser(&self.current_token)
            .with_context(|| format!("Loading prefix parser for expression with operator: {:?}.", &self.current_token))?;

        if let ParseFunction::PREFIX(func) = prefix_parse_fn {
            let mut left_expr = func(self).context("Parsing left expression with prefix parser.")?;
            while !self.peek_token_is(TokenType::SEMICOLON) && *precedence < self.peek_precedence() {
                let infix_parse_fn = Parser::get_infix_parser(&self.peek_token)
                    .with_context(|| format!("Loading infix parser for expression for operator: {:?}.", &self.current_token))?;

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
        } else { bail!("Loaded parse function is not: ParseFunction::PREFIX got instead: {:?}", prefix_parse_fn) }
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
    fn parse_identifier(&mut self) -> Result<Expression> {
        Ok(Expression::IDENT(
            Identifier {
                token: self.current_token.clone()
                    .with_context(|| format!("Current token missing: {:?}", &self.current_token))?,
                value: self.current_token.clone().unwrap().literal,
            }))
    }
    fn parse_integer_literal(&mut self) -> Result<Expression> {
        let val = i64::from_str(self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?
            .literal.as_str()).unwrap();
        Ok(Expression::INT_LITERAL(self.current_token.clone().unwrap(), val))
    }
    fn parse_boolean(&mut self) -> Result<Expression> {
        let token = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;
        let token_type = &token.token_type;
        match token_type {
            TokenType::FALSE => { Ok(Expression::BOOL_LITERAL(token, false)) }
            TokenType::TRUE => { Ok(Expression::BOOL_LITERAL(token, true)) }
            _ => { bail!("Invalid token found: {:?}", &token) }
        }
    }
    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let operator = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;
        self.next_token();
        let expression = self.parse_expression(&Precedence::PREFIX)
            .context("Parsing inner expression of prefix expression")?;
        return Ok(Expression::PREFIX(operator, Box::new(expression)));
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
    fn parse_infix_expression(&mut self, left_expression: &Expression) -> Result<Expression> {
        let current_token = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;
        let precedence = self.current_precedence();
        self.next_token();
        let right_expression = self.parse_expression(&precedence)
            .context("Parsing right expression of infix expression.")?;
        Ok(Expression::INFIX(Box::new(left_expression.clone()), current_token, Box::new(right_expression)))
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
    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let exp = self.parse_expression(&Precedence::LOWEST)
            .context("Parsing inner expression of group.")?;
        self.expect_peek(TokenType::RPAREN)
            .context("Peeking right parenthesis of grouped expression.")?;
        Ok(exp)
    }
    fn parse_if_expression(&mut self) -> Result<Expression> {
        let token = self.current_token.clone().unwrap();
        self.expect_peek(TokenType::LPAREN)
            .context("Peeking left parenthesis of if expression.")?;
        self.next_token();
        let condition = self.parse_expression(&Precedence::LOWEST)
            .context("Parsing conditional expression of if expression.")?;

        self.expect_peek(TokenType::RPAREN)
            .context("Peeking right parenthesis of if expression.")?;

        self.expect_peek(TokenType::LBRACE)
            .context("Peeking left brace of if expression block statement")?;

        let consequence = self.parse_block_statement()
            .context("Parsing block statement of if expression.")?;

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            self.expect_peek(TokenType::LBRACE)
                .context("Peeking left brace of if expression alternative block statement")?;
            let alternative = self.parse_block_statement()
                .context("Parsing alternative block statement of if expression.")?;
            return Ok(Expression::IF_EXPRESSION(token, Box::new(condition), consequence, Some(alternative)));
        }

        Ok(Expression::IF_EXPRESSION(token, Box::new(condition), consequence, None))
    }
    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let token = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;
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
    fn parse_function_literal(&mut self) -> Result<Expression> {
        let tok = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;

        self.expect_peek(TokenType::LPAREN)
            .context("Peeking left parenthesis of function literal.")?;

        let parameters = self.parse_function_parameters()
            .context("Parsing parameters of function literal.")?;

        self.expect_peek(TokenType::LBRACE)
            .context("Peeking left brace of function block statement.")?;

        let body = self.parse_block_statement()
            .context("Parsing block statement of function.")?;

        Ok(Expression::FUNCTION(tok, parameters, body))
    }
    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
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
            if self.current_token.is_none() { bail!("Current token is missing: {:?}", &self.current_token); }
            if !self.current_token_is(&TokenType::IDENT) { bail!("Expected another identifier after comma, got {:?} instead.", &self.current_token.clone().unwrap()) }
            let ident = Identifier { token: self.current_token.clone().unwrap(), value: self.current_token.clone().unwrap().literal };
            identifiers.push(ident);
        }

        self.expect_peek(TokenType::RPAREN)
            .context("Peeking right parenthesis of function parameter list.")?;

        Ok(identifiers)
    }
    fn parse_call_expression(&mut self, left: &Expression) -> Result<Expression> {
        let tok = self.current_token.clone()
            .with_context(|| format!("Current token missing: {:?}", &self.current_token))?;
        let args = self.parse_call_arguments()
            .context("Parsing arguments of function call.")?;
        Ok(Expression::CALL(tok, Box::new(left.clone()), args))
    }
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Ok(Vec::new());
        }
        self.next_token();
        let mut args = Vec::new();
        let first = self.parse_expression(&Precedence::LOWEST)
            .context("Parsing first expression in argument list.")?;
        args.push(first);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let next = self.parse_expression(&Precedence::LOWEST)
                .context("Parsing expression in argument list.")?;
            args.push(next);
        }

        self.expect_peek(TokenType::RPAREN)
            .context("Peeking right parenthesis of function call.")?;

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
    PREFIX(Box<dyn Fn(&mut Parser) -> Result<Expression>>),
    INFIX(Box<dyn Fn(&mut Parser, &Expression) -> Result<Expression>>),
}

impl fmt::Debug for ParseFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            ParseFunction::PREFIX(_) => "ParseFunction::PREFIX",
            ParseFunction::INFIX(_) => "ParseFunction::INFIX"
        })
    }
}
