use anyhow::{bail, Context, Result};

use crate::ast::{BlockStatement, Expression, Program, Statement};
use crate::object::Object;
use crate::token::{Token, TokenType};

#[cfg(test)]
mod test;

pub fn eval_program(program: Program) -> Result<Object> {
    eval_statements(program.statements)
        .context("Evaluating statements of program.")
}

fn eval_statements(stmts: Vec<Statement>) -> Result<Object> {
    let mut last_value = Object::Null;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            let value = eval_statement(stmt)
                .context("Evaluating return statement.")?;
            return Ok(value);
        }

        last_value = eval_statement(stmt)
            .context("Evaluating statement.")?;
        if let Object::Return(val) = last_value {
            return Ok(*val)
        }
    }
    Ok(last_value)
}

fn eval_block_statements(stmts: Vec<Statement>) -> Result<Object> {
    let mut last_value = Object::Null;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            return Ok(
                Object::Return(
                    Box::new(
                        eval_statement(stmt)
                            .context("Evaluating return statement.")?
                    )
                )
            );
        }

        last_value = eval_statement(stmt)
            .context("Evaluating statement.")?;
        if let Object::Return(_) = &last_value {
            return Ok(last_value);
        }
    }
    Ok(last_value)
}

pub fn eval_statement(stmt: Statement) -> Result<Object> {
    match stmt {
        Statement::EXPRESSION(
            exp_stmt) => eval_expression(exp_stmt.expression)
            .context("Evaluating expression."),
        Statement::RETURN(ret) => {
            eval_expression(ret.return_value)
                .context("Evaluating return value.")
        }
        _ => Ok(Object::Null)
    }
}

pub fn eval_expression(exp: Expression) -> Result<Object> {
    match exp {
        Expression::INT_LITERAL(_, i) => Ok(Object::Integer(i)),
        Expression::BOOL_LITERAL(_, b) => Ok(native_bool_to_boolean_object(b)),
        Expression::PREFIX(op, exp) => {
            let right = eval_expression(*exp).context("Evaluating right expression of prefix operator.")?;

            eval_prefix_expression(op, right)
                .context("Evaluating prefix expression.")
        }
        Expression::INFIX(l, op, r) => {
            let left = eval_expression(*l)
                .context("Evaluating left expression of infix expression.")?;
            let right = eval_expression(*r)
                .context("Evaluating right expression of infix expression.")?;

            eval_infix_expression(op, left, right)
                .context("Evaluating infix expression.")
        }
        Expression::IF_EXPRESSION(_, cond, block, alt) => {
            eval_if_expression(*cond, block, alt)
                .context("Evaluating if expression.")
        }
        e => bail!("Unknown expression type: {:?}", e)
    }
}

fn eval_prefix_expression(operator: Token, right: Object) -> Result<Object> {
    match operator.token_type {
        TokenType::BANG => eval_bang_expression(right)
            .context("Evaluating prefix bang expression."),
        TokenType::MINUS => eval_minus_prefix_expression(right)
            .context("Evaluating prefix minus expression."),
        t => bail!("Unknown prefix operator type: {}", t)
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Result<Object> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, l, r)
                .context("Evaluating infix expression for two integers.")
        }
        (Object::Boolean(l), Object::Boolean(r)) => {
            eval_boolean_infix_expression(operator, l, r)
                .context("Evaluating infix expression for two booleans.")
        }
        (l, r) => bail!("No infix operator defined for objects {:?} and {:?}", l, r)
    }
}

fn eval_bang_expression(right: Object) -> Result<Object> {
    match right {
        Object::Boolean(b) => Ok(Object::Boolean(!b)),
        Object::Integer(i) => Ok(Object::Boolean(i == 0)),
        r => bail!("Bang operator undefined for {:?}", r)
    }
}

fn eval_minus_prefix_expression(right: Object) -> Result<Object> {
    match right {
        Object::Integer(i) => Ok(Object::Integer(-i)),
        r => bail!("Prefix minus operator undefined for {:?}", r)
    }
}

fn eval_integer_infix_expression(operator: Token, left: i64, right: i64) -> Result<Object> {
    match operator.token_type {
        TokenType::PLUS => { Ok(Object::Integer(left + right)) }
        TokenType::MINUS => { Ok(Object::Integer(left - right)) }
        TokenType::ASTERISK => { Ok(Object::Integer(left * right)) }
        TokenType::SLASH => {
            if right == 0 { return Ok(Object::Null); };
            Ok(Object::Integer(left / right))
        }
        TokenType::LT => { Ok(native_bool_to_boolean_object(left < right)) }
        TokenType::GT => { Ok(native_bool_to_boolean_object(left > right)) }
        TokenType::EQ => { Ok(native_bool_to_boolean_object(left == right)) }
        TokenType::NEQ => { Ok(native_bool_to_boolean_object(left != right)) }
        TokenType::LEQ => { Ok(native_bool_to_boolean_object(left <= right)) }
        TokenType::GEQ => { Ok(native_bool_to_boolean_object(left >= right)) }
        t => bail!("Unknown infix operator for two integers: {}", t)
    }
}

fn eval_boolean_infix_expression(operator: Token, left: bool, right: bool) -> Result<Object> {
    match operator.token_type {
        TokenType::EQ => Ok(native_bool_to_boolean_object(left == right)),
        TokenType::NEQ => Ok(native_bool_to_boolean_object(left != right)),
        t => bail!("Unknown infix operator for two booleans: {}", t)
    }
}

fn eval_if_expression(condition: Expression, block: BlockStatement, alternative: Option<BlockStatement>) -> Result<Object> {
    let cond = eval_expression(condition).context("Evaluating condition of if expression.")?;

    if is_truthy(cond) {
        return Ok(eval_block_statements(block.statements)
            .context("Evaluating block statements of if expression.")?);
    } else if let Some(alt) = alternative {
        return Ok(eval_block_statements(alt.statements)
            .context("Evaluating alternative statements of if expression.")?);
    }
    Ok(Object::Null)
}

fn native_bool_to_boolean_object(b: bool) -> Object {
    Object::Boolean(b)
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Boolean(b) => b,
        Object::Integer(i) => i != 0,
        Object::Null => false,
        _ => true
    }
}