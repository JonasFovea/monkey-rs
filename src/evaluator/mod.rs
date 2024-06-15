use anyhow::{bail, Context, Result};

use crate::ast::{BlockStatement, Expression, Identifier, LetStatement, Program, Statement};
use crate::object::{Environment, Object};
use crate::token::{Token, TokenType};

#[cfg(test)]
mod test;

pub fn eval_program(program: Program, env: Environment) -> Result<(Object, Environment)> {
    eval_statements(program.statements, env)
        .context("Evaluating statements of program.")
}

fn eval_statements(stmts: Vec<Statement>, env: Environment) -> Result<(Object, Environment)> {
    let mut last_value = Object::Null;
    let mut env = env;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            let (value, env) = eval_statement(stmt, env)
                .context("Evaluating return statement.")?;
            return Ok((value, env));
        }

        (last_value, env) = eval_statement(stmt, env)
            .context("Evaluating statement.")?;
        if let Object::Return(val) = last_value {
            return Ok((*val, env));
        }
    }
    Ok((last_value, env))
}

fn eval_block_statements(stmts: Vec<Statement>, env: Environment) -> Result<(Object, Environment)> {
    let mut last_value = Object::Null;
    let mut env = env;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            let (obj, env) = eval_statement(stmt, env)
                .context("Evaluating return statement.")?;
            return Ok((
                Object::Return(
                    Box::new(obj)
                ), env)
            );
        }

        (last_value, env) = eval_statement(stmt, env)
            .context("Evaluating statement.")?;
        if let Object::Return(_) = &last_value {
            return Ok((last_value, env));
        }
    }
    Ok((last_value, env))
}

pub fn eval_statement(stmt: Statement, env: Environment) -> Result<(Object, Environment)> {
    match stmt {
        Statement::EXPRESSION(
            exp_stmt) => eval_expression(exp_stmt.expression, env)
            .context("Evaluating expression."),
        Statement::RETURN(ret) => {
            eval_expression(ret.return_value, env)
                .context("Evaluating return value.")
        }
        Statement::LET(LetStatement { token: _, identifier: ident, value: val }) => {
            let (value, mut env) = eval_expression(val, env)
                .context("Evaluating value of let statement.")?;

            env.set(&ident.value, value)
                .context("Setting identifier value.")?;
            
            Ok((Object::Null, env))
        }
    }
}

pub fn eval_expression(exp: Expression, env: Environment) -> Result<(Object, Environment)> {
    match exp {
        Expression::INT_LITERAL(_, i) => Ok((Object::Integer(i), env)),
        Expression::BOOL_LITERAL(_, b) => Ok((native_bool_to_boolean_object(b), env)),
        Expression::PREFIX(op, exp) => {
            let (right, env) = eval_expression(*exp, env)
                .context("Evaluating right expression of prefix operator.")?;

            Ok((eval_prefix_expression(op, right)
                    .context("Evaluating prefix expression.")?, env))
        }
        Expression::INFIX(l, op, r) => {
            let (left, env) = eval_expression(*l, env)
                .context("Evaluating left expression of infix expression.")?;
            let (right, env) = eval_expression(*r, env)
                .context("Evaluating right expression of infix expression.")?;

            Ok((eval_infix_expression(op, left, right)
                    .context("Evaluating infix expression.")?,
                env))
        }
        Expression::IF_EXPRESSION(_, cond, block, alt) => {
            let (res, env) = eval_if_expression(*cond, block, alt, env)
                .context("Evaluating if expression.")?;
            Ok((res, env))
        }
        Expression::IDENT(ident) => {
            let value = env.get(&ident.value)
                .context("Retrieving identifier value from environment.")?;
            Ok((value.clone(), env))
        }
        Expression::FUNCTION(_, params, body) => {
            Ok((Object::Function(params, body, Environment::new_enclosed(env.clone())), env))
        }
        Expression::CALL(_, func, args) => {
            let (function, env) = eval_expression(*func, env)
                .context("Evaluating function expression.")?;
            let (args, env) = eval_expressions(args, env)
                .context("Evaluating function arguments.")?;
            Ok((apply_function(function, args)
                    .context("Evaluating function with arguments from call expression.")?,
                env))
        }
        e => bail!("Unknown expression type: {:?}", e)
    }
}

fn eval_expressions(expressions: Vec<Expression>, env: Environment) -> Result<(Vec<Object>, Environment)> {
    let mut env = env;
    let mut objects = Vec::with_capacity(expressions.len());
    for exp in expressions {
        let (o, e) = eval_expression(exp, env)
            .context("Evaluating one of multiple expressions.")?;
        env = e;
        objects.push(o);
    }
    Ok((objects, env))
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

fn eval_if_expression(condition: Expression, block: BlockStatement, alternative: Option<BlockStatement>, env: Environment) -> Result<(Object, Environment)> {
    let (cond, env) = eval_expression(condition, env)
        .context("Evaluating condition of if expression.")?;

    if is_truthy(cond) {
        return Ok(eval_block_statements(block.statements, env)
            .context("Evaluating block statements of if expression.")?);
    } else if let Some(alt) = alternative {
        return Ok(eval_block_statements(alt.statements, env)
            .context("Evaluating alternative statements of if expression.")?);
    }
    Ok((Object::Null, env))
}

fn native_bool_to_boolean_object(b: bool) -> Object {
    Object::Boolean(b)
}

fn apply_function(func: Object, args: Vec<Object>) -> Result<Object> {
    if let Object::Function(params, body, env) = func {
        let extended_env: Environment = extend_function_env(params, args, env)
            .context("Extending exising environment with parameter/argument pairs.")?;
        // println!("Function environment: \n{extended_env}");
        let (evaluated, _) = eval_block_statements(body.statements, extended_env)
            .context("Evaluating function body.")?;

        return Ok(unwrap_return_value(evaluated));
    } else { bail!("Object {:?} is not a function!", func); }
}

fn extend_function_env(parameters: Vec<Identifier>, arguments: Vec<Object>, environment: Environment) -> Result<Environment> {
    if parameters.len() != arguments.len() {
        bail!(
            "Number of parameters and arguments do not match! Expected: {} Got: {}",
            parameters.len(), 
            arguments.len()
        );
    }
    let mut env = environment;
    for (param, arg) in parameters.iter().zip(arguments) {
        env.set(&param.value, arg)
            .context("Inserting parameter/argument pair into function environment.")?;
    }
    Ok(env)
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::Return(o) => *o,
        o => o
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Boolean(b) => b,
        Object::Integer(i) => i != 0,
        Object::Null => false,
        _ => true
    }
}