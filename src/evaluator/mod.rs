use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::ast::{BlockStatement, Expression, Identifier, LetStatement, Program, Statement};
use crate::object::{Environment, get_builtin_function, HashKey, Object};
use crate::token::{Token, TokenType};

#[cfg(test)]
mod test;

pub fn eval_program(program: Program, env: Rc<Mutex<Environment>>) -> Result<Object> {
    eval_statements(program.statements, env)
        .context("Evaluating statements of program.")
}

fn eval_statements(stmts: Vec<Statement>, env: Rc<Mutex<Environment>>) -> Result<Object> {
    let mut last_value = Object::Null;
    let env = env;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            let value = eval_statement(stmt, env.clone())
                .context("Evaluating return statement.")?;
            return Ok(value);
        }

        last_value = eval_statement(stmt, env.clone())
            .context("Evaluating statement.")?;
        if let Object::Return(val) = last_value {
            return Ok(*val);
        }
    }
    Ok(last_value)
}

fn eval_block_statements(stmts: Vec<Statement>, env: Rc<Mutex<Environment>>) -> Result<Object> {
    let mut last_value = Object::Null;
    let env = env;
    for stmt in stmts {
        if let Statement::RETURN(_) = &stmt {
            let obj = eval_statement(stmt, env.clone())
                .context("Evaluating return statement.")?;
            return Ok(
                Object::Return(
                    Box::new(obj)
                )
            );
        }

        last_value = eval_statement(stmt, env.clone())
            .context("Evaluating statement.")?;
        if let Object::Return(_) = &last_value {
            return Ok(last_value);
        }
    }
    Ok(last_value)
}

pub fn eval_statement(stmt: Statement, env: Rc<Mutex<Environment>>) -> Result<Object> {
    match stmt {
        Statement::EXPRESSION(
            exp_stmt) => eval_expression(exp_stmt.expression, env.clone())
            .context("Evaluating expression."),
        Statement::RETURN(ret) => {
            eval_expression(ret.return_value, env.clone())
                .context("Evaluating return value.")
        }
        Statement::LET(LetStatement { token: _, identifier: ident, value: val }) => {
            let value = eval_expression(val, env.clone())
                .context("Evaluating value of let statement.")?;

            env.lock().unwrap().set(&ident.value, value)
                .context("Setting identifier value.")?;

            Ok(Object::Null)
        }
    }
}

pub fn eval_expression(exp: Expression, env: Rc<Mutex<Environment>>) -> Result<Object> {
    match exp {
        Expression::INT_LITERAL(_, i) => Ok(Object::Integer(i)),
        Expression::BOOL_LITERAL(_, b) => Ok(native_bool_to_boolean_object(b)),
        Expression::STRING_LITERAL(_, s) => Ok(Object::String(s)),
        Expression::ARRAY_LITERAL(_, elems) => {
            let elements = eval_expressions(elems, env.clone())
                .context("Evaluating array elements.")?;
            Ok(Object::Array(elements))
        }
        Expression::HASH_LITERAL(_, _, _) => {
            eval_hash_literal(exp, env.clone())
                .context("Evaluating hash literal.")
        }
        Expression::PREFIX(op, exp) => {
            let right = eval_expression(*exp, env.clone())
                .context("Evaluating right expression of prefix operator.")?;

            Ok(eval_prefix_expression(op, right)
                .context("Evaluating prefix expression.")?)
        }
        Expression::INFIX(l, op, r) => {
            let left = eval_expression(*l, env.clone())
                .context("Evaluating left expression of infix expression.")?;
            let right = eval_expression(*r, env.clone())
                .context("Evaluating right expression of infix expression.")?;

            Ok(eval_infix_expression(op, left, right)
                .context("Evaluating infix expression.")?
            )
        }
        Expression::IF_EXPRESSION(_, cond, block, alt) => {
            let res = eval_if_expression(*cond, block, alt, env.clone())
                .context("Evaluating if expression.")?;
            Ok(res)
        }
        Expression::IDENT(ident) => {
            let value = env.lock().unwrap().get(&ident.value)
                .context("Retrieving identifier value from environment.");
            if value.is_err() {
                if let Ok(_) = get_builtin_function(&ident.value)
                    .context("Retrieving identifier from builtin functions") {
                    Ok(Object::Builtin(ident.value.clone()))
                } else { bail!("Identifier not found: {}", &ident) }
            } else { Ok(value.unwrap().clone()) }
        }
        Expression::FUNCTION(_, params, body, _) => {
            Ok(Object::Function(params, body, Rc::new(Mutex::new(Environment::new_enclosed(env.clone())))))
        }
        Expression::CALL(_, func, args) => {
            let function = eval_expression(*func, env.clone())
                .context("Evaluating function expression.")?;
            let args = eval_expressions(args, env.clone())
                .context("Evaluating function arguments.")?;
            Ok(apply_function(function, args)
                .context("Evaluating function with arguments from call expression.")?)
        }
        Expression::INDEX_EXPRESSION(_, left, idx) => {
            let left = eval_expression(*left, env.clone()).context("Evaluating left side of index expression.")?;
            let idx = eval_expression(*idx, env.clone()).context("Evaluating index value of index expression.")?;
            eval_index_expression(left, idx).context("Evaluating index expression.")
        }
        e => bail!("Unknown expression type: {:?}", e)
    }
}

fn eval_expressions(expressions: Vec<Expression>, env: Rc<Mutex<Environment>>) -> Result<Vec<Object>> {
    let mut objects = Vec::with_capacity(expressions.len());
    for exp in expressions {
        let o = eval_expression(exp, env.clone())
            .context("Evaluating one of multiple expressions.")?;
        objects.push(o);
    }
    Ok(objects)
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
        (Object::String(l), Object::String(r)) => {
            eval_string_infix_expression(operator, l, r)
                .context("Evaluating infix expression for two strings.")
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
        TokenType::PLUS => {
            Ok(Object::Integer(left.checked_add(right)
                .context("Addition overflowed!")?))
        }
        TokenType::MINUS => {
            Ok(Object::Integer(left.checked_sub(right)
                .context("Subtraction overflowed!")?))
        }
        TokenType::ASTERISK => {
            Ok(Object::Integer(left.checked_mul(right)
                .context("Multiplication overflowed!")?))
        }
        TokenType::SLASH => {
            Ok(Object::Integer(left.checked_div(right)
                .with_context(|| format!("Illegal division: {}/{}", left, right))?))
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

fn eval_string_infix_expression(operator: Token, left: String, right: String) -> Result<Object> {
    match operator.token_type {
        TokenType::PLUS => {
            let mut left = left;
            left.push_str(&right);
            Ok(Object::String(left))
        }
        t => bail!("Unknown infix operator for two strings: {}", t)
    }
}

fn eval_if_expression(condition: Expression, block: BlockStatement, alternative: Option<BlockStatement>, env: Rc<Mutex<Environment>>) -> Result<Object> {
    let cond = eval_expression(condition, env.clone())
        .context("Evaluating condition of if expression.")?;

    if is_truthy(cond) {
        return Ok(eval_block_statements(block.statements, env.clone())
            .context("Evaluating block statements of if expression.")?);
    } else if let Some(alt) = alternative {
        return Ok(eval_block_statements(alt.statements, env.clone())
            .context("Evaluating alternative statements of if expression.")?);
    }
    Ok(Object::Null)
}

fn native_bool_to_boolean_object(b: bool) -> Object {
    Object::Boolean(b)
}

fn apply_function(func: Object, args: Vec<Object>) -> Result<Object> {
    match func {
        Object::Function(params, body, env) => {
            let extended_env =
                extend_function_env(params, args, env.clone())
                    .context("Extending exising environment with parameter/argument pairs.")?;
            // println!("Function environment: \n{extended_env}");
            let evaluated = eval_block_statements(body.statements, extended_env.clone())
                .context("Evaluating function body.")?;

            return Ok(unwrap_return_value(evaluated));
        }
        Object::Builtin(fn_name) => {
            let function = get_builtin_function(&fn_name).unwrap();
            function(args).context("Calling builtin function.")
        }
        _ => { bail!("Object {:?} is not a function!", func); }
    }
}

fn extend_function_env(parameters: Vec<Identifier>, arguments: Vec<Object>, environment: Rc<Mutex<Environment>>) -> Result<Rc<Mutex<Environment>>> {
    if parameters.len() != arguments.len() {
        bail!(
            "Number of parameters and arguments do not match! Expected: {} Got: {}",
            parameters.len(), 
            arguments.len()
        );
    }
    let env = environment;
    for (param, arg) in parameters.iter().zip(arguments) {
        env.lock().unwrap().set(&param.value, arg)
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

fn eval_index_expression(left: Object, idx: Object) -> Result<Object> {
    match (&left, &idx) {
        (Object::Array(elems), Object::Integer(i)) => {
            if *i < 0 || *i as usize > elems.len() - 1 {
                return Ok(Object::Null);
            }
            return Ok(elems[*i as usize].clone());
        }
        (Object::Hash(map), io) => {
            let key = HashKey::from_object(io).context("Converting index to hash key.")?;
            if let Some(val) = map.get(&key) {
                Ok(val.clone())
            } else { Ok(Object::Null) }
        }
        _ => {
            bail!("Index operator unknown for types {}[{}]", &left.type_str(), &idx)
        }
    }
}

fn eval_hash_literal(node: Expression, env: Rc<Mutex<Environment>>) -> Result<Object> {
    if let Expression::HASH_LITERAL(_, keys, vals) = node {
        let mut map = HashMap::with_capacity(keys.len());
        for (k, v) in keys.iter().zip(vals) {
            let key = eval_expression(k.clone(), env.clone())
                .context("Evaluating key expression.")?;
            let value = eval_expression(v.clone(), env.clone())
                .context("Evaluating key expression.")?;
            map.insert(HashKey::from_object(&key)
                           .context("Converting Object into HashKey")?,
                       value);
        }
        Ok(Object::Hash(map))
    } else { bail!("Expected hash literal, got {}", node) }
}