use anyhow::{Context, Result};

use crate::ast::Parser;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::Object;

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5+ 5+ 5-10", 10),
        ("2*2*2*2*2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50/2 * 2 +10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 *3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_integer_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }
}

#[test]
fn test_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("1 >= 2", false),
        ("1 >= 1", true),
        ("1 <= 2", true),
        ("1 <= 1", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1<2) == true", true),
        ("(1<2)==false", false),
        ("(1>2)==true", false),
        ("(1 > 2) == false", true),
    ];
    for (input, expected) in tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_boolean_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];
    for (input, expected) in tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_boolean_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }
}

#[test]
fn test_if_else_expression() {
    let int_tests = vec![
        ("if (true) {10}", 10),
        ("if (1) {10}", 10),
        ("if (1 < 2) {10}", 10),
        ("if (1 > 2) {10} else {20}", 20),
        ("if (1 < 2) {10} else {20}", 10),
    ];

    let null_tests = vec![
        ("if (false) {10}", Object::Null),
        ("if (1 > 2) {10}", Object::Null),
    ];

    for (input, expected) in int_tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_integer_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }

    for (input, expected) in null_tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9", 10),
        ("if (10 > 1) { if ( 10 > 1 ) { return 10; } return 1; }", 10),
    ];
    for (input, expected) in tests {
        let result = test_eval(input);
        if let Ok(evaluated) = result {
            test_integer_object(evaluated, expected);
        } else {
            eprintln!("{:?}", result);
            assert!(false);
        }
    }
}

fn test_eval(input: &str) -> Result<Object> {
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()
        .context("Parsing program.")?;

    return eval_program(program);
}

fn test_integer_object(evaluated: Object, expected: i64) {
    if let Object::Integer(i) = evaluated {
        assert_eq!(i, expected);
    } else {
        eprintln!("{:?}", evaluated);
        assert!(false);
    }
}

fn test_boolean_object(evaluated: Object, expected: bool) {
    if let Object::Boolean(b) = evaluated {
        assert_eq!(b, expected);
    } else { assert!(false); }
}

fn test_object(evaluated: Object, expected: Object)
{
    assert_eq!(evaluated, expected);
}