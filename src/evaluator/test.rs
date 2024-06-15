use anyhow::{Context, Result};

use crate::ast::Parser;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::{Environment, Object};

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

#[test]
fn test_let_statement() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
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
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "No infix operator defined for objects Integer {value: 5} and Boolean {value: true}"),
        ("5 + true; 5;", "No infix operator defined for objects Integer {value: 5} and Boolean {value: true}"),
        ("-true", "Prefix minus operator undefined for Boolean {value: true}"),
        ("true + false;", "Unknown infix operator for two booleans: PLUS"),
        ("5; true + false; 5", "Unknown infix operator for two booleans: PLUS"),
        ("if (10 > 1) { true + false }", "Unknown infix operator for two booleans: PLUS"),
        ("if (10 > 1) { if ( 10 > 1) { return true + false; } return 1; }", "Unknown infix operator for two booleans: PLUS"),
        ("foobar", "Identifier not found: foobar"),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        if let Err(e) = evaluated {
            let first_err = e.chain().last().unwrap().to_string();
            assert_eq!(expected, first_err);
        } else {
            eprintln!("Evaluation should have failed! Gor result instead: {:?}", evaluated);
            assert!(false);
        }
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2 };";

    let evaluated = test_eval(input);

    if let Err(e) = &evaluated {
        eprintln!("{e:?}");
        assert!(false);
    }

    let evaluated = evaluated.unwrap();

    if let Object::Function(params, body, _) = evaluated {
        assert_eq!(1, params.len());
        assert_eq!("x", params[0].value);
        assert_eq!("{\n\t(x + 2)\n}", format!("{body}"));
    } else { assert!(false); }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x){x;}; identity(5);", 5),
        ("let identity = fn(x) {return x;}; identity(5);", 5),
        ("let double = fn(x) {x * 2;}; double(5);", 10),
        ("let add = fn(x,y){x+y;}; add(5,5);", 10),
        ("let add = fn(x,y){x+y;}; add(5+5,add(5,5));", 20),
        ("fn(x){x;}(5)", 5),
        ("let adder = fn(x) { fn(y) { x + y } }; let add_two = adder(2); add_two(5)", 7),
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

    let env = Environment::new();

    let (res, _) = eval_program(program, env)
        .context("Evaluating test program.")?;
    Ok(res)
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