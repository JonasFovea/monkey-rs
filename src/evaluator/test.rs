use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{Context, Result};

use crate::ast::Parser;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::{Environment, HashKey, Object};

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
        ("\"Hello\" - \"World\"", "Unknown infix operator for two strings: MINUS"),
        ("{\"name\": \"Monkey\"}[fn(x){x}];", "Object of type Function is not hashable! Supported types: Integer, String, Boolean.")
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

#[test]
fn test_string_literal() {
    let input = "\"Hello World!\"";
    let evaluated = test_eval(input);
    if let Ok(Object::String(s)) = evaluated {
        assert_eq!(s, "Hello World!");
    } else { assert!(false); }
}

#[test]
fn test_string_concatenation() {
    let input = "\"Hello\" + \" \" + \"World!\"";
    let evaluated = test_eval(input);
    if let Ok(Object::String(s)) = evaluated {
        assert_eq!(s, "Hello World!");
    } else { assert!(false); }
}

#[test]
fn test_builtin_functions() {
    let int_tests = vec![
        ("len(\"\")", 0),
        ("len(\"four\")", 4),
        ("len(\"hello world\")", 11),
    ];

    for (input, expected) in int_tests {
        if let Ok(Object::Integer(i)) = test_eval(input) {
            assert_eq!(expected, i);
        } else { assert!(false); }
    }

    let failing_tests = vec![
        ("len(1)", "Invalid argument of type: Integer"),
        ("len(\"one\", \"two\")", "Invalid number of arguments! Expected: 1, Got: 2"),
    ];

    for (input, expected) in failing_tests {
        if let Err(e) = test_eval(input) {
            assert_eq!(expected, e.root_cause().to_string());
        } else { assert!(false); }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    if let Ok(Object::Array(e)) = evaluated {
        assert_eq!(format!("{}", e[0]), "1");
        assert_eq!(format!("{}", e[1]), "4");
        assert_eq!(format!("{}", e[2]), "6");
    } else { assert!(false); }
}

#[test]
fn test_array_index_expressions() {
    let tests = vec![
        ("[1,2,3][0]", Object::Integer(1)),
        ("[1,2,3][1]", Object::Integer(2)),
        ("[1,2,3][2]", Object::Integer(3)),
        ("let i = 0;[1][i];", Object::Integer(1)),
        ("[1,2,3][1+1];", Object::Integer(3)),
        ("let myArray = [1,2,3]; myArray[2];", Object::Integer(3)),
        ("let myArray = [1,2,3]; myArray[0] + myArray[1] + myArray[2];", Object::Integer(6)),
        ("[1,2,3][3]", Object::Null),
        ("[1,2,3][-1]", Object::Null),
    ];

    for (input, expected) in tests {
        if let Ok(ev) = test_eval(input) {
            // println!("{:?} == {:?}", &expected, &ev);
            assert_eq!(expected, ev);
        } else { assert!(false) }
    }
}

#[test]
fn test_hash_literals() {
    let input = "let two = \"two\";\
    {\
    \"one\": 10 - 9,\
    two: 1+1,\
    \"thr\" + \"ee\": 6/2,\
    4: 4,\
    true: 5,\
    false: 6\
    }";

    let expected = vec![
        (Object::String("one".to_string()), Object::Integer(1)),
        (Object::String("two".to_string()), Object::Integer(2)),
        (Object::String("three".to_string()), Object::Integer(3)),
        (Object::Integer(4), Object::Integer(4)),
        (Object::Boolean(true), Object::Integer(5)),
        (Object::Boolean(false), Object::Integer(6)),
    ];


    let evaluated = test_eval(input);
    println!("{:?}", evaluated);
    if let Ok(Object::Hash(map)) = evaluated {
        for (ek, ev) in expected {
            assert_eq!(*map.get(&HashKey::from_object(&ek).unwrap()).unwrap(), ev);
        }
    } else { assert!(false); }
}

#[test]
fn test_hash_index_expressions() {
    let tests = vec![
        ("{\"foo\":5}[\"foo\"]", Object::Integer(5)),
        ("{\"foo\":5}[\"bar\"]", Object::Null),
        ("let key = \"foo\"; {\"foo\":5}[key]", Object::Integer(5)),
        ("{}[\"foo\"]", Object::Null),
        ("{5:5}[5]", Object::Integer(5)),
        ("{true:5}[true]", Object::Integer(5)),
        ("{false:5}[false]", Object::Integer(5)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        if let Ok(obj) = evaluated {
            assert_eq!(obj, expected);
        } else { assert!(false); }
    }
}

fn test_eval(input: &str) -> Result<Object> {
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()
        .context("Parsing program.")?;
    let env = Rc::new(Mutex::new(Environment::new()));

    let res = eval_program(program, env)
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