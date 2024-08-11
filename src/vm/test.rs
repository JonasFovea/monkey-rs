use crate::ast::{Parser, Program};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::{HashKey, Object};
use crate::vm::VM;
use std::collections::HashMap;

fn parse(input: &str) -> Program {
    let lex = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lex);
    return parser.parse_program().unwrap();
}

fn test_integer_object(expected: i64, actual: &Object) {
    match actual {
        Object::Integer(i) => assert_eq!(expected, *i, "Object has wrong value. got={}, want={}", i, expected),
        _ => assert!(false, "Object is not an Integer! Got {} instead.", actual.type_str())
    }
}

fn test_boolean_object(expected: bool, actual: &Object) {
    match actual {
        Object::Boolean(b) => assert_eq!(expected, *b, "Object has wrong value. got={}, want={}", b, expected),
        _ => assert!(false, "Object is not a Boolean! Got {} instead.", actual.type_str())
    }
}

fn test_string_object(expected: &str, actual: &Object) {
    match actual {
        Object::String(s) => assert_eq!(expected, s, "Object has wrong value. got={}, want={}", s, expected),
        _ => assert!(false, "Object is not a string! Got {} instead.", actual.type_str())
    }
}

fn test_array_object(objects: &[Object], actual: &Object) {
    match actual {
        Object::Array(elems) => {
            for (i, (e, a)) in objects.iter().zip(elems).enumerate() {
                assert_eq!(e, a, "Object at index {} in array is not equal to expected!\nwant={}\n got={}\n", i, e, a);
            }
        }
        _ => assert!(false, "Object is not an array! Got {} instead.", actual.type_str())
    }
}

fn test_hash_object(expected: &HashMap<HashKey, Object>, actual: &Object) {
    match actual {
        Object::Hash(amap) => {
            for (k, v) in expected {
                assert_eq!(*v, amap[k], "Expected values not equal!\nwant={}\n got={}\n", v, amap[k]);
            }
        }
        _ => assert!(false, "Object is not a hash! Got {} instead.", actual.type_str())
    }
}

#[derive(Debug)]
struct VMTestCase {
    input: String,
    expected: Object,
}

impl VMTestCase {
    pub(crate) fn new_int_result_case(input: &str, expected_int: i64) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Integer(expected_int),
        }
    }
    pub(crate) fn new_bool_result_case(input: &str, expected_bool: bool) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Boolean(expected_bool),
        }
    }

    pub(crate) fn new_string_result_case(input: &str, expected_string: &str) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::String(expected_string.to_string()),
        }
    }

    pub(crate) fn new_array_result_case(input: &str, expected_array: Vec<Object>) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Array(expected_array),
        }
    }
    pub(crate) fn new_null_result_case(input: &str) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Null,
        }
    }
}

fn run_vm_tests(tests: Vec<VMTestCase>) {
    for test in tests {
        let program = parse(&test.input);
        let mut comp = Compiler::new();
        let cmpl_err = comp.compile_program(&program);
        if cmpl_err.is_err() {
            assert!(false, "Compiling the program failed!\n{:?}", cmpl_err);
        }

        let mut vm = VM::new(comp.bytecode().unwrap());
        // println!("{:?}", &vm);
        let run_err = vm.run();
        if let Err(e) = run_err {
            for (i, cause) in e.chain().enumerate() {
                eprintln!("\t{i}: {cause}");
            }
            assert!(false, "Running bytecode failed!\n{:?}", e);
        }
        let stack_elem = vm.last_popped_stack_elem();
        if stack_elem.is_err() {
            assert!(false, "Couldn't retrieve top stack element!\n{:?}", stack_elem);
        }
        let stack_elem = stack_elem.unwrap();

        test_expected_object(&test.expected, &stack_elem);
    }
}

fn test_expected_object(expected: &Object, actual: &Object) {
    match expected {
        Object::Integer(i) => test_integer_object(*i, actual),
        Object::Boolean(b) => test_boolean_object(*b, actual),
        Object::Null => assert_eq!(expected, actual),
        Object::String(s) => test_string_object(s, actual),
        Object::Array(obj) => test_array_object(obj, actual),
        Object::Hash(map) => test_hash_object(map, actual),
        _ => todo!("Test case not implemented.")
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = vec![
        VMTestCase::new_int_result_case("1", 1),
        VMTestCase::new_int_result_case("2", 2),
        VMTestCase::new_int_result_case("1 + 2", 3),
        VMTestCase::new_int_result_case("1 - 2", -1),
        VMTestCase::new_int_result_case("1 * 2", 2),
        VMTestCase::new_int_result_case("4 / 2", 2),
        VMTestCase::new_int_result_case("50 / 2 * 2 + 10 - 5", 55),
        VMTestCase::new_int_result_case("5 + 5 + 5 + 5 - 10", 10),
        VMTestCase::new_int_result_case("2 * 2 * 2 * 2 * 2", 32),
        VMTestCase::new_int_result_case("5 * 2 + 10", 20),
        VMTestCase::new_int_result_case("5 + 2 * 10", 25),
        VMTestCase::new_int_result_case("5 * ( 2 + 10 )", 60),
        VMTestCase::new_int_result_case("-5", -5),
        VMTestCase::new_int_result_case("-10", -10),
        VMTestCase::new_int_result_case("-50 + 100 + -50", 0),
        VMTestCase::new_int_result_case("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    run_vm_tests(tests);
}

#[test]
fn test_boolean_expressions() {
    let tests = vec![
        VMTestCase::new_bool_result_case("true", true),
        VMTestCase::new_bool_result_case("false", false),
        VMTestCase::new_bool_result_case("1 < 2", true),
        VMTestCase::new_bool_result_case("1 > 2", false),
        VMTestCase::new_bool_result_case("1 < 1", false),
        VMTestCase::new_bool_result_case("1 > 1", false),
        VMTestCase::new_bool_result_case("1 >= 1", true),
        VMTestCase::new_bool_result_case("1 >= 2", false),
        VMTestCase::new_bool_result_case("2 >= 1", true),
        VMTestCase::new_bool_result_case("1 <= 1", true),
        VMTestCase::new_bool_result_case("1 <= 2", true),
        VMTestCase::new_bool_result_case("1 <= 0", false),
        VMTestCase::new_bool_result_case("1 == 1", true),
        VMTestCase::new_bool_result_case("1 != 1", false),
        VMTestCase::new_bool_result_case("1 == 2", false),
        VMTestCase::new_bool_result_case("1 != 2", true),
        VMTestCase::new_bool_result_case("true == true", true),
        VMTestCase::new_bool_result_case("false == false", true),
        VMTestCase::new_bool_result_case("true == false", false),
        VMTestCase::new_bool_result_case("true != false", true),
        VMTestCase::new_bool_result_case("false != true", true),
        VMTestCase::new_bool_result_case("(1 < 2) == true", true),
        VMTestCase::new_bool_result_case("(1 < 2) == false", false),
        VMTestCase::new_bool_result_case("(1 > 2) == true", false),
        VMTestCase::new_bool_result_case("(1 > 2) == false", true),
        VMTestCase::new_bool_result_case("!true", false),
        VMTestCase::new_bool_result_case("!false", true),
        VMTestCase::new_bool_result_case("!5", false),
        VMTestCase::new_bool_result_case("!!true", true),
        VMTestCase::new_bool_result_case("!!false", false),
        VMTestCase::new_bool_result_case("!!5", true),
        VMTestCase::new_bool_result_case("!if(false){5;}", true),
    ];
    run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
    let tests = vec![
        VMTestCase {
            input: "if (true) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VMTestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
        },
        VMTestCase {
            input: "if (false) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
        },
        VMTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VMTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Object::Integer(10),
        },
        VMTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
        },
        VMTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
        },
        VMTestCase {
            input: "if (1 > 2) { 10 }".to_string(),
            expected: Object::Null,
        },
        VMTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: Object::Null,
        },
        VMTestCase {
            input: "if (( if (false) { 10 })) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_global_let_statement() {
    let tests = vec![
        VMTestCase::new_int_result_case("let one = 1; one;", 1),
        VMTestCase::new_int_result_case("let one = 1; let two = 2; one + two", 3),
        VMTestCase::new_int_result_case("let one = 1; let two = one + one; one + two", 3),
    ];

    run_vm_tests(tests);
}

#[test]
fn test_string_expressions() {
    let tests = vec![
        VMTestCase::new_string_result_case("\"monkey\"", "monkey"),
        VMTestCase::new_string_result_case("\"mon\" + \"key\"", "monkey"),
        VMTestCase::new_string_result_case("\"mon\" + \"key\" + \"banana\"", "monkeybanana"),
    ];

    run_vm_tests(tests);
}

#[test]
fn test_array_literals() {
    let tests = vec![
        VMTestCase::new_array_result_case("[]",
                                          vec![]),
        VMTestCase::new_array_result_case("[1, 2, 3]",
                                          vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)]),
        VMTestCase::new_array_result_case("[1 + 2, 3 * 4, 5 + 6]",
                                          vec![Object::Integer(3), Object::Integer(12), Object::Integer(11)]),
    ];

    run_vm_tests(tests)
}

#[test]
fn test_hash_literals() {
    let tests = vec![
        VMTestCase {
            input: "{}".to_string(),
            expected: Object::Hash(HashMap::new()),
        },
        VMTestCase {
            input: "{1: 2, 2: 3}".to_string(),
            expected: Object::Hash(HashMap::from([
                (HashKey::from_object(&Object::Integer(1)).unwrap(), Object::Integer(2)),
                (HashKey::from_object(&Object::Integer(2)).unwrap(), Object::Integer(3)),
            ])),
        },
        VMTestCase {
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
            expected: Object::Hash(HashMap::from([
                (HashKey::from_object(&Object::Integer(2)).unwrap(), Object::Integer(4)),
                (HashKey::from_object(&Object::Integer(6)).unwrap(), Object::Integer(16)),
            ])),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_index_expressions() {
    let tests = vec![
        VMTestCase::new_int_result_case("[1, 2, 3][1]", 2),
        VMTestCase::new_int_result_case("[1, 2, 3][0 + 2]", 3),
        VMTestCase::new_int_result_case("[[1, 1, 1]][0][0]", 1),
        VMTestCase::new_null_result_case("[][0]"),
        VMTestCase::new_null_result_case("[1, 2, 3][99]"),
        VMTestCase::new_null_result_case("[1][-1]"),
        VMTestCase::new_int_result_case("{1: 1, 2: 2}[1]", 1),
        VMTestCase::new_int_result_case("{1: 1, 2: 2}[2]", 2),
        VMTestCase::new_null_result_case("{1: 1}[0]"),
        VMTestCase::new_null_result_case("{}[0]"),
    ];

    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_without_arguments() {
    let tests = vec![
        VMTestCase::new_int_result_case("let fivePlusTen = fn(){ 5 + 10;}; fivePlusTen();", 15),
        VMTestCase::new_int_result_case(
            "let one = fn(){ 1;}; \
            one(); \
            let two = fn(){ 2;}; \
            two(); \
            one() + two()",
            3),
        VMTestCase::new_int_result_case(
            "let a = fn(){1;}; \
            let b = fn(){a() + 1;}; \
            let c = fn(){b() + 1}; \
            c();",
            3)
    ];

    run_vm_tests(tests);
}

#[test]
fn test_functions_with_return_statement() {
    let tests = vec![
        VMTestCase::new_int_result_case("let earlyExit = fn(){return 99; 100;}; earlyExit();", 99),
        VMTestCase::new_int_result_case("let earlyExit = fn(){return 99; return 100;}; earlyExit();", 99),
    ];

    run_vm_tests(tests);
}

#[test]
fn test_functions_without_return_value() {
    let tests = vec![
        VMTestCase::new_null_result_case("let noReturn = fn(){}; noReturn();"),
        VMTestCase::new_null_result_case(
            "let noReturn = fn(){}; \
            let noReturnTwo = fn(){noReturn();}; \
            noReturn(); \
            noReturnTwo();"
        ),
    ];

    run_vm_tests(tests);
}

#[test]
fn test_first_class_functions() {
    let tests = vec![
        VMTestCase::new_int_result_case(
            "let returnsOne = fn(){1;}; \
            let returnsOneReturner = fn(){returnsOne;}; \
            returnsOneReturner()();", 1)
    ];

    run_vm_tests(tests);
}
