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

fn test_integer_object(expected: i64, actual: &Object, test_no: usize) {
    match actual {
        Object::Integer(i) => assert_eq!(expected, *i, "Object in test no {} has wrong value. got={}, want={}", test_no, i, expected),
        _ => assert!(false, "Object in test no {} is not an Integer! Got {} instead.", test_no, actual.type_str())
    }
}

fn test_boolean_object(expected: bool, actual: &Object, test_no: usize) {
    match actual {
        Object::Boolean(b) => assert_eq!(expected, *b, "Object in test no {} has wrong value. got={}, want={}", test_no, b, expected),
        _ => assert!(false, "Object in test no {} is not a Boolean! Got {} instead.", test_no, actual.type_str())
    }
}

fn test_string_object(expected: &str, actual: &Object, test_no: usize) {
    match actual {
        Object::String(s) => assert_eq!(expected, s, "Object in test no {} has wrong value. got={}, want={}", test_no, s, expected),
        _ => assert!(false, "Object in test no {} is not a string! Got {} instead.", test_no, actual.type_str())
    }
}

fn test_array_object(objects: &[Object], actual: &Object, test_no: usize) {
    match actual {
        Object::Array(elems) => {
            for (i, (e, a)) in objects.iter().zip(elems).enumerate() {
                assert_eq!(e, a, "Object in test no {} at index {} in array is not equal to expected!\nwant={}\n got={}\n", test_no, i, e, a);
            }
        }
        _ => assert!(false, "Object in test no {} is not an array! Got {} instead.", test_no, actual.type_str())
    }
}

fn test_hash_object(expected: &HashMap<HashKey, Object>, actual: &Object, test_no: usize) {
    match actual {
        Object::Hash(amap) => {
            for (k, v) in expected {
                assert_eq!(*v, amap[k], "Expected values in test no {} not equal!\nwant={}\n got={}\n", test_no, v, amap[k]);
            }
        }
        _ => assert!(false, "Object in test no {} is not a hash! Got {} instead.", test_no, actual.type_str())
    }
}

#[derive(Debug)]
enum ExpectError {
    None,
    Error(String),
}

#[derive(Debug)]
struct VMTestCase {
    input: String,
    expected: Object,
    expected_error: ExpectError,
}

impl VMTestCase {
    pub(crate) fn new_int_result_case(input: &str, expected_int: i64) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Integer(expected_int),
            expected_error: ExpectError::None,
        }
    }
    pub(crate) fn new_bool_result_case(input: &str, expected_bool: bool) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Boolean(expected_bool),
            expected_error: ExpectError::None,
        }
    }

    pub(crate) fn new_string_result_case(input: &str, expected_string: &str) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::String(expected_string.to_string()),
            expected_error: ExpectError::None,
        }
    }

    pub(crate) fn new_array_result_case(input: &str, expected_array: Vec<Object>) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Array(expected_array),
            expected_error: ExpectError::None,
        }
    }
    pub(crate) fn new_null_result_case(input: &str) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Null,
            expected_error: ExpectError::None,
        }
    }

    pub(crate) fn new_expected_error(input: &str, err_msg: &str) -> Self {
        VMTestCase {
            input: input.to_string(),
            expected: Object::Null,
            expected_error: ExpectError::Error(err_msg.to_string()),
        }
    }
}

fn run_vm_tests(tests: Vec<VMTestCase>) {
    for (tst_no, test) in tests.iter().enumerate() {
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
            if let ExpectError::Error(msg) = &test.expected_error {
                assert_eq!(*msg, e.root_cause().to_string(), "Unexpected error message!\nwant={},\n got={}", msg, e.root_cause().to_string());
                continue;
            } else {
                for (i, cause) in e.chain().enumerate() {
                    eprintln!("\t{i}: {cause}");
                }
                assert!(false, "Running bytecode failed!\n{:?}", e);
            }
        }
        let stack_elem = vm.last_popped_stack_elem();
        if stack_elem.is_err() {
            assert!(false, "Couldn't retrieve top stack element!\n{:?}", stack_elem);
        }
        let stack_elem = stack_elem.unwrap();

        test_expected_object(&test.expected, &stack_elem, tst_no);
    }
}

fn test_expected_object(expected: &Object, actual: &Object, test_no: usize) {
    match expected {
        Object::Integer(i) => test_integer_object(*i, actual, test_no),
        Object::Boolean(b) => test_boolean_object(*b, actual, test_no),
        Object::Null => assert_eq!(expected, actual, "Object in test no. {} is not Null!", test_no),
        Object::String(s) => test_string_object(s, actual, test_no),
        Object::Array(obj) => test_array_object(obj, actual, test_no),
        Object::Hash(map) => test_hash_object(map, actual, test_no),
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
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (true) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (false) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (1) { 10 }".to_string(),
            expected: Object::Integer(10),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (1 < 2) { 10 }".to_string(),
            expected: Object::Integer(10),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (1 < 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(10),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (1 > 2) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (1 > 2) { 10 }".to_string(),
            expected: Object::Null,
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (false) { 10 }".to_string(),
            expected: Object::Null,
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "if (( if (false) { 10 })) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20),
            expected_error: ExpectError::None,
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
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "{1: 2, 2: 3}".to_string(),
            expected: Object::Hash(HashMap::from([
                (HashKey::from_object(&Object::Integer(1)).unwrap(), Object::Integer(2)),
                (HashKey::from_object(&Object::Integer(2)).unwrap(), Object::Integer(3)),
            ])),
            expected_error: ExpectError::None,
        },
        VMTestCase {
            input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
            expected: Object::Hash(HashMap::from([
                (HashKey::from_object(&Object::Integer(2)).unwrap(), Object::Integer(4)),
                (HashKey::from_object(&Object::Integer(6)).unwrap(), Object::Integer(16)),
            ])),
            expected_error: ExpectError::None,
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
            returnsOneReturner()();", 1),
        VMTestCase::new_int_result_case(
            "let returnsOneReturner = fn() {\
            let returnsOne = fn () { 1; };\
            returnsOne;\
            };\
            returnsOneReturner()();",
            1
        )
    ];

    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_bindings() {
    let tests = vec![
        VMTestCase::new_int_result_case("let one = fn () { let one = 1; one }; one();", 1),
        VMTestCase::new_int_result_case("let oneAndTwo = fn () { let one = 1; let two = 2; one + two; }; \
        oneAndTwo();", 3),
        VMTestCase::new_int_result_case("let oneAndTwo = fn () { let one = 1; let two = 2; one + two; };\
        let threeAndFour = fn () { let three = 3; let four = 4; three + four; };\
        oneAndTwo() + threeAndFour();", 10),
        VMTestCase::new_int_result_case("let firstFoobar = fn() { let foobar = 50; foobar;}\
        let secondFoobar = fn() { let foobar = 100; foobar;}\
        firstFoobar() + secondFoobar();", 150),
        VMTestCase::new_int_result_case(
            "let globalSeed = 50;\
            let minusOne = fn() {\
            let num = 1;\
            globalSeed - num;\
            };\
            let minusTwo = fn () {\
            let num = 2;\
            globalSeed - num;\
            };\
            minusOne() + minusTwo();",
            97
        )
    ];

    run_vm_tests(tests);
}

#[test]
fn test_calling_functions_with_arguments_and_bindings() {
    let tests = vec![
        VMTestCase::new_int_result_case("let identity = fn (a) {a;}; identity(4);", 4),
        VMTestCase::new_int_result_case("let sum = fn (a, b) { a + b; }; sum(1, 2);", 3),
        VMTestCase::new_int_result_case("let sum = fn (a, b) { let c = a + b; c;}; sum(1, 2);", 3),
        VMTestCase::new_int_result_case("let sum = fn (a, b) { let c = a + b; c;}; sum(1, 2) + sum(3 , 4);", 10),
        VMTestCase::new_int_result_case("let sum = fn (a, b) { let c = a + b; c; }; let outer = fn () { sum(1,2) + sum (3, 4); }; outer();", 10),
        VMTestCase::new_int_result_case(
            "let globalNum = 10;\
            let sum = fn(a, b) {\
            let c = a + b;\
            c + globalNum;\
            };\
            let outer = fn() {\
            sum(1, 2) + sum(3, 4) + globalNum;\
            };\
            outer() + globalNum;",
            50)
    ];

    run_vm_tests(tests)
}

#[test]
fn test_calling_functions_with_wrong_arguments() {
    let tests = vec![
        ("fn(){1;}(1);", "wrong number of arguments: want=0, got=1"),
        ("fn(a){a;}();", "wrong number of arguments: want=1, got=0"),
        ("fn(a, b){ a+b;}(1);", "wrong number of arguments: want=2, got=1")
    ];

    for (input, expected) in tests {
        let program = parse(input);

        let mut comp = Compiler::new();
        if let Err(e) = comp.compile_program(&program) {
            assert!(false, "Compiler error: {:?}", e);
        }

        let mut vm = VM::new(comp.bytecode().unwrap());
        match vm.run() {
            Ok(_) => assert!(false, "Expected VM error but resulted in none."),
            Err(e) => {
                assert_eq!(expected, e.root_cause().to_string());
            }
        }
    }
}

#[test]
fn test_builtin_functions() {
    let tests = vec![
        VMTestCase::new_int_result_case("len(\"\")", 0),
        VMTestCase::new_int_result_case("len(\"four\")", 4),
        VMTestCase::new_int_result_case("len(\"hello world\")", 11),
        VMTestCase::new_expected_error("len(1)", "argument to `len` not supported, got INTEGER"),
        VMTestCase::new_expected_error("len(\"one\", \"two\")", "wrong number of arguments. got=2, want=1"),
        VMTestCase::new_int_result_case("len([1,2,3])", 3),
        VMTestCase::new_int_result_case("len([])", 0),
        VMTestCase::new_null_result_case("puts(\"hello\", \"world\")"),
        VMTestCase::new_int_result_case("first([1,2,3])", 1),
        VMTestCase::new_null_result_case("first([])"),
        VMTestCase::new_expected_error("first(1)", "argument to `first` must be ARRAY, got INTEGER"),
        VMTestCase::new_int_result_case("last([1,2,3])", 3),
        VMTestCase::new_null_result_case("last([])"),
        VMTestCase::new_expected_error("last(1)", "argument to `last` must be ARRAY, got INTEGER"),
        VMTestCase::new_array_result_case("rest([1,2,3])", vec![Object::Integer(2), Object::Integer(3)]),
        VMTestCase::new_null_result_case("rest([])"),
        VMTestCase::new_expected_error("rest(1)", "argument to `rest` must be ARRAY, got INTEGER"),
        VMTestCase::new_array_result_case("push([], 1)", vec![Object::Integer(1)]),
        VMTestCase::new_expected_error("push(1, 1)", "argument to `push` must be ARRAY, got INTEGER"),
    ];

    run_vm_tests(tests)
}

#[test]
fn test_closures() {
    let tests = vec![
        VMTestCase::new_int_result_case("let newClosure = fn(a){ fn(){a;};}; let closure = newClosure(99); closure();", 99),
        VMTestCase::new_int_result_case("let newAdder = fn(a, b){ fn(c){ a + b + c;};}; let adder = newAdder(1, 2); adder(8);", 11),
        VMTestCase::new_int_result_case("let newAdder = fn(a, b){ let c = a + b; fn(d){c + d;}; }; let adder = newAdder(1, 2); adder(8);", 11),
        VMTestCase::new_int_result_case(
            "let newAdderOuter = fn(a, b){\
            let c = a + b;\
            fn(d){\
            let e = d + c;\
            fn(f){\
            e + f;\
            };\
            };\
            };\
            let newAdderInner = newAdderOuter(1, 2);\
            let adder = newAdderInner(3);\
            adder(8);",
            14),
        VMTestCase::new_int_result_case(
            "let a = 1;\
            let newAdderOuter = fn(b){\
            fn(c){
            fn(d){ a + b + c + d };
            };
            };
            let newAdderInner = newAdderOuter(2);
            let adder = newAdderInner(3);
            adder(8);",
            14),
    ];

    run_vm_tests(tests)
}
