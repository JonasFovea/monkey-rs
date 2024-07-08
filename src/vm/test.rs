use crate::ast::{Parser, Program};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::vm::VM;

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
        VMTestCase{
            input: "if (( if (false) { 10 })) { 10 } else { 20 }".to_string(),
            expected: Object::Integer(20)
        }
    ];

    run_vm_tests(tests);
}

#[test]
fn test_global_let_statement(){
    let tests = vec![
        VMTestCase::new_int_result_case("let one = 1; one;", 1),
        VMTestCase::new_int_result_case("let one = 1; let two = 2; one + two", 3),
        VMTestCase::new_int_result_case("let one = 1; let two = one + one; one + two", 3),
    ];
    
    run_vm_tests(tests);
}