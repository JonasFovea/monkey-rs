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

#[derive(Debug)]
struct VMTestCase {
    input: String,
    expected: Object,
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
        if run_err.is_err() {
            assert!(false, "Running bytecode failed!\n{:?}", run_err);
        }
        let stack_elem = vm.stack_top();
        if stack_elem.is_err(){
            assert!(false, "Couldn't retrieve top stack element!\n{:?}", stack_elem);
        }
        let stack_elem = stack_elem.unwrap();

        test_expected_object(&test.expected, &stack_elem);
    }
}

fn test_expected_object(expected: &Object, actual: &Object) {
    match expected {
        Object::Integer(i) => test_integer_object(*i, actual),
        _ => todo!("Test case not implemented.")
    }
}

#[test]
fn test_integer_arithmetic() {
    let tests = vec![
        VMTestCase {
            input: "1".to_string(),
            expected: Object::Integer(1),
        },
        VMTestCase {
            input: "2".to_string(),
            expected: Object::Integer(2),
        },
        VMTestCase {
            input: "1 + 2".to_string(),
            expected: Object::Integer(3),
        },
    ];
    run_vm_tests(tests);
}