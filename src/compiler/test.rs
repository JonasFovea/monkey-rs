use crate::ast::{Parser, Program};
use crate::code::{Instructions, lookup, make, Opcode, read_operands};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;

struct CompilerTestCase {
    input: String,
    expected_constants: Vec<Object>,
    expected_instructions: Vec<Instructions>,
}

#[test]
fn test_integer_arithmetic() {
    let tests = vec![
        CompilerTestCase {
            input: "1 + 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions:
            vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpAdd, vec![]).unwrap()
            ],
        }
    ];
    run_compiler_tests(tests);
}

fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
    for test in tests {
        let program = parse(&test.input);
        let mut compiler = Compiler::new();
        let res = compiler.compile_program(&program);
        if res.is_err() {
            assert!(false, "Compiling failed: {:?}", res);
        }

        let bytecode = compiler.bytecode();
        if bytecode.is_err() {
            assert!(false, "No bytecode generated: {:?}\nwant={:?}\n", &compiler,
                    Instructions::join(test.expected_instructions).to_string().unwrap());
        }
        let bytecode = bytecode.unwrap();

        test_instructions(test.expected_instructions.clone(), bytecode.instructions.clone());
        test_constants(test.expected_constants.clone(), bytecode.constants.clone());
    }
}

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

fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
    let concatted = Instructions::join(expected.clone());
    assert_eq!(concatted.len(), actual.len(),
               "Wrong instruction length.\nwant={:?}\n got={:?}",
               &concatted.to_string(), &actual.to_string());

    for (e, a) in concatted.0.iter().zip(actual.0) {
        assert_eq!(*e, a);
    }
}

fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
    assert_eq!(expected.len(), actual.len());

    for (e, a) in expected.iter().zip(actual) {
        match e {
            Object::Integer(i) => {
                test_integer_object(*i, &a);
            }
            _ => { assert!(false, "Constants can't be compared.") }
        }
    }
}


#[test]
fn test_read_operands() {
    let tests = vec![(Opcode::OpConstant, vec![65535], 2)];

    for (op, operands, bytes_read) in tests {
        let instruction = make(op, operands.clone()).unwrap();
        let def = lookup(op.into()).unwrap();

        let (operands_read, n) = read_operands(def, Instructions(instruction.0[1..].to_vec()));
        assert_eq!(bytes_read, n);

        for (e, a) in operands.iter().zip(operands_read) {
            assert_eq!(*e, a);
        }
    }
}