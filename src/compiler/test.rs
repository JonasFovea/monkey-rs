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
                make(Opcode::OpAdd, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1; 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 - 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions:
            vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpSub, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 * 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions:
            vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpMul, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "2 / 1".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(1)],
            expected_instructions:
            vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpDiv, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "-1".to_string(),
            expected_constants: vec![Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpMinus, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_boolean_expressions() {
    let tests = vec![
        CompilerTestCase {
            input: "true;".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "false;".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpFalse, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 > 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpGreaterThan, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 < 2".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpGreaterThan, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 == 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpEqual, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 != 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpNotEqual, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 >= 2".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpGreaterEquals, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "1 <= 2".to_string(),
            expected_constants: vec![Object::Integer(2), Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpGreaterEquals, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "true == false".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, vec![]).unwrap(),
                make(Opcode::OpFalse, vec![]).unwrap(),
                make(Opcode::OpEqual, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "true != false".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, vec![]).unwrap(),
                make(Opcode::OpFalse, vec![]).unwrap(),
                make(Opcode::OpNotEqual, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "!true".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpTrue, vec![]).unwrap(),
                make(Opcode::OpBang, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_conditionals() {
    let tests = vec![
        CompilerTestCase {
            input: "if (true) { 10 }; 3333;".to_string(),
            expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
            expected_instructions: vec![
                //0000
                make(Opcode::OpTrue, vec![]).unwrap(),
                //0001
                make(Opcode::OpJumpNotTruthy, vec![10]).unwrap(),
                //0004
                make(Opcode::OpConstant, vec![0]).unwrap(),
                //0007
                make(Opcode::OpJump, vec![11]).unwrap(),
                //0010
                make(Opcode::OpNull, vec![]).unwrap(),
                //0011
                make(Opcode::OpPop, vec![]).unwrap(),
                //0012
                make(Opcode::OpConstant, vec![1]).unwrap(),
                //0015
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "if (true) {10} else {20}; 3333;".to_string(),
            expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
            expected_instructions: vec![
                //0000
                make(Opcode::OpTrue, vec![]).unwrap(),
                //0001
                make(Opcode::OpJumpNotTruthy, vec![10]).unwrap(),
                //0004
                make(Opcode::OpConstant, vec![0]).unwrap(),
                //0007
                make(Opcode::OpJump, vec![13]).unwrap(),
                //0010
                make(Opcode::OpConstant, vec![1]).unwrap(),
                //0013
                make(Opcode::OpPop, vec![]).unwrap(),
                //0014
                make(Opcode::OpConstant, vec![2]).unwrap(),
                //0017
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_global_let_statement() {
    let tests = vec![
        CompilerTestCase {
            input: "let one = 1; let two = 2;".to_string(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpSetGlobal, vec![1]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "let one = 1; one;".to_string(),
            expected_constants: vec![Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "let one = 1; let two = one; two;".to_string(),
            expected_constants: vec![Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![1]).unwrap(),
                make(Opcode::OpGetGlobal, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];

    run_compiler_tests(tests)
}

#[test]
fn test_string_expressions() {
    let tests = vec![
        CompilerTestCase {
            input: "\"monkey\"".to_string(),
            expected_constants: vec![Object::String("monkey".to_string())],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "\"mon\" + \"key\"".to_string(),
            expected_constants: vec![Object::String("mon".to_string()), Object::String("key".to_string())],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpAdd, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_array_expressions() {
    let tests = vec![
        CompilerTestCase {
            input: "[]".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpArray, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "[1, 2, 3]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpArray, vec![3]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4),
                Object::Integer(5),
                Object::Integer(6),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpAdd, vec![]).unwrap(),
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpSub, vec![]).unwrap(),
                make(Opcode::OpConstant, vec![4]).unwrap(),
                make(Opcode::OpConstant, vec![5]).unwrap(),
                make(Opcode::OpMul, vec![]).unwrap(),
                make(Opcode::OpArray, vec![3]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
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

fn test_string_object(expected: &str, actual: &Object) {
    match actual {
        Object::String(s) => assert_eq!(expected, *s, "Object has wrong value. got={}, want={}", s, expected),
        _ => assert!(false, "Object is not a String! Got {} instead.", actual.type_str())
    }
}

fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
    let concatted = Instructions::join(expected.clone());
    assert_eq!(concatted.len(), actual.len(),
               "Wrong instruction length.\nwant={:?}\n got={:?}",
               &concatted.to_string(), &actual.to_string());

    for (i, (e, a)) in concatted.0.iter().zip(actual.0).enumerate() {
        assert_eq!(*e, a, "Instruction {} does not match!\nwant={}\n got={}\ninstructions:\n{}\n", i, e, a, concatted.to_string().unwrap());
    }
}

fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
    assert_eq!(expected.len(), actual.len());

    for (e, a) in expected.iter().zip(actual) {
        match e {
            Object::Integer(i) => {
                test_integer_object(*i, &a);
            }
            Object::String(s) => {
                test_string_object(s, &a);
            }
            _ => { assert!(false, "Constants can't be compared.") }
        }
    }
}
