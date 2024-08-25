use crate::ast::{Parser, Program};
use crate::code::{lookup, make, read_operands, Instructions, Opcode};
use crate::compiler::symbol_table::{Scope, Symbol, SymbolTable};
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::object::Object;
use std::rc::Rc;
use std::sync::Mutex;

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

#[test]
fn test_hash_literals() {
    let tests = vec![
        CompilerTestCase {
            input: "{}".to_string(),
            expected_constants: vec![],
            expected_instructions: vec![
                make(Opcode::OpHash, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "{1: 2, 3: 4, 5: 6}".to_string(),
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
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpConstant, vec![4]).unwrap(),
                make(Opcode::OpConstant, vec![5]).unwrap(),
                make(Opcode::OpHash, vec![6]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "{1: 2 + 3, 4: 5 * 6}".to_string(),
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
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpAdd, vec![]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpConstant, vec![4]).unwrap(),
                make(Opcode::OpConstant, vec![5]).unwrap(),
                make(Opcode::OpMul, vec![]).unwrap(),
                make(Opcode::OpHash, vec![4]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];
    run_compiler_tests(tests);
}

#[test]
fn test_index_expressions() {
    let tests = vec![
        CompilerTestCase {
            input: "[1, 2, 3][1 + 1]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(1),
                Object::Integer(1),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpArray, vec![3]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpConstant, vec![4]).unwrap(),
                make(Opcode::OpAdd, vec![]).unwrap(),
                make(Opcode::OpIndex, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "{1: 2}[2 - 1]".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::Integer(2),
                Object::Integer(1),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpHash, vec![2]).unwrap(),
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpSub, vec![]).unwrap(),
                make(Opcode::OpIndex, vec![]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];

    run_compiler_tests(tests)
}

#[test]
fn test_functions() {
    let tests = vec![
        CompilerTestCase {
            input: "fn() { return 5 + 10 }".to_string(),
            expected_constants: vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpConstant, vec![0]).unwrap(),
                    make(Opcode::OpConstant, vec![1]).unwrap(),
                    make(Opcode::OpAdd, vec![]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap(),
                ]), 0, 0),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "fn() { 5 + 10 }".to_string(),
            expected_constants: vec![
                Object::Integer(5),
                Object::Integer(10),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpConstant, vec![0]).unwrap(),
                    make(Opcode::OpConstant, vec![1]).unwrap(),
                    make(Opcode::OpAdd, vec![]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap(),
                ]), 0, 0),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "fn() {1; 2}".to_string(),
            expected_constants: vec![
                Object::Integer(1),
                Object::Integer(2),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpConstant, vec![0]).unwrap(),
                    make(Opcode::OpPop, vec![]).unwrap(),
                    make(Opcode::OpConstant, vec![1]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap(),
                ]), 0, 0),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_compilation_scopes() {
    let mut compiler = Compiler::new();
    assert_eq!(0, compiler.scope_index, "Scope index wrong. got={}, want={}", compiler.scope_index, 0);

    let global_symbol_table = compiler.symbol_table.clone();

    compiler.emit(Opcode::OpMul, vec![]).expect("Unable to emit OpMul.");

    compiler.enter_scope();
    assert_eq!(1, compiler.scope_index, "Scope index wrong. got={}, want={}", compiler.scope_index, 1);

    compiler.emit(Opcode::OpSub, vec![]).expect("Unable to emit OpSub.");

    assert_eq!(1, compiler.scopes[compiler.scope_index].instructions.len(),
               "Instructions length wrong. got={}, want={}",
               compiler.scopes[compiler.scope_index].instructions.len(), 1);

    let last = compiler.scopes[compiler.scope_index].last_instruction
        .expect("Last instruction of current scope is None!");
    assert_eq!(Opcode::OpSub, last.opcode, "Last instruction opcode wrong. got={:?} want={:?}", last.opcode, Opcode::OpSub);

    if let Some(outer) = &compiler.symbol_table.lock().unwrap().outer {
        assert!(Rc::ptr_eq(outer, &global_symbol_table), "Compiler did not enclose the correct symbol table!");
    } else {
        assert!(false, "Compiler did not enclose the symbol table!")
    }

    compiler.leave_scope().unwrap();
    assert_eq!(0, compiler.scope_index, "Scope index wrong. got={}, want={}", compiler.scope_index, 0);

    assert!(Rc::ptr_eq(&compiler.symbol_table, &global_symbol_table), "Compiler did not restore global symbol table!");
    assert!(compiler.symbol_table.lock().unwrap().outer.is_none(), "Compiler modified global symbol table incorrectly!");

    compiler.emit(Opcode::OpAdd, vec![]).expect("Unable to emit OpAdd.");
    assert_eq!(2, compiler.scopes[compiler.scope_index].instructions.len(),
               "instructions lenght wrong. got={}, want={}",
               compiler.scopes[compiler.scope_index].instructions.len(), 2);

    let last = compiler.scopes[compiler.scope_index].last_instruction
        .expect("Last instruction of current scope is None!");
    assert_eq!(Opcode::OpAdd, last.opcode, "Last instruction opcode wrong. got={:?} want={:?}", last.opcode, Opcode::OpAdd);

    let previous = compiler.scopes[compiler.scope_index].previous_instruction
        .expect("Previous instruction of current scope is None!");
    assert_eq!(Opcode::OpMul, previous.opcode, "Last instruction opcode wrong. got={:?} want={:?}", previous.opcode, Opcode::OpMul);
}

#[test]
fn test_functions_without_return_value() {
    let tests = vec![
        CompilerTestCase {
            input: "fn () {}".to_string(),
            expected_constants: vec![
                Object::CompiledFunction(make(Opcode::OpReturn, vec![]).unwrap(), 0, 0)
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        }
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_function_calls() {
    let tests = vec![
        CompilerTestCase {
            input: "fn () { 24 }()".to_string(),
            expected_constants: vec![
                Object::Integer(24),
                Object::CompiledFunction(
                    Instructions::join(vec![
                        make(Opcode::OpConstant, vec![0]).unwrap(),
                        make(Opcode::OpReturnValue, vec![]).unwrap(),
                    ]),
                    0, 0
                ),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpCall, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "let noArg = fn () { 24 }; noArg();".to_string(),
            expected_constants: vec![
                Object::Integer(24),
                Object::CompiledFunction(
                    Instructions::join(vec![
                        make(Opcode::OpConstant, vec![0]).unwrap(),
                        make(Opcode::OpReturnValue, vec![]).unwrap(),
                    ]),
                    0, 0
                ),
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                make(Opcode::OpCall, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ],
        },
        CompilerTestCase {
            input: "let oneArg = fn(a) {a;}; oneArg(24);".to_string(),
            expected_constants: vec![Object::CompiledFunction(Instructions::join(vec![
                make(Opcode::OpGetLocal, vec![0]).unwrap(),
                make(Opcode::OpReturnValue, vec![]).unwrap()
            ]), 0, 1),
                                     Object::Integer(24)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpCall, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap()
            ]
        },
        CompilerTestCase {
            input: "let manyArg = fn (a,b,c) {a; b; c;}; manyArg(24, 25, 26);".to_string(),
            expected_constants: vec![
                Object::CompiledFunction(
                    Instructions::join(vec![
                        make(Opcode::OpGetLocal, vec![0]).unwrap(),
                        make(Opcode::OpPop, vec![]).unwrap(),
                        make(Opcode::OpGetLocal, vec![1]).unwrap(),
                        make(Opcode::OpPop, vec![]).unwrap(),
                        make(Opcode::OpGetLocal, vec![2]).unwrap(),
                        make(Opcode::OpReturnValue, vec![]).unwrap()
                    ]), 0, 3),
                Object::Integer(24), Object::Integer(25), Object::Integer(26)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpConstant, vec![3]).unwrap(),
                make(Opcode::OpCall, vec![3]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap()
            ]
        }
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_let_statement_scopes() {
    let tests = vec![
        CompilerTestCase {
            input: "let num = 55; fn() {num}".to_string(),
            expected_constants: vec![
                Object::Integer(55),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpGetGlobal, vec![0]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap()
                ]), 0, 0)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpSetGlobal, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap()
            ]
        },
        CompilerTestCase {
            input: "fn() {let num = 55; num}".to_string(),
            expected_constants: vec![
                Object::Integer(55),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpConstant, vec![0]).unwrap(),
                    make(Opcode::OpSetLocal, vec![0]).unwrap(),
                    make(Opcode::OpGetLocal, vec![0]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap()
                ]), 0, 0)],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ]
        },
        CompilerTestCase {
            input: "fn () { let a = 55; let b = 77; a + b }".to_string(),
            expected_constants: vec![
                Object::Integer(55),
                Object::Integer(77),
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpConstant, vec![0]).unwrap(),
                    make(Opcode::OpSetLocal, vec![0]).unwrap(),
                    make(Opcode::OpConstant, vec![1]).unwrap(),
                    make(Opcode::OpSetLocal, vec![1]).unwrap(),
                    make(Opcode::OpGetLocal, vec![0]).unwrap(),
                    make(Opcode::OpGetLocal, vec![1]).unwrap(),
                    make(Opcode::OpAdd, vec![]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap()
                ]), 0, 0)
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![2]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap()
            ]
        }
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_builtins() {
    let tests = vec![
        CompilerTestCase {
            input: "len([]); push([],1);".to_string(),
            expected_constants: vec![Object::Integer(1)],
            expected_instructions: vec![
                make(Opcode::OpGetBuiltin, vec![0]).unwrap(),
                make(Opcode::OpArray, vec![0]).unwrap(),
                make(Opcode::OpCall, vec![1]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
                make(Opcode::OpGetBuiltin, vec![4]).unwrap(),
                make(Opcode::OpArray, vec![0]).unwrap(),
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpCall, vec![2]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap(),
            ]
        },
        CompilerTestCase {
            input: "fn() { len([]) }".to_string(),
            expected_constants: vec![
                Object::CompiledFunction(Instructions::join(vec![
                    make(Opcode::OpGetBuiltin, vec![0]).unwrap(),
                    make(Opcode::OpArray, vec![0]).unwrap(),
                    make(Opcode::OpCall, vec![1]).unwrap(),
                    make(Opcode::OpReturnValue, vec![]).unwrap()
                ]), 0, 0)
            ],
            expected_instructions: vec![
                make(Opcode::OpConstant, vec![0]).unwrap(),
                make(Opcode::OpPop, vec![]).unwrap()
            ]
        }
    ];

    run_compiler_tests(tests)
}

#[test]
fn test_resolve_builtins() {
    let global = Rc::new(Mutex::new(SymbolTable::new()));
    let first_local = Rc::new(Mutex::new(SymbolTable::new_enclosed(global.clone())));
    let second_local = Rc::new(Mutex::new(SymbolTable::new_enclosed(first_local.clone())));

    let expected = vec![
        Symbol { name: "a".to_string(), scope: Scope::BuiltinScope, index: 0 },
        Symbol { name: "c".to_string(), scope: Scope::BuiltinScope, index: 1 },
        Symbol { name: "e".to_string(), scope: Scope::BuiltinScope, index: 2 },
        Symbol { name: "f".to_string(), scope: Scope::BuiltinScope, index: 3 },
    ];

    for (i, sym) in (expected.clone()).iter().enumerate() {
        global.lock().unwrap().define_builtin(i, &sym.name);
    }

    for table in [global, first_local, second_local] {
        for exp in &expected {
            if let Some(sym) = table.lock().unwrap().resolve(&exp.name) {
                assert_eq!(*exp, sym, "expected {:?} to resolve to {:?}, got={:?}", exp.name, *exp, sym)
            } else {
                assert!(false, "name {:?} is not resolvable!", exp.name);
            }
        }
    }
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
               &concatted.to_string().unwrap(), &actual.to_string().unwrap());

    for (i, (e, a)) in concatted.0.iter().zip(&actual.0).enumerate() {
        assert_eq!(*e, *a,
                   "Instruction {} does not match!\nwant={} ({:?})\n got={} ({:?})\nInstructions:\n\twant={:?}\n\t got={:?}\n",
                   i, e, Opcode::try_from(*e).unwrap(), a, Opcode::try_from(*a).unwrap(), actual.to_string().unwrap(), concatted.to_string().unwrap());
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
            Object::CompiledFunction(ie, ..) => {
                match a {
                    Object::CompiledFunction(ia, ..) => {
                        test_instructions(vec![ie.clone()], ia);
                    }
                    _ => assert!(false, "Object is not a CompiledFunction! Got {} instead.", a.type_str())
                }
            }
            _ => { assert!(false, "Constants can't be compared.") }
        }
    }
}
