use crate::ast::{Parser, Program};
use crate::compiler::Compiler;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::{Environment, Object};
use crate::vm::VM;
use std::rc::Rc;
use std::sync::Mutex;
use std::time::Instant;

const INPUT: &'static str = "let fibonacci = fn (x) { if (x == 0){ return 0; } else { if (x == 1) {return 1;} else {return fibonacci(x - 1) + fibonacci(x - 2);}}};fibonacci(15);";

pub(crate) fn fib_benchmark() {
    println!("Starting Fibonacci Benchmark...");
    let lexer = Lexer::new(INPUT).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();

    println!("Running Compiler Benchmark...");
    let comp_start = Instant::now();
    let comp_result = run_compiled(program.clone());
    let comp_elapsed = comp_start.elapsed();
    println!("Compiler results: {}ns\t{:?}\n", comp_elapsed.as_nanos(), comp_result);

    println!("Running Interpreter Benchmark...");
    let int_start = Instant::now();
    let _int_result = run_interpreted(program.clone());
    let int_elapsed = int_start.elapsed();
    println!("Taking Time Done.");

    println!("=== Benchmark Results===\n\n{}\n\n\
    Compiled Run:\t\t{}ns\n\
    Interpreted Run:\t{}ns\n",
             program.to_string(),
             comp_elapsed.as_nanos(),
             int_elapsed.as_nanos())
}

fn run_compiled(program: Program) -> Object {
    let mut comp = Compiler::new();
    comp.compile_program(&program).unwrap();
    let mut vm = VM::new(comp.bytecode().unwrap());
    vm.run().unwrap();
    vm.last_popped_stack_elem().unwrap()
}

fn run_interpreted(program: Program) -> Object {
    let env = Rc::new(Mutex::new(Environment::new()));
    eval_program(program, env).unwrap()
}
