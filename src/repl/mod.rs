use std::io;
use std::io::Write;
use std::rc::Rc;
use std::sync::Mutex;

use anyhow::Context;
use whoami;

use crate::ast::Parser;
use crate::compiler::symbol_table::SymbolTable;
use crate::compiler::Compiler;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::{Environment, Object};
use crate::vm::{GLOBALS_SIZE, VM};

pub fn start_interpreted_repl() {
    let env = Rc::new(Mutex::new(Environment::new()));

    println!("Hello {}, welcome to the interpreted monkey-rs repl!\
    \nFeel free to type any Monkey statements following the '>>'-prompt.\n", whoami::username());
    loop {
        let mut input = String::new();
        print!(">> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let lexer = Lexer::new(&input).unwrap();
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                if let Err(e) = program {
                    eprintln!("Parsing error:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{}: {}", i, cause);
                    }
                    continue;
                }
                let program = program.unwrap();
                let evaluated = eval_program(program, env.clone());
                if let Ok(obj) = evaluated {
                    if obj != Object::Null {
                        println!("{obj}");
                    }
                } else if let Err(e) = evaluated {
                    eprintln!("Evaluation error:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{i}: {cause}");
                    }
                    continue;
                }
            }
            Err(e) => {
                println!("Error reading input: {:?}", e);
                break;
            }
        }
    }
}

pub fn start_compiled_repl() {
    let symbol_table = Rc::new(Mutex::new(SymbolTable::new()));
    let constants = Rc::new(Mutex::new(Vec::new()));
    const NULL: Object = Object::Null;
    let globals = Rc::new(Mutex::new(vec![NULL; GLOBALS_SIZE].into_boxed_slice()));

    println!("Hello {}, welcome to the compiled monkey-rs repl!\
    \nFeel free to type any Monkey statements following the '>>'-prompt.\n", whoami::username());
    loop {
        let mut input = String::new();
        print!(">> ");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let lexer = Lexer::new(&input).unwrap();
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                if let Err(e) = program {
                    eprintln!("Parsing error:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{}: {}", i, cause);
                    }
                    continue;
                }
                let program = program.unwrap();
                let mut comp = Compiler::with_state(symbol_table.clone(), constants.clone());
                if let Err(e) = comp.compile_program(&program) {
                    eprintln!("Compilation failed:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{i}: {cause}");
                    }
                    continue;
                }
                let mut machine = VM::with_global_store(
                    comp.bytecode()
                        .context("Turning compiled code into bytecode.").unwrap(),
                    globals.clone(),
                );
                if let Err(e) = machine.run() {
                    eprintln!("Executing bytecode failed:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{i}: {cause}");
                    }
                    continue;
                }
                let stack_top = machine.last_popped_stack_elem();
                if let Err(e) = stack_top {
                    eprintln!("Unable to pop top stack element:");
                    for (i, cause) in e.chain().enumerate() {
                        eprintln!("\t{i}: {cause}");
                    }
                    continue;
                }
                let evaluated = stack_top.unwrap();


                if evaluated != Object::Null {
                    println!("{evaluated}");
                }
            }
            Err(e) => {
                println!("Error reading input: {:?}", e);
                break;
            }
        }
    }
}
