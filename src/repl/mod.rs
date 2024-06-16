use std::io;
use std::io::Write;
use std::rc::Rc;
use std::sync::Mutex;

use whoami;

use crate::ast::Parser;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::Environment;

pub fn start() {
    
    let env = Rc::new(Mutex::new(Environment::new()));
    
    println!("Hello {}, welcome to the monkey-rs repl!", whoami::username());
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
                    println!("{obj}");
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