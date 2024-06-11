use std::io;
use std::io::Write;
use crate::ast::Parser;
use crate::lexer::Lexer;

use whoami;

pub fn start() {
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
                if let Err(e) = program{
                    eprintln!("Error: {}", &e);
                    for (i, cause) in e.chain().skip(1).enumerate() {
                        eprintln!("\t{}: {}", i, cause);
                    }
                    continue;
                }
                let program = program.unwrap();
                println!("{}", program);
            },
            Err(e) => {
                println!("Error reading input: {:?}", e);
                break;
            }
        }
    }
}