use std::process::exit;
use std::rc::Rc;
use std::sync::Mutex;

use argparse;
use argparse::{ArgumentParser, Store, StoreFalse, StoreTrue};

use crate::ast::Parser;
use crate::benchmark::fib_benchmark;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::Environment;

mod token;
mod lexer;
mod repl;
pub(crate) mod ast;
mod object;
mod evaluator;
mod code;
mod compiler;
mod vm;
mod benchmark;

struct Options {
    filename: String,
    use_interpreter: bool,
    benchmark: bool,
}

fn main() {
    let mut opts = Options {
        filename: String::new(),
        use_interpreter: true,
        benchmark: false,
    };

    let mut ap = ArgumentParser::new();
    ap.set_description("This is Monkey-rs, an interpreter for the Monkey programming language.");

    ap.refer(&mut opts.filename)
        .add_option(&["-f", "--file"],
                    Store,
                    "File to interpret. Usually ending on '.mky'",
        );
    ap.refer(&mut opts.use_interpreter)
        .add_option(
            &["-c", "--compile"],
            StoreFalse,
            "Flag for usage of the Monkey-rs compiler.",
        );
    ap.refer(&mut opts.benchmark)
        .add_option(
            &["-b", "--benchmark"],
            StoreTrue,
            "Flag to run benchmark. All other input is ignored.",
        );

    ap.parse_args_or_exit();
    drop(ap);

    if opts.benchmark {
        fib_benchmark();
        return;
    }

    if opts.filename.len() > 0 {
        interpret_file(&opts.filename);
    } else {
        if opts.use_interpreter {
            repl::start_interpreted_repl();
        } else {
            repl::start_compiled_repl();
        }
    }
}

fn interpret_file(filename: &str) {
    let file_content = match std::fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Cold not load Monkey source file!\n{:?}", e);
            exit(1);
        }
    };


    let content = file_content.replace("\n", " ");

    let env: Rc<Mutex<Environment>> = Rc::new(Mutex::new(Environment::new()));

    let lexer = Lexer::new(&content);
    match lexer {
        Ok(lexer) => {
            let mut parser = Parser::new(lexer);
            match parser.parse_program() {
                Ok(program) => {
                    match eval_program(program, env) {
                        Ok(_result) => {
                            // println!("{}", _result);
                            exit(0);
                        }
                        Err(e) => {
                            eprintln!("Evaluation error: {:?}", e);
                            exit(-3);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Parsing error: {:?}", e);
                    exit(-2);
                }
            }
        }
        Err(e) => {
            eprintln!("Lexing error: {:?}", e);
            exit(-1);
        }
    }
}
