use std::io;
use std::io::Write;
use crate::lexer::Lexer;
use crate::token::TokenType::EOF;

pub(crate) fn start() {
    println!("Welcome to the monkey-rs repl!");
    let mut input = String::new();
    loop {
        print!(">>");
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let mut lexer = Lexer::new(&input).unwrap();
                let mut token = lexer.next_token();
                while token.token_type != EOF{
                    println!("{}", token);
                    token = lexer.next_token();
                }
            },
            Err(e) => {
                println!("Error reading input: {:?}", e);
                break;
            }
        }
    }
}