mod token;
mod lexer;
mod repl;
mod ast;
mod object;
mod evaluator;

fn main() {
    repl::start();
}
