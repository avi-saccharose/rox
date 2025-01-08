use std::io::{self, Write};

use interpreter::Interpreter;

mod error;
mod expr;
mod interpreter;
mod lexer;
mod parser;
mod token;
fn repl() {
    let mut interpreter = Interpreter::new();
    loop {
        let mut line = String::new();
        print!(">> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read line");
        let lexed = lexer::lex(&line);
        if lexed.is_err() {
            eprintln!("Lexing error {}", lexed.err().unwrap());
            continue;
        }

        let ast = parser::parse(lexed.unwrap());

        if ast.is_err() {
            eprintln!("Parsing error {}", ast.err().unwrap());
            continue;
        }

        match interpreter.run(ast.unwrap()) {
            Ok(()) => continue,
            Err(e) => dbg!(e),
        };
    }
}
fn main() {
    repl();
}
