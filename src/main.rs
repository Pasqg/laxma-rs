use std::io::Write;

use compiler::repl::REPL;

mod compiler;
mod parser;
mod recur;

fn main() {
    let mut repl = REPL::new();
    loop {
        print!("laxma> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .expect("Invalid input!");

        let input = input.trim();
        if input == "quit" || input == "exit" {
            return;
        }

        repl.handle_input(input);
    }
}
