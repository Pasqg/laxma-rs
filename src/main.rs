use std::io::{Read, Write};
use std::fs::{metadata, read_dir};
use std::time::Instant;

use compiler::lexer::Lexer;
use compiler::repl::REPL;

mod compiler;
mod parser;
mod recur;
mod utils;

const LOAD_COMMAND: &'static str = "/load";

fn execute_input(repl: &mut REPL, input: &str) {
    let mut tokens = Lexer::token_stream(input);
    while tokens.not_done() && &tokens.peek().unwrap_str() == LOAD_COMMAND {
        let (_, remaining) = tokens.advance();
        if remaining.not_done() {
            let (file_name, remaining) = remaining.advance();
            load_file(repl, format!("{}.m", &file_name.unwrap_str()).as_str());
            tokens = remaining;
        } else {
            tokens = remaining;
        }
    }

    let result = repl.handle_tokens(&tokens);
    if result.is_err() {
        println!("ERROR: {:?}", result.unwrap_err());
    }
}

fn load_file(repl: &mut REPL, file_name: &str) {
    let file = std::fs::File::open(file_name);
    if file.is_err() {
        println!("ERROR for '{}': {}", file_name, file.unwrap_err());
    } else {
        if metadata(file_name).unwrap().is_dir() {
            for path in read_dir(file_name).unwrap() {
                load_file(repl, path.unwrap().path().to_str().unwrap());
            }
        } else {
            let mut content = String::new();
            let result = file.unwrap().read_to_string(&mut content);
            if result.is_err() {
                println!("ERROR for '{}': {}", file_name, result.unwrap_err());
            } else {
                execute_input(repl, &content);
            }
        }
    }
}

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
        if input == "/quit" || input == "/exit" {
            return;
        }

        let start = Instant::now();
        execute_input(&mut repl, input);
        println!("Done in {}ms", start.elapsed().as_millis());
    }
}
