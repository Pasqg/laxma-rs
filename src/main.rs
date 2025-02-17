use std::io::{Read, Write};
use std::fs::{metadata, read_dir};

use compiler::repl::REPL;

mod compiler;
mod parser;
mod recur;
mod utils;

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
                repl.handle_input(&content);
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
        if !input.starts_with("/load ") {
            repl.handle_input(input);
            continue;
        }

        let files = input.split(&[' ', '\t', '\n']);
        for file_name in files.skip(1) {
            let file_name = file_name.trim();
            if !file_name.is_empty() {
                load_file(&mut repl, file_name);
            }
        }
    }
}
