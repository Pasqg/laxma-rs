use std::collections::{HashMap, HashSet};
use std::io::Write;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::parser::combinators::ParserCombinator;
use crate::{compiler::grammar, parser::token_stream::TokenStream};

use super::{Program, TypeInfo};

pub fn start_repl() {
    let mut program = Program {
        functions: HashMap::new(),
        types: HashMap::new(),
    };
    let mut type_info: TypeInfo = TypeInfo::new();

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

        let tokens: Vec<&str> = input.split_whitespace().collect();
        let tokens = TokenStream::from_str(tokens);
        let result = grammar::program_parser().parse(&tokens);

        if !result.result || result.remaining.not_done() {
            println!("ERROR: Failed to parse!");
        } else {
            println!("{}\n\n", result.ast);
            let result = to_repr(&result.ast);
            if result.is_ok() {
                let Program { functions, types } = result.unwrap();
                for (name, definition) in types {
                    println!("Defined type {}", name);

                    type_info.add_user_type(&name, &definition);
                    program.types.insert(name, definition);
                }
                for (name, definition) in functions {
                    let result = infer_function_type(&program, &type_info, &definition);
                    if result.is_err() {
                        println!("ERROR: {}", result.unwrap_err());
                    } else {
                        let return_type = result.unwrap();
                        println!("Defined function {} -> {}", name, return_type.name());

                        program.functions.insert(name.clone(), definition);
                        type_info.function_types.insert(name, return_type);
                    }
                }
            } else {
                println!("ERROR: {}", result.unwrap_err());
            }
        }
    }
}
