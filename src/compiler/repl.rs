use std::collections::{HashMap, HashSet};
use std::io::Write;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::{compiler::grammar, parser::token_stream::TokenStream};
use crate::parser::combinators::ParserCombinator;

use super::{Program, Type, TypeInfo};

pub fn start_repl() {
    let mut program = Program {
        functions: HashMap::new(),
        types: HashMap::new(),
    };

    //todo: deduplicate, perhaps in type_system
    let primitive_types = HashSet::from([
        "Int".to_string(),
        "String".to_string(),
        "Bool".to_string(),
        "Void".to_string(),
    ]);
    let bool_type = Type::SimpleType("Bool".to_string());
    let void_type = Type::SimpleType("Void".to_string());
    let mut type_info = TypeInfo {
        primitive_types,
        user_types: HashMap::new(),
        function_types: HashMap::from([("print".to_string(), void_type.clone())]),
        constant_types: HashMap::from([
            ("true".to_string(), bool_type.clone()),
            ("false".to_string(), bool_type.clone()),
        ]),
    };
    loop {
        print!("laxma> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).expect("Invalid input!");

        let input = input.trim();
        if input == "quit" || input == "exit" {
            return;
        }

        let tokens: Vec<&str> = input.split_whitespace().collect();
        let tokens = TokenStream::from_str(tokens);
        let result = grammar::parser().parse(&tokens);

        if !result.result || result.remaining.not_done() {
            println!("ERROR: Failed to parse!");
        } else {
            println!("{}\n\n", result.ast);
            let result = to_repr(&result.ast);
            if result.is_ok() {
                let Program { functions, types } = result.unwrap();
                for (name, definition) in types {
                    println!("Defined type {}", name);

                    type_info.user_types.insert(name.clone(), definition.def.to_owned());
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