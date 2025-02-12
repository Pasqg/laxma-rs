use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::parser::combinators::ParserCombinator;
use crate::{compiler::grammar, parser::token_stream::TokenStream};

use super::internal_repr::{expression_repr, Pattern};
use super::{DestructuringComponent, Expression, Program, Type, TypeInfo, TypeVariant};

#[derive(Clone, Debug)]
pub enum Value {
    Typed(String, String, Vec<Value>),
    Num(i64),
    Bool(bool),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Typed(name, variant, values) => write!(
                f,
                "{name}::{variant}({})",
                values
                    .iter()
                    .map(|val| format!("{val}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Num(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Void => write!(f, "Void"),
        }
    }
}

fn value_to_str(value: &Value) -> Result<String, String> {
    match value {
        Value::Typed(name, variant, values) => {
            let mut args = Vec::new();
            for value in values {
                let result = value_to_str(value);
                if result.is_err() {
                    return result;
                }
                args.push(result.unwrap());
            }
            Ok(format!("{name}::{variant}({})", args.join(", ")))
        }
        Value::Num(x) => Ok(format!("{x}")),
        Value::Bool(x) => Ok(format!("{x}")),
        Value::Void => Err("Void".to_string()),
    }
}

#[derive(Clone, Debug)]
struct PatternMatchResult {
    is_match: bool,
    bindings: HashMap<String, Value>,
}

impl PatternMatchResult {
    fn no_match() -> Self {
        Self {
            is_match: false,
            bindings: HashMap::new(),
        }
    }

    fn with_match(bindings: HashMap<String, Value>) -> Self {
        Self {
            is_match: true,
            bindings,
        }
    }
}

fn pattern_match(
    function_name: &str,
    pattern: &Pattern,
    args: &Vec<Value>,
) -> Result<PatternMatchResult, String> {
    if pattern.components.len() != args.len() {
        return Err(format!(
            "Found pattern with {} elements but function '{function_name}' has {} arguments",
            pattern.components.len(),
            args.len()
        ));
    }

    let mut bindings = HashMap::new();
    for i in 0..pattern.components.len() {
        let element = &pattern.components[i];
        let arg = &args[i];
        match element {
            // Should also add binding of identifier to value!
            DestructuringComponent::Identifier(identifier) => match arg {
                Value::Typed(name, variant, values) => {
                    if identifier.as_str() != variant.as_str() {
                        return Ok(PatternMatchResult::no_match());
                    }

                    if !values.is_empty() {
                        return Err(format!("Cannot destructure variant '{variant}' of type '{name}' with zero elements, because constructor requires {} arguments", values.len()));
                    }
                }
                //todo: think about whether to allow this. Might be nicer to force use of "_" instead of re-binding
                Value::Num(_) => {
                    //todo: multiple "_" should also be considered wildcard
                    if identifier.as_str() != "_" {
                        bindings.insert(identifier.to_owned(), arg.clone());
                    }
                }
                Value::Bool(bool_val) => {
                    let bool_id = identifier.as_str();
                    if (bool_id == "true" && !bool_val) || (bool_id == "false" && *bool_val) {
                        return Ok(PatternMatchResult::no_match());
                    }
                }
                Value::Void => return Err(format!("Arg is Void")),
            },
            DestructuringComponent::Destructuring(destructuring) => match arg {
                Value::Typed(name, variant, values) => {
                    if variant.as_str() != destructuring.0.as_str() {
                        return Ok(PatternMatchResult::no_match());
                    }

                    if values.len() != destructuring.1.len() {
                        return Err(format!("Cannot destructure variant '{variant}' of type '{name}' in {} elements, because constructor requires {} arguments", values.len(), destructuring.1.len()));
                    }

                    for i in 0..values.len() {
                        let value = &values[i];
                        let pattern = &destructuring.1[i];

                        match (pattern, value) {
                            (DestructuringComponent::Identifier(i), _) if i.as_str() == "_" => {}
                            (
                                DestructuringComponent::Identifier(identifier),
                                Value::Typed(name, variant, values),
                            ) => {
                                if identifier.as_str() != variant.as_str() {
                                    return Ok(PatternMatchResult::no_match());
                                }

                                if !values.is_empty() {
                                    return Err(format!("Cannot destructure variant '{variant}' of type '{name}' with zero elements, because constructor requires {} arguments", values.len()));
                                }
                            }
                            (DestructuringComponent::Number(x), Value::Num(y)) => {
                                if x != y {
                                    return Ok(PatternMatchResult::no_match());
                                }
                            }
                            (DestructuringComponent::Identifier(i), Value::Num(_)) => {
                                bindings.insert(i.to_owned(), value.clone());
                            }
                            (DestructuringComponent::Identifier(x), Value::Bool(y)) => {
                                if (x.as_str() == "true" && !y) || (x.as_str() == "false" && *y) {
                                    return Ok(PatternMatchResult::no_match());
                                }
                                bindings.insert(x.to_owned(), value.clone());
                            }
                            _ => return Err("Unsupported".to_string()),
                        }
                    }
                }
                Value::Num(_) => return Err(format!("Cannot destructure Int")),
                Value::Bool(_) => return Err(format!("Cannot destructure Bool")),
                Value::Void => return Err(format!("Arg is Void")),
            },
            DestructuringComponent::Number(pattern_val) => match arg {
                Value::Typed(name, _, _) => {
                    return Err(format!("Type '{name}' cannot be matched to Int"));
                }
                Value::Num(arg_val) => {
                    if pattern_val != arg_val {
                        return Ok(PatternMatchResult::no_match());
                    }
                }
                Value::Bool(_) => return Err(format!("Bool cannot be matched to Int")),
                Value::Void => return Err(format!("Arg is Void")),
            },
        }
    }
    return Ok(PatternMatchResult::with_match(bindings));
}

fn evaluate_expression(
    identifier_values: &HashMap<String, Value>,
    program: &Program,
    type_info: &TypeInfo,
    expression: &Expression,
) -> Result<Value, String> {
    match expression {
        Expression::TypeConstructor(name, variant, expressions) => {
            if !type_info.user_types.contains_key(name) {
                return Err(format!("Type '{name}' is not defined"));
            }

            let type_definition = program.types.get(name).unwrap();
            if !type_definition.variants.contains_key(variant) {
                return Err(format!("Type '{name}' doesn't have variant '{variant}'"));
            }

            let type_variant = type_definition.variants.get(variant).unwrap();
            let arg_num = match type_variant {
                TypeVariant::Constant(_) => 0,
                TypeVariant::Cartesian(_, items) => items.len(),
            };
            if arg_num != expressions.len() {
                return Err(format!("Variant '{variant}' of type '{name}' expects {arg_num} arguments but constructor provided {}", expressions.len()));
            }

            let mut values = Vec::new();
            for expr in expressions {
                let result = evaluate_expression(identifier_values, program, type_info, expr);
                if result.is_err() {
                    return result;
                }

                values.push(result.unwrap());
            }
            if values.len() > 0 {
                match type_variant {
                    TypeVariant::Cartesian(variant, items) => {
                        for i in 0..values.len() {
                            match (&values[i], &items[i]) {
                                (Value::Typed(provided, _, _), Type::SimpleType(expected)) => {
                                    if expected.as_str() != provided.as_str() {
                                        return Err(format!("Expected '{expected}' in parameter {i} for {name}::{variant} but got '{provided}'"));
                                    }
                                }
                                (Value::Num(_), Type::SimpleType(type_name)) => {
                                    if type_name.as_str() != "Int" {
                                        return Err(format!("Expected '{type_name}' in parameter {i} for {name}::{variant} but got 'Int'"));
                                    }
                                }
                                (Value::Bool(_), Type::SimpleType(type_name)) => {
                                    if type_name.as_str() != "Bool" {
                                        return Err(format!("Expected '{type_name}' in parameter {i} for {name}::{variant} but got 'Bool'"));
                                    }
                                }
                                _ => {
                                    return Err(format!(
                                        "Mismatching type in constructor for {name}::{variant}"
                                    ));
                                }
                            }
                        }
                    }
                    _ => panic!("not possible"),
                };
            }
            Ok(Value::Typed(name.clone(), variant.clone(), values))
        }
        Expression::FunctionCall(function_call) => match function_call.name.as_str() {
            "+" | "-" | "/" | "*" => {
                if function_call.parameters.len() < 2 {
                    return Err(format!(
                        "Function '{}' requires at least 2 parameters but got '{}'",
                        function_call.name,
                        function_call.parameters.len()
                    ));
                }

                let mut values = Vec::new();
                let mut i = 1;
                for param in &function_call.parameters {
                    let result = evaluate_expression(identifier_values, program, type_info, param);
                    if result.is_err() {
                        return result;
                    }
                    match result.unwrap() {
                        Value::Num(x) => values.push(x),
                        _ => {
                            return Err(format!(
                                "Argument {i} of function '{}' is not numeric",
                                function_call.name
                            ))
                        }
                    }
                    i += 1;
                }
                match function_call.name.as_str() {
                    "+" => Ok(Value::Num(
                        values.into_iter().reduce(|acc, x| acc + x).unwrap(),
                    )),
                    "-" => Ok(Value::Num(
                        values.into_iter().reduce(|acc, x| acc - x).unwrap(),
                    )),
                    "*" => Ok(Value::Num(
                        values.into_iter().reduce(|acc, x| acc * x).unwrap(),
                    )),
                    "/" => Ok(Value::Num(
                        values.into_iter().reduce(|acc, x| acc / x).unwrap(),
                    )),
                    _ => panic!("Unhandled case"),
                }
            }
            "print" => {
                let mut values = Vec::new();
                let mut i = 1;
                for param in &function_call.parameters {
                    let result = evaluate_expression(identifier_values, program, type_info, param);
                    if result.is_err() {
                        return result;
                    }
                    match value_to_str(&result.unwrap()) {
                        Ok(val) => values.push(val),
                        Err(err) => return Err(format!("Cannot print argument {i}: {err}")),
                    }
                    i += 1;
                }
                println!("{}", values.join(" "));
                Ok(Value::Void)
            }
            _ => {
                if !program.functions.contains_key(&function_call.name) {
                    return Err(format!("Function '{}' is not defined", function_call.name));
                }

                let definition = program.functions.get(&function_call.name).unwrap();

                let expected_arg_num = definition.arguments.len();
                let actual_arg_num = function_call.parameters.len();
                if expected_arg_num != actual_arg_num {
                    return Err(format!(
                        "Function '{}' expects {} arguments, but {} were provided",
                        function_call.name, expected_arg_num, actual_arg_num
                    ));
                }

                let mut arg_values = identifier_values.clone();
                let mut ordered_arg_values = Vec::new();
                for i in 0..actual_arg_num {
                    // Evaluate parameter value
                    let result = evaluate_expression(
                        identifier_values,
                        program,
                        type_info,
                        &function_call.parameters[i],
                    );
                    if result.is_err() {
                        return result;
                    }

                    // Verify type matches
                    let value = result.unwrap();
                    let arg_name = &definition.arguments[i].identifier;
                    let arg_type = &definition.arguments[i].typing;
                    match &value {
                        Value::Typed(name, variant, values) => {
                            match arg_type {
                                Type::SimpleType(simple_type) => {
                                    if simple_type.as_str() != name {
                                        return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", arg_name, function_call.name, arg_type.name(), simple_type));
                                    }
                                    if !program.types.contains_key(simple_type) {
                                        return Err(format!("Type '{simple_type}' is not defined"));
                                    }
                                    let definition = program.types.get(simple_type).unwrap();
                                    if !definition.variants.contains_key(variant) {
                                        return Err(format!("Argument '{arg_name}' in function call '{}' has undefined variant '{variant}' for type '{simple_type}'", function_call.name));
                                    }
                                }
                                //todo: should track that the same type is bound to the same type parameter
                                Type::TypeParameter(name) => {}
                                Type::ParametrizedType(name, items) => {}
                                Type::Unknown => {
                                    return Err(format!("Unknown value is not supported for argument '{arg_name}' in function '{}'", function_call.name));
                                }
                            }
                        }
                        Value::Num(_) => match arg_type {
                            Type::SimpleType(name) if name.as_str() == "Int" => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Int' but '{}' was provided", arg_name, function_call.name, arg_type.name()));
                            }
                        },
                        Value::Bool(_) => match arg_type {
                            Type::SimpleType(name) if name.as_str() == "Bool" => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Bool' but '{}' was provided", arg_name, function_call.name, arg_type.name()));
                            }
                        },
                        Value::Void => {
                            return Err(format!(
                                "Void is not a valid argument type in function '{}'",
                                function_call.name
                            ))
                        }
                    };
                    arg_values.insert(arg_name.clone(), value.clone());
                    ordered_arg_values.push(value);
                }

                if definition.is_not_pattern_matched() {
                    return evaluate_expression(
                        &arg_values,
                        program,
                        type_info,
                        &definition.bodies[0].1,
                    );
                }

                for (pattern, expression) in &definition.bodies {
                    let result = pattern_match(&function_call.name, pattern, &ordered_arg_values);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    let result = result.unwrap();
                    if result.is_match {
                        let mut bindings = result.bindings;
                        for (k,v) in &arg_values {
                            bindings.insert(k.clone(), v.clone());
                        }
                        return evaluate_expression(&bindings, program, type_info, expression);
                    }
                }

                //todo: should be caught at compile time
                Err(format!(
                    "Non-exhaustive patterns in function '{}'",
                    function_call.name
                ))
            }
        },
        Expression::Identifier(identifier) => match identifier_values.get(identifier) {
            Some(value) => Ok(value.clone()),
            None => Err(format!(
                "Unknown identifier '{}'  | {:?}",
                identifier, identifier_values
            )),
        },
        Expression::Number(x) => Ok(Value::Num(*x)),
    }
}

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

        //todo: parse should return Result<ParserResult>
        let mut result = grammar::program_parser().parse(&tokens);
        if !result.result || result.remaining.not_done() {
            result = grammar::expression_parser().parse(&tokens);
            println!("{:?}", result);
        }

        if !result.result || result.remaining.not_done() {
            println!("ERROR: Failed to parse! {:?}", result.remaining);
            continue;
        }

        let ast = result.ast;
        println!("{}\n\n", ast);
        let result = to_repr(&ast);
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
            let original_error = result.unwrap_err();
            let result = expression_repr(&ast);
            if result.is_ok() {
                println!("{:?}", result);
                let result = evaluate_expression(
                    &HashMap::from([
                        ("true".to_string(), Value::Bool(true)),
                        ("false".to_string(), Value::Bool(false)),
                    ]),
                    &program,
                    &type_info,
                    &result.unwrap(),
                );
                if result.is_ok() {
                    println!("{}", result.unwrap());
                } else {
                    println!("ERROR: {}", result.unwrap_err());
                }
            } else {
                println!("ERROR(s): {} {}", original_error, result.unwrap_err());
            }
        }
    }
}
