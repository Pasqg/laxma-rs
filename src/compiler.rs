use std::collections::{HashMap, HashSet};

use grammar::Rules;
use internal_repr::{
    to_repr, DestructuringComponent, Expression, FunctionCall, FunctionDefinition, Program, Type,
    TypeDefinition, TypeVariant,
};
use type_system::{infer_function_type, TypeInfo};

use crate::parser::ast::AST;

mod grammar;
mod internal_repr;
pub mod repl;
mod type_system;

fn compile_function_call(function_call: &FunctionCall) -> String {
    let formatted_parameters = function_call
        .parameters
        .iter()
        .map(|expr| compile_expression(expr))
        .collect::<Vec<String>>();
    match function_call.name.as_str() {
        "+" | "-" | "/" | "*" => format!(
            "({})",
            formatted_parameters.join(&format!(") {} (", function_call.name))
        ),
        "print" => format!(
            "println!(\"{}\", {})",
            formatted_parameters
                .iter()
                .map(|_| "{:?}".to_string())
                .collect::<Vec<String>>()
                .join(" "),
            formatted_parameters.join(",")
        ),
        _ => format!(
            "{}({})",
            function_call.name,
            formatted_parameters.join(", ")
        ),
    }
}

pub fn compile_expression(expression: &Expression) -> String {
    match expression {
        Expression::FunctionCall(function_call) => compile_function_call(function_call),
        Expression::TypeConstructor(type_name, constant, expressions) => {
            format!(
                "{}::{}({})",
                type_name,
                constant,
                expressions
                    .iter()
                    .map(|expr| compile_expression(expr))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        Expression::Identifier(var) => format!("{}", var),
        Expression::Number(n) => format!("{}", n),
        Expression::WithBlock(items, expression) => format!(
            "{}\n{}",
            items
                .iter()
                .map(|(id, expr)| format!("let {} = {};", id.to_string(), compile_expression(expr)))
                .collect::<Vec<String>>()
                .join("\n"),
            compile_expression(expression)
        ),
        Expression::If(condition, true_branch, false_branch) => format!(
            "if {} {{ {} }} else {{ {} }}",
            compile_expression(condition),
            compile_expression(true_branch),
            compile_expression(false_branch)
        ),
    }
}

fn compile_function(
    definition: &FunctionDefinition,
    program: &Program,
    type_info: &TypeInfo,
) -> Result<String, String> {
    let function_name = &definition.name;
    let mut type_parameters = HashSet::new();
    let mut args = Vec::new();
    let mut arg_types = HashMap::new();
    for argument in &definition.arguments {
        for parameter in argument.typing.type_parameters() {
            type_parameters.insert(parameter);
        }

        args.push(argument.identifier.clone());
        arg_types.insert(argument.identifier.clone(), argument.typing.clone());
    }

    let return_type = infer_function_type(program, &type_info, definition);
    if return_type.is_err() {
        return Err(return_type.unwrap_err());
    }
    let return_type = return_type.unwrap();

    let body;
    if definition.is_not_pattern_matched() {
        let (_, expression) = &definition.bodies[0];
        body = compile_expression(expression);
    } else {
        let mut patterns = Vec::new();
        for (pattern, expression) in &definition.bodies {
            let expr = compile_expression(expression);

            let mut arg_id = 0;
            let mut pattern_components = Vec::new();
            for pattern_component in &pattern.components {
                let arg_type = definition.arguments[arg_id].typing.name();
                match pattern_component {
                    DestructuringComponent::Identifier(variant) => {
                        let type_definition = program.types.get(arg_type);
                        let constant = if type_definition.is_some() {
                            if !type_definition
                                .unwrap()
                                .variants
                                .contains_key(variant.as_ref())
                            {
                                return Err(format!(
                                    "Variant {variant} doesn't exist for type {arg_type}"
                                ));
                            }
                            format!("{arg_type}::{variant}")
                        } else {
                            variant.to_string()
                        };

                        pattern_components.push(format!(
                            "{constant}{}",
                            if constant != "_" { "()" } else { "" }
                        ));
                    }
                    DestructuringComponent::Number(n) => pattern_components.push(format!("{n}")),
                    DestructuringComponent::Destructuring(destructuring) => {
                        let constant = if program.types.contains_key(arg_type) {
                            format!("{arg_type}::{}", destructuring.0)
                        } else {
                            destructuring.0.clone()
                        };

                        let mut components = Vec::new();
                        for component in &destructuring.1 {
                            match component {
                                DestructuringComponent::Identifier(x) => components.push(x.clone()),
                                _ => return Err(
                                    "Only DestructuringComponent::Identifier is supported for now"
                                        .to_owned(),
                                ),
                            }
                        }
                        pattern_components.push(format!(
                            "{constant}({})",
                            components
                                .iter()
                                .map(|s| s.as_str())
                                .collect::<Vec<&str>>()
                                .join(", "),
                        ));
                    }
                };
                arg_id += 1;
            }
            patterns.push(format!(
                "        ({}) => {expr},",
                pattern_components.join(", ")
            ));
        }
        body = format!(
            "match ({}) {{
{}
        _ => panic!(\"Non-exhaustive pattern matching in function {}\"),
    }}",
            args.iter()
                .map(|arg| {
                    //todo: share with format type
                    match arg_types.get(arg).unwrap() {
                        Type::SimpleType(name) => {
                            if !type_info.primitive_types.contains(name) {
                                format!("*{}", arg)
                            } else {
                                arg.to_string()
                            }
                        }
                        Type::TypeParameter(_) => arg.to_string(),
                        Type::ParametrizedType(_, _) => format!("*{arg}"),
                        Type::Unknown => panic!("Arg type is unknown"),
                    }
                })
                .collect::<Vec<String>>()
                .join(", "),
            patterns.join("\n"),
            function_name
        );
    }

    Ok(format!(
        "
pub fn {function_name}{}({}) -> {} {{
    {body}
}}
        ",
        if type_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                type_parameters
                    .into_iter()
                    .map(|x| x[1..].to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        },
        args.iter()
            .map(|arg| format!(
                "{}: {}",
                arg,
                format_type(arg_types.get(arg).unwrap(), &type_info.primitive_types)
            ))
            .collect::<Vec<String>>()
            .join(", "),
        compile_type(&return_type)
    ))
}

fn compile_type(_type: &Type) -> String {
    match _type {
        Type::SimpleType(name) => name.clone(),
        Type::ParametrizedType(name, vec) => format!(
            "{}<{}>",
            name,
            vec.iter()
                .map(|t| compile_type(t))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        Type::TypeParameter(name) => name[1..].to_string(),
        Type::Unknown => panic!("Unknown is not a valid type"),
    }
}

fn format_type(_type: &Type, primitive_types: &HashSet<String>) -> String {
    match _type {
        Type::SimpleType(name) => {
            if !primitive_types.contains(name) {
                format!("Box<{}>", compile_type(_type))
            } else {
                compile_type(_type)
            }
        }
        Type::TypeParameter(_) => compile_type(_type),
        Type::ParametrizedType(_, _) => format!("Box<{}>", compile_type(_type)),
        Type::Unknown => panic!("Unsupported Uknown type"),
    }
}

fn compile_type_definition(
    definition: &TypeDefinition,
    primitive_types: &HashSet<String>,
) -> Result<String, String> {
    let type_name = match &definition.def {
        Type::SimpleType(name) => name,
        Type::ParametrizedType(_, _) => &compile_type(&definition.def),
        _ => return Err(format!("Unexpected type name {:?}", definition.def)),
    };

    let mut variants = Vec::new();
    for (name, variant) in &definition.variants {
        variants.push(match variant {
            TypeVariant::Constant(_) => format!("    {}(),", name),
            TypeVariant::Cartesian(_, vec) => format!(
                "    {}({}),",
                name,
                vec.iter()
                    .map(|t| format_type(t, primitive_types))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        });
    }

    Ok(format!(
        "
#[derive(Debug)]
pub enum {} {{
{}
}}
    ",
        type_name,
        variants.join("\n")
    ))
}

pub fn compile(ast: &AST<Rules>) -> Result<String, String> {
    let result = to_repr(ast);
    if result.is_err() {
        return Err(result.unwrap_err());
    }

    let program = result.unwrap();
    println!("{:?}", program);

    let mut type_info = TypeInfo::new();
    program
        .types
        .iter()
        .for_each(|(name, def)| type_info.add_user_type(name.clone(), def));

    let mut code = "type Int = i64;\ntype Bool = bool;\ntype Void = ();\n".to_owned();
    for (_, definition) in &program.types {
        let result = compile_type_definition(definition, &type_info.primitive_types);
        if result.is_err() {
            return Err(result.unwrap_err());
        }
        code.push_str(&result.unwrap());
    }

    for (_, definition) in &program.functions {
        let result = compile_function(definition, &program, &type_info);
        if result.is_err() {
            return Err(result.unwrap_err());
        }
        code.push_str(&result.unwrap());
    }

    Ok(code)
}

pub enum List<T> {
    NonEmpty(Box<T>, Box<List<T>>),
    Empty(),
}

fn length<T>(x: List<T>) -> List<T> {
    List::Empty()
}

#[cfg(test)]
mod test {
    use std::fs;

    use crate::compiler::compile;
    use crate::parser::combinators::ParserCombinator;
    use crate::{compiler::grammar::program_parser, parser::token_stream::TokenStream};

    #[test]
    fn compile_test() {
        let code: Vec<&str> = "
        type Optional [ 'T ] -> None | Some 'T
        type List [ 'T ] -> Empty | NonEmpty 'T List [ 'T ]
        type Tree [ 'T ] -> Nil | Node 'T Tree [ 'T ] Tree [ 'T ]
        type IntList -> Empty | List Int IntList
        type Byte -> b0 | b1

        fn is_empty lst : List [ 'T ] =
            Empty -> true
            NonEmpty _ _ -> false

        fn append x : 'T lst : List [ 'T ] ->
            List :: NonEmpty ( x lst )

        fn list_of x : 'T ->
            append ( x List :: Empty ( ) )

        fn length lst : List [ 'T ] -> length_tail ( lst 0 )

        fn length_tail lst : List [ 'T ] acc : Int =
            NonEmpty _ rest , _ -> length_tail ( rest + ( acc 1 ) )
            Empty , _ -> acc

        fn main -> print ( List :: Empty ( ) )

        "
        .split_whitespace()
        .collect();
        let tokens = TokenStream::from_str(code);
        let result = program_parser().parse(&tokens);

        assert!(result.result);
        print!("{}", result.ast);

        let result = compile(&result.ast);
        if result.is_err() {
            panic!("{:?}", result.unwrap_err());
        }

        let code = result.unwrap();
        print!("{}", code);

        fs::write("src/output.rs", code).expect("Unable to write file");
    }
}
