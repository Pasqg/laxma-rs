use std::collections::{HashMap, HashSet};

use super::internal_repr::{Expression, FunctionDefinition, Program, Type};

pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<String>,
    pub(super) user_types: HashMap<String, Type>,
    pub(super) function_types: HashMap<String, Type>,
    pub(super) constant_types: HashMap<String, Type>,
}

fn infer_expression_type(
    program: &Program,
    type_info: &TypeInfo,
    identifier_types: &HashMap<String, Type>,
    current_function: &FunctionDefinition,
    expression: &Expression,
) -> Result<Type, String> {
    match expression {
        Expression::TypeConstructor(type_name, variant, vec) => {
            let function_name = &current_function.name;
            let result = program.types.get(type_name);
            if result.is_none() {
                Err(format!(
                    "Undefined type '{type_name}' in function '{function_name}'"
                ))
            } else {
                let definition = result.unwrap();
                if !definition.variants.contains_key(variant) {
                    Err(format!("Undefined variant '{variant}' for type '{type_name}' in function '{function_name}'"))
                } else {
                    //todo: should use inferred types of expressions in vec to concretized type parameters
                    Ok(result.unwrap().def.to_owned())
                }
            }
        }
        Expression::FunctionCall(function_call) => {
            let function_type = type_info.function_types.get(&function_call.name);
            if function_type.is_some() {
                Ok(function_type.unwrap().clone())
            } else if current_function.name == function_call.name {
                Ok(Type::Unknown)
            } else {
                let function_definition = program.functions.get(&function_call.name);
                if function_definition.is_none() {
                    Err(format!("Function '{}' was not defined", function_call.name))
                } else {
                    infer_function_type(program, type_info, function_definition.unwrap())
                }
            }
        }
        Expression::Identifier(var) => {
            let var_type = identifier_types.get(var);
            if var_type.is_some() {
                Ok(var_type.unwrap().to_owned())
            } else {
                Err(format!(
                    "Undefined identifier {var} in function {}. Known {:?}",
                    current_function.name, identifier_types,
                ))
            }
        }
        Expression::Number(_) => Ok(Type::SimpleType("Int".to_string())),
    }
}

//todo: String is probably not the best for a type return
//todo: Perhaps even primitive types should be SimpleTypes?
pub fn infer_function_type(
    program: &Program,
    type_info: &TypeInfo,
    current_function: &FunctionDefinition,
) -> Result<Type, String> {
    let mut arg_types = HashMap::new();
    for (constant, _type) in &type_info.constant_types {
        arg_types.insert(constant.clone(), _type.clone());
    }
    for argument in &current_function.arguments {
        match argument.typing {
            Type::TypeParameter(_) => {}
            Type::SimpleType(_) | Type::ParametrizedType(_, _) => {
                let arg_type = argument.typing.name();
                if !type_info.primitive_types.contains(arg_type) && !program.types.contains_key(arg_type) {
                    return Err(format!("'{}' is not a valid type", arg_type));
                }
            }
            Type::Unknown => panic!(
                "Unknown type is invalid for argument '{}' of function '{}'",
                argument.identifier, current_function.name
            ),
        }

        arg_types.insert(argument.identifier.clone(), argument.typing.clone());
    }

    let function_name = &current_function.name;
    if current_function.bodies.len() == 1 {
        let result = infer_expression_type(
            program,
            type_info,
            &arg_types,
            current_function,
            &current_function.bodies[0].1,
        );
        if result.is_err() {
            return result;
        }

        let _type = result.unwrap();
        if _type != Type::Unknown {
            Ok(_type)
        } else {
            Err(format!("Cannot infer type of function {function_name}"))
        }
    } else {
        let mut branch_types = HashSet::new();
        for (destructuring, expression) in &current_function.bodies {
            //todo: get additional identifier types from destructuring
            //let identifier_types = HashMap::new();

            let result: Result<Type, String> = infer_expression_type(
                program,
                type_info,
                &arg_types,
                current_function,
                &expression,
            );
            if result.is_err() {
                return Err(result.unwrap_err());
            }
            branch_types.insert(result.unwrap());
        }

        match (branch_types.len(), branch_types.contains(&Type::Unknown)) {
            (1, true) => Err(format!(
                "Cannot infer type of function {function_name}, possibly infinite recursion?"
            )),
            (1, false) => Ok(branch_types.iter().next().unwrap().clone()),
            (2, true) => Ok(branch_types
                .into_iter()
                .filter(|t| *t != Type::Unknown)
                .next()
                .unwrap()
                .clone()),
            _ => Err(format!(
                "Function '{function_name}' has matched patterns with different types: {:?}",
                branch_types
            )),
        }
    }
}
