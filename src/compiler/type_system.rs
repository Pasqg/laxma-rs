use std::{collections::{HashMap, HashSet}, rc::Rc};

use super::{
    internal_repr::{Expression, FunctionDefinition, Program, Type},
    DestructuringComponent, TypeDefinition,
};

pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<String>,
    pub(super) user_types: HashMap<Rc<String>, Type>,
    pub(super) function_types: HashMap<Rc<String>, Type>,
    pub(super) constant_types: HashMap<Rc<String>, Type>,
}

impl TypeInfo {
    pub fn new() -> Self {
        let primitive_types = HashSet::from([
            "Int".to_string(),
            "String".to_string(),
            "Bool".to_string(),
            "Void".to_string(),
        ]);
        let bool_type = Type::SimpleType("Bool".to_string());
        let void_type = Type::SimpleType("Void".to_string());
        let int_type = Type::SimpleType("Int".to_string());
        Self {
            primitive_types,
            user_types: HashMap::new(),
            function_types: HashMap::from([
                (Rc::new("print".to_string()), void_type.clone()),
                (Rc::new("+".to_string()), int_type.clone()),
                (Rc::new("-".to_string()), int_type.clone()),
                (Rc::new("*".to_string()), int_type.clone()),
                (Rc::new("/".to_string()), int_type.clone()),
            ]),
            constant_types: HashMap::from([
                (Rc::new("true".to_string()), bool_type.clone()),
                (Rc::new("false".to_string()), bool_type.clone()),
            ]),
        }
    }

    pub fn add_user_type(&mut self, type_name: Rc<String>, type_definition: &TypeDefinition) {
        self.user_types
            .insert(type_name, type_definition.def.to_owned());
    }
}

fn infer_expression_type(
    program: &Program,
    type_info: &TypeInfo,
    identifier_types: &HashMap<Rc<String>, Type>,
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
                if function_definition.is_some() {
                    return infer_function_type(program, type_info, function_definition.unwrap());
                }

                let builtin = type_info.function_types.get(&function_call.name);
                if builtin.is_some() {
                    return Ok(builtin.unwrap().to_owned());
                }

                Err(format!("Function '{}' was not defined", function_call.name))
            }
        }
        Expression::WithBlock(items, expression) => {
            let mut inner_types = identifier_types.clone();
            for (identifier, expr) in items {
                let result = infer_expression_type(program, type_info, &inner_types, current_function, expr);
                if result.is_err() {
                    return result;
                }
                inner_types.insert(identifier.clone(), result.unwrap());
            }
            infer_expression_type(program, type_info,  identifier_types, current_function, expression.as_ref())
        }
        Expression::Identifier(var) => {
            let var_type = identifier_types.get(var);
            if var_type.is_some() {
                Ok(var_type.unwrap().to_owned())
            } else {
                Err(format!(
                    "Undefined identifier '{var}' in function '{}'. Known {:?}",
                    current_function.name, identifier_types,
                ))
            }
        }
        Expression::Number(_) => Ok(Type::SimpleType("Int".to_string())),
    }
}

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
                if !type_info.primitive_types.contains(arg_type)
                    && !program.types.contains_key(arg_type)
                {
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
    if current_function.is_not_pattern_matched() {
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
            let mut identifier_types = HashMap::new();
            for (k, v) in &arg_types {
                identifier_types.insert(k.clone(), v.clone());
            }

            let mut i = 0;
            for component in &destructuring.components {
                let arg = &current_function.arguments[i];
                match component {
                    DestructuringComponent::Identifier(identifier) if identifier.as_str() != "_" => {
                        identifier_types.insert(
                            identifier.clone(),
                            arg_types.get(&arg.identifier).unwrap().clone(),
                        );
                    }
                    //todo: don't do this twice in repl maybe?
                    DestructuringComponent::Destructuring(destructuring) => {
                        let result = program.types.get(arg.typing.name());
                        if result.is_none() {
                            return Err(format!("Cannot find type '{}' for argument '{}' of function '{function_name}'", arg.typing.name(), arg.identifier));
                        }
                        let variant = result.unwrap().variants.get(&destructuring.0);
                        if variant.is_none() {
                            return Err(format!("Cannot find variant '{}' for type '{}' in function '{function_name}'", destructuring.0, arg.typing.name()));
                        }
                        let variant = variant.unwrap();
                        match variant {
                            super::TypeVariant::Constant(name) => {
                                if destructuring.1.len() != 0 {
                                    return Err(format!("Variant '{name}' for type '{}' expects 0 components but got {} in function '{function_name}'", arg.typing.name(), destructuring.1.len()));
                                }
                            }
                            super::TypeVariant::Cartesian(name, items) => {
                                if items.len() != destructuring.1.len() {
                                    return Err(format!("Variant '{name}' for type '{}' expects {} components but got {} in function '{function_name}'", arg.typing.name(), items.len(), destructuring.1.len()));
                                }
                                let mut k = 0;
                                for inner_component in &destructuring.1 {
                                    match inner_component {
                                        DestructuringComponent::Identifier(identifier) if identifier.as_str() != "_" => {
                                            identifier_types.insert(identifier.clone(), items[k].clone());
                                        }
                                        _ => {}
                                    }
                                    k += 1;
                                }
                            }
                        }
                    }
                    _ => {}
                }
                i += 1;
            }

            let result: Result<Type, String> = infer_expression_type(
                program,
                type_info,
                &identifier_types,
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
            //todo: for N types, as long as none is Unknown, we need to check what's the supertype of all of them (common ancestor in hierarchy tree)
            _ => Err(format!(
                "Function '{function_name}' has matched patterns with different types: {:?}",
                branch_types
            )),
        }
    }
}
