use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::internal_repr::{
    DestructuringComponent, Expression, FunctionDefinition, Program, Type, TypeDefinition,
    TypeVariant,
};

#[derive(Debug, Clone)]
pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<String>,
    pub(super) user_types: HashMap<Rc<String>, Rc<Type>>,
    //todo: function_types and constant_types could be part of the same map
    pub(super) function_types: HashMap<Rc<String>, Rc<Type>>,
    pub(super) constant_types: HashMap<Rc<String>, Rc<Type>>,
}

impl TypeInfo {
    pub fn new() -> Self {
        let primitive_types = HashSet::from([
            "Int".to_string(),
            "String".to_string(),
            "Bool".to_string(),
            "Void".to_string(),
        ]);
        let bool_type = Rc::new(Type::SimpleType(Rc::new("Bool".to_string())));
        let void_type = Rc::new(Type::SimpleType(Rc::new("Void".to_string())));

        let int_type = Rc::new(Type::SimpleType(Rc::new("Int".to_string())));
        let binary_int_type = Rc::new(Type::FunctionType(
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&int_type),
        ));
        let int_comparison_type = Rc::new(Type::FunctionType(
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&bool_type),
        ));
        Self {
            primitive_types,
            user_types: HashMap::new(),
            function_types: HashMap::from([
                (
                    Rc::new("print".to_string()),
                    Rc::new(Type::FunctionType(
                        vec![Rc::new(Type::TypeParameter(Rc::new("'T".to_string())))],
                        Rc::clone(&void_type),
                    )),
                ),
                (Rc::new("+".to_string()), Rc::clone(&binary_int_type)),
                (Rc::new("-".to_string()), Rc::clone(&binary_int_type)),
                (Rc::new("*".to_string()), Rc::clone(&binary_int_type)),
                (Rc::new("/".to_string()), Rc::clone(&binary_int_type)),
                (Rc::new(">".to_string()), Rc::clone(&int_comparison_type)),
                (Rc::new(">=".to_string()), Rc::clone(&int_comparison_type)),
                (Rc::new("==".to_string()), Rc::clone(&bool_type)),
                (Rc::new("<".to_string()), Rc::clone(&int_comparison_type)),
                (Rc::new("<=".to_string()), Rc::clone(&int_comparison_type)),
            ]),
            constant_types: HashMap::from([
                (Rc::new("true".to_string()), Rc::clone(&bool_type)),
                (Rc::new("false".to_string()), Rc::clone(&bool_type)),
            ]),
        }
    }

    pub fn add_user_type(&mut self, type_name: Rc<String>, type_definition: &TypeDefinition) {
        self.user_types
            .insert(type_name, Rc::clone(&type_definition.def));
    }

    pub fn type_exists(&self, type_name: &Rc<String>) -> bool {
        self.primitive_types.contains(type_name.as_ref()) || self.user_types.contains_key(type_name)
    }
}

fn infer_expression_type(
    program: &Program,
    type_info: &TypeInfo,
    identifier_types: &HashMap<Rc<String>, Rc<Type>>,
    current_function: &FunctionDefinition,
    expression: &Expression,
) -> Result<Rc<Type>, String> {
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
                if !definition.variants.contains_key(variant.as_ref()) {
                    Err(format!("Undefined variant '{variant}' for type '{type_name}' in function '{function_name}'"))
                } else {
                    //todo: should use inferred types of expressions in vec to concretized type parameters
                    Ok(Rc::clone(&result.unwrap().def))
                }
            }
        }
        Expression::FunctionCall(function_call) => {
            let function_type = type_info.function_types.get(&function_call.name);
            if function_type.is_some() {
                Ok(function_type.unwrap().as_return_type())
            } else if current_function.name == function_call.name {
                Ok(Rc::new(Type::Unknown))
            } else {
                let id_type = identifier_types.get(&function_call.name);
                if id_type.is_some() {
                    match id_type.unwrap().as_ref() {
                        Type::FunctionType(_, return_type) => {
                            return Ok(Rc::clone(return_type));
                        }
                        _ => {
                            return Err(format!(
                                "'{}' with type '{} is not callable",
                                function_call.name,
                                id_type.unwrap().name()
                            ));
                        }
                    }
                }

                let function_definition = program.functions.get(&function_call.name);
                if function_definition.is_some() {
                    let function_type =
                        infer_function_type(program, type_info, function_definition.unwrap());
                    if function_type.is_err() {
                        return function_type;
                    }
                    let function_type = function_type.unwrap();
                    return match function_type.as_ref() {
                        Type::FunctionType(_, return_type) => Ok(Rc::clone(&return_type)),
                        _ => Err(format!(
                            "Expected FunctionType but got '{}'",
                            function_type.name()
                        )),
                    };
                }

                let builtin = type_info.function_types.get(&function_call.name);
                if builtin.is_some() {
                    return Ok(builtin.unwrap().as_return_type());
                }

                Err(format!("Function '{}' was not defined", function_call.name))
            }
        }
        Expression::WithBlock(items, expression) => {
            let mut inner_types = identifier_types.clone();
            for (identifier, expr) in items {
                let result =
                    infer_expression_type(program, type_info, &inner_types, current_function, expr);
                if result.is_err() {
                    return result;
                }
                inner_types.insert(Rc::clone(identifier), Rc::clone(&result.unwrap()));
            }
            infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function,
                expression.as_ref(),
            )
        }
        Expression::If(condition, when_true, when_false) => {
            let result = infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function,
                condition,
            );
            if result.is_err() {
                return result;
            }
            let result = result.unwrap();
            match result.as_ref() {
                Type::SimpleType(x) if x.as_str() == "Bool" => {}
                _ => {
                    return Err(format!(
                        "If condition must be boolean but got '{}'",
                        result.name()
                    ));
                }
            }

            let true_type = infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function,
                &when_true,
            );
            if true_type.is_err() {
                return true_type;
            }
            let false_type = infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function,
                &when_false,
            );
            if false_type.is_err() {
                return false_type;
            }

            let true_type = true_type.unwrap();
            let false_type = false_type.unwrap();
            if false_type.is_unknown() || true_type.is_unknown() {
                return Ok(true_type);
            }

            if false_type != true_type {
                return Err(format!(
                    "If branches must have same type but got '{}' and '{}'",
                    true_type.name(),
                    false_type.name(),
                ));
            }

            Ok(true_type)
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
        Expression::Number(_) => Ok(Rc::new(Type::SimpleType(Rc::new("Int".to_string())))),
        Expression::LambdaExpression(function_definition) => {
            infer_function_type(program, type_info, &function_definition)
        }
    }
}

pub fn infer_function_type(
    program: &Program,
    type_info: &TypeInfo,
    current_function: &FunctionDefinition,
) -> Result<Rc<Type>, String> {
    let mut arg_types = HashMap::new();
    for (constant, _type) in &type_info.constant_types {
        arg_types.insert(Rc::clone(constant), Rc::clone(_type));
    }
    for argument in &current_function.arguments {
        match &argument.typing.as_ref() {
            Type::TypeParameter(_) => {}
            Type::SimpleType(_) | Type::ParametrizedType(_, _) => {
                let arg_type = argument.typing.name();
                if !type_info.type_exists(&arg_type) {
                    return Err(format!("'{}' is not a valid type", arg_type));
                }
            }
            Type::Unknown => panic!(
                "Unknown type is invalid for argument '{}' of function '{}'",
                argument.identifier, current_function.name
            ),
            Type::FunctionType(args, return_type) => {
                for arg_type in args {
                    let type_name = arg_type.name();
                    if !type_info.type_exists(&type_name) {
                        return Err(format!("'{}' is not a valid type", type_name.as_ref()));
                    }
                }

                let type_name = return_type.name();
                if !type_info.type_exists(&type_name) {
                    return Err(format!("'{}' is not a valid type", type_name.as_ref()));
                }
            }
        }

        arg_types.insert(Rc::clone(&argument.identifier), Rc::clone(&argument.typing));
    }

    let function_name = &current_function.name;
    let function_type_args = current_function
        .arguments
        .iter()
        .map(|arg| Rc::clone(&arg.typing))
        .collect();
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

        let return_type = result.unwrap();
        if return_type.is_unknown() {
            return Err(format!(
                "Cannot infer return type of function {function_name}"
            ));
        }

        Ok(Rc::new(Type::FunctionType(function_type_args, return_type)))
    } else {
        let mut branch_types = HashSet::new();
        for (destructuring, expression) in &current_function.bodies {
            let mut identifier_types = HashMap::new();
            for (k, v) in &arg_types {
                identifier_types.insert(Rc::clone(k), Rc::clone(v));
            }

            let mut i = 0;
            for component in &destructuring.components {
                let arg = &current_function.arguments[i];
                match component {
                    DestructuringComponent::Identifier(identifier)
                        if identifier.as_str() != "_" =>
                    {
                        identifier_types.insert(
                            Rc::clone(identifier),
                            Rc::clone(arg_types.get(&arg.identifier).unwrap()),
                        );
                    }
                    //todo: don't do this twice in repl maybe?
                    DestructuringComponent::Destructuring(destructuring) => {
                        let result = program.types.get(arg.typing.name().as_ref());
                        if result.is_none() {
                            return Err(format!("Cannot find type '{}' for argument '{}' of function '{function_name}'", arg.typing.name(), arg.identifier));
                        }
                        let variant = result.unwrap().variants.get(&destructuring.0);
                        if variant.is_none() {
                            return Err(format!("Cannot find variant '{}' for type '{}' in function '{function_name}'", destructuring.0, arg.typing.name()));
                        }
                        let variant = variant.unwrap();
                        match variant {
                            TypeVariant::Constant(name) => {
                                if destructuring.1.len() != 0 {
                                    return Err(format!("Variant '{name}' for type '{}' expects 0 components but got {} in function '{function_name}'", arg.typing.name(), destructuring.1.len()));
                                }
                            }
                            TypeVariant::Cartesian(name, items) => {
                                if items.len() != destructuring.1.len() {
                                    return Err(format!("Variant '{name}' for type '{}' expects {} components but got {} in function '{function_name}'", arg.typing.name(), items.len(), destructuring.1.len()));
                                }
                                let mut k = 0;
                                for inner_component in &destructuring.1 {
                                    match inner_component {
                                        DestructuringComponent::Identifier(identifier)
                                            if identifier.as_str() != "_" =>
                                        {
                                            identifier_types
                                                .insert(Rc::clone(identifier), Rc::clone(&items[k]));
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

            let result: Result<Rc<Type>, String> = infer_expression_type(
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
            (1, false) => Ok(Rc::new(Type::FunctionType(
                function_type_args,
                Rc::clone(branch_types.iter().next().unwrap()),
            ))),
            (2, true) => Ok(Rc::new(Type::FunctionType(
                function_type_args,
                Rc::clone(&branch_types
                    .into_iter()
                    .filter(|t| !t.is_unknown())
                    .next()
                    .unwrap()),
            ))),
            //todo: for N types, as long as none is Unknown, we need to check what's the supertype of all of them (common ancestor in hierarchy tree)
            _ => Err(format!(
                "Function '{function_name}' has matched patterns with different types: {:?}",
                branch_types
            )),
        }
    }
}
