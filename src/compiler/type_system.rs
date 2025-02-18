use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{
    identifier_map::{
        IdentifierId, ADD_ID, BINARY_INT_BOOL_FUNC, BINARY_INT_INT_FUNC, BOOL_ID, DIV_ID, EQ_ID, FALSE_ID, FLOAT_ID, GE_ID, GT_ID, INT_ID, LE_ID, LT_ID, MUL_ID, PRINT_ID, STRING_ID, SUB_ID, TRUE_ID, T_TYPE_PARAM_ID, T_VOID_FUNC, VOID_ID, WILDCARD_ID
    },
    internal_repr::{
        DestructuringComponent, Expression, FunctionDefinition, Program, Type, TypeDefinition,
        TypeVariant,
    },
};

#[derive(Debug, Clone)]
pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<IdentifierId>,
    pub(super) user_types: HashMap<IdentifierId, Rc<Type>>,
    //todo: function_types and constant_types could be part of the same map
    pub(super) function_types: HashMap<IdentifierId, Rc<Type>>,
    pub(super) constant_types: HashMap<IdentifierId, Rc<Type>>,
}

impl TypeInfo {
    pub fn new() -> Self {
        let primitive_types = HashSet::from([INT_ID, STRING_ID, BOOL_ID, VOID_ID, FLOAT_ID]);
        let bool_type = Rc::new(Type::SimpleType(BOOL_ID));
        let void_type = Rc::new(Type::SimpleType(VOID_ID));

        let int_type = Rc::new(Type::SimpleType(INT_ID));
        let binary_int_type = Rc::new(Type::FunctionType(
            BINARY_INT_INT_FUNC,
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&int_type),
        ));
        let int_comparison_type = Rc::new(Type::FunctionType(
            BINARY_INT_BOOL_FUNC,
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&bool_type),
        ));
        Self {
            primitive_types,
            user_types: HashMap::new(),
            function_types: HashMap::from([
                (
                    PRINT_ID,
                    Rc::new(Type::FunctionType(
                        T_VOID_FUNC,
                        vec![Rc::new(Type::TypeParameter(T_TYPE_PARAM_ID))],
                        Rc::clone(&void_type),
                    )),
                ),
                (ADD_ID, Rc::clone(&binary_int_type)),
                (SUB_ID, Rc::clone(&binary_int_type)),
                (MUL_ID, Rc::clone(&binary_int_type)),
                (DIV_ID, Rc::clone(&binary_int_type)),
                (GT_ID, Rc::clone(&int_comparison_type)),
                (GE_ID, Rc::clone(&int_comparison_type)),
                (EQ_ID, Rc::clone(&bool_type)),
                (LT_ID, Rc::clone(&int_comparison_type)),
                (LE_ID, Rc::clone(&int_comparison_type)),
            ]),
            constant_types: HashMap::from([
                (TRUE_ID, Rc::clone(&bool_type)),
                (FALSE_ID, Rc::clone(&bool_type)),
            ]),
        }
    }

    pub fn add_user_type(&mut self, type_id: IdentifierId, type_definition: &TypeDefinition) {
        self.user_types
            .insert(type_id, Rc::clone(&type_definition.def));
    }

    pub fn type_exists(&self, type_id: &IdentifierId) -> bool {
        self.primitive_types.contains(type_id) || self.user_types.contains_key(type_id)
    }
}

fn infer_expression_type(
    program: &mut Program,
    type_info: &TypeInfo,
    identifier_types: &HashMap<IdentifierId, Rc<Type>>,
    current_function: &FunctionDefinition,
    expression: &Expression,
) -> Result<Rc<Type>, String> {
    match expression {
        Expression::TypeConstructor(type_id, variant, expressions) => {
            let function_id = &current_function.id;

            let type_variant = {
                let result = program.types.get(type_id);
                if result.is_none() {
                    return Err(format!(
                        "Undefined type '{}' in function '{}'",
                        program.var_name(type_id),
                        program.var_name(function_id),
                    ));
                }

                let definition = result.unwrap();

                let type_variant = definition.variants.get(variant);
                if type_variant.is_none() {
                    return Err(format!(
                        "Undefined variant '{}' for type '{}' in function '{}'",
                        program.var_name(variant),
                        program.var_name(type_id),
                        program.var_name(function_id)
                    ));
                }
                let type_variant = type_variant.unwrap();

                let arg_num = match type_variant.as_ref() {
                    TypeVariant::Constant(_) => 0,
                    TypeVariant::Cartesian(_, items) => items.len(),
                };
                if arg_num != expressions.len() {
                    return Err(format!("Variant '{}' of type '{}' expects {arg_num} arguments but constructor provided {}",
                    program.var_name(variant),
                    program.var_name(type_id),
                    expressions.len()));
                }

                //necessary to avoid mut borrowing of program while another borrowing is active
                //the borrowing (variant as internal state of program) is dropped after the end of this block
                Rc::clone(type_variant)
            };

            if expressions.len() > 0 {
                match type_variant.as_ref() {
                    TypeVariant::Cartesian(variant, items) => {
                        for i in 0..expressions.len() {
                            let expression_type = infer_expression_type(
                                program,
                                type_info,
                                identifier_types,
                                current_function,
                                &expressions[i],
                            );
                            if expression_type.is_err() {
                                return expression_type;
                            }

                            if expression_type.unwrap() != items[i] {
                                return Err(format!(
                                    "Mismatching type in constructor for {}::{variant}",
                                    program.var_name(type_id),
                                ));
                            }
                        }
                    }
                    _ => panic!("not possible"),
                };
            }

            //todo: should use inferred types of expressions in vec to concretized type parameters
            Ok(Rc::clone(&program.types.get(type_id).unwrap().def))
        }
        Expression::FunctionCall(function_call) => {
            let function_type = type_info.function_types.get(&function_call.id);
            if function_type.is_some() {
                Ok(function_type.unwrap().as_return_type())
            } else if current_function.id == function_call.id {
                Ok(Rc::new(Type::Unknown))
            } else {
                let id_type = identifier_types.get(&function_call.id);
                if id_type.is_some() {
                    match id_type.unwrap().as_ref() {
                        Type::FunctionType(_, _, return_type) => {
                            return Ok(Rc::clone(return_type));
                        }
                        _ => {
                            return Err(format!(
                                "'{}' with type '{} is not callable",
                                program.var_name(&function_call.id),
                                program.var_name(&id_type.unwrap().id())
                            ));
                        }
                    }
                }

                let function_definition = program.functions.get(&function_call.id);
                if function_definition.is_some() {
                    let function_type = infer_function_type(
                        program,
                        type_info,
                        &Rc::clone(function_definition.unwrap()),
                    );
                    if function_type.is_err() {
                        return function_type;
                    }
                    let function_type = function_type.unwrap();
                    return match function_type.as_ref() {
                        Type::FunctionType(_, _, return_type) => Ok(Rc::clone(&return_type)),
                        _ => Err(format!(
                            "Expected FunctionType but got '{}'",
                            program.var_name(&function_type.id())
                        )),
                    };
                }

                let builtin = type_info.function_types.get(&function_call.id);
                if builtin.is_some() {
                    return Ok(builtin.unwrap().as_return_type());
                }

                Err(format!(
                    "Function '{}' was not defined",
                    program.var_name(&function_call.id)
                ))
            }
        }
        Expression::WithBlock(items, expression) => {
            let mut inner_types = identifier_types.clone();
            for (id, expr) in items {
                let result =
                    infer_expression_type(program, type_info, &inner_types, current_function, expr);
                if result.is_err() {
                    return result;
                }
                inner_types.insert(*id, Rc::clone(&result.unwrap()));
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
                Type::SimpleType(x) if *x == BOOL_ID => {}
                _ => {
                    return Err(format!(
                        "If condition must be boolean but got '{}'",
                        program.var_name(&result.id())
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
                    program.var_name(&true_type.id()),
                    program.var_name(&false_type.id()),
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
                    program.var_name(&current_function.id),
                    identifier_types,
                ))
            }
        }
        Expression::Integer(_) => Ok(Rc::new(Type::SimpleType(INT_ID))),
        Expression::Float(_) => Ok(Rc::new(Type::SimpleType(FLOAT_ID))),
        Expression::LambdaExpression(function_definition) => {
            infer_function_type(program, type_info, &function_definition)
        }
    }
}

pub fn infer_function_type(
    program: &mut Program,
    type_info: &TypeInfo,
    current_function: &FunctionDefinition,
) -> Result<Rc<Type>, String> {
    let mut arg_types = HashMap::new();
    for (constant, _type) in &type_info.constant_types {
        arg_types.insert(*constant, Rc::clone(_type));
    }
    for argument in &current_function.arguments {
        match &argument.typing.as_ref() {
            Type::TypeParameter(_) => {}
            Type::SimpleType(_) | Type::ParametrizedType(_, _) => {
                let id = argument.typing.id();
                if !type_info.type_exists(&id) {
                    return Err(format!("'{}' is not a valid type", program.var_name(&id)));
                }
            }
            Type::Unknown => panic!(
                "Unknown type is invalid for argument '{}' of function '{}'",
                argument.identifier, current_function.id
            ),
            Type::FunctionType(_, args, return_type) => {
                for arg_type in args {
                    if !type_info.type_exists(&arg_type.id()) {
                        return Err(format!(
                            "'{}' is not a valid type",
                            program.var_name(&arg_type.id())
                        ));
                    }
                }

                let return_id = return_type.id();
                if !type_info.type_exists(&return_id) {
                    return Err(format!(
                        "'{}' is not a valid type",
                        program.var_name(&return_id)
                    ));
                }
            }
        }

        arg_types.insert(argument.identifier, Rc::clone(&argument.typing));
    }

    let function_id = &current_function.id;
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
                "Cannot infer return type of function {}",
                program.var_name(function_id),
            ));
        }

        Ok(Rc::new(Type::create_function_type(
            &mut program.identifier_id_map,
            function_type_args,
            return_type,
        )))
    } else {
        let mut branch_types = HashSet::new();
        for (destructuring, expression) in &current_function.bodies {
            let mut identifier_types = HashMap::new();
            for (k, v) in &arg_types {
                identifier_types.insert(*k, Rc::clone(v));
            }

            let mut i = 0;
            for component in &destructuring.components {
                let arg = &current_function.arguments[i];
                match component {
                    DestructuringComponent::Identifier(identifier)
                        if *identifier != WILDCARD_ID =>
                    {
                        identifier_types.insert(
                            *identifier,
                            Rc::clone(arg_types.get(&arg.identifier).unwrap()),
                        );
                    }
                    //todo: don't do this twice in repl maybe?
                    DestructuringComponent::Destructuring(destructuring) => {
                        let result = program.types.get(&arg.typing.id());
                        if result.is_none() {
                            return Err(format!(
                                "Cannot find type '{}' for argument '{}' of function '{}'",
                                program.var_name(&arg.typing.id()),
                                program.var_name(&arg.identifier),
                                program.var_name(&function_id)
                            ));
                        }
                        let variant = result.unwrap().variants.get(&destructuring.0);
                        if variant.is_none() {
                            return Err(format!(
                                "Cannot find variant '{}' for type '{}' in function '{}'",
                                program.var_name(&destructuring.0),
                                program.var_name(&arg.typing.id()),
                                program.var_name(&function_id)
                            ));
                        }
                        let variant = variant.unwrap();
                        match variant.as_ref() {
                            TypeVariant::Constant(name) => {
                                if destructuring.1.len() != 0 {
                                    return Err(format!("Variant '{name}' for type '{}' expects 0 components but got {} in function '{}'", program.var_name(&arg.typing.id()), destructuring.1.len(),
                                    program.var_name(&function_id)));
                                }
                            }
                            TypeVariant::Cartesian(name, items) => {
                                if items.len() != destructuring.1.len() {
                                    return Err(format!("Variant '{name}' for type '{}' expects {} components but got {} in function '{}'", program.var_name(&arg.typing.id()), items.len(), destructuring.1.len(),
                                    program.var_name(&function_id)));
                                }
                                let mut k = 0;
                                for inner_component in &destructuring.1 {
                                    match inner_component {
                                        DestructuringComponent::Identifier(identifier)
                                            if *identifier != WILDCARD_ID =>
                                        {
                                            identifier_types
                                                .insert(*identifier, Rc::clone(&items[k]));
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
                "Cannot infer type of function {}, possibly infinite recursion?",
                program.var_name(function_id),
            )),
            (1, false) => Ok(Rc::new(Type::create_function_type(
                &mut program.identifier_id_map,
                function_type_args,
                Rc::clone(branch_types.iter().next().unwrap()),
            ))),
            (2, true) => Ok(Rc::new(Type::create_function_type(
                &mut program.identifier_id_map,
                function_type_args,
                Rc::clone(
                    &branch_types
                        .into_iter()
                        .filter(|t| !t.is_unknown())
                        .next()
                        .unwrap(),
                ),
            ))),
            //todo: for N types, as long as none is Unknown, we need to check what's the supertype of all of them (common ancestor in hierarchy tree)
            _ => Err(format!(
                "Function '{}' has matched patterns with different types: {:?}",
                program.var_name(function_id),
                branch_types
            )),
        }
    }
}
