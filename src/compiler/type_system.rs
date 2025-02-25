use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use nohash_hasher::IntMap;

use super::{
    identifier_map::{
        IdentifierId, ADD_ID, BINARY_INT_BOOL_FUNC, BINARY_INT_INT_FUNC, BOOL_ID, DIV_ID, EQ_ID,
        ERROR_ID, FALSE_ID, FLOAT_ID, GE_ID, GT_ID, INT_ID, LE_ID, LIST_ID, LT_ID, MUL_ID,
        PRINTLN_ID, PRINT_ID, RANGE_ID, RANGE_SIGNATURE, STRING_ID, SUB_ID, TRUE_ID, T_BOOL_FUNC,
        T_TYPE_PARAM_ID, T_T_FUNC, T_UNKNOWN_FUNC, T_VOID_FUNC, VOID_ID, WHILE_FUNC_SIGNATURE,
        WHILE_ID, WILDCARD_ID,
    },
    internal_repr::{
        DestructuringComponent, Expression, FunctionDefinition, Program, Type, TypeDefinition,
        TypeVariant,
    },
    utils::to_int_map,
};

#[derive(Debug, Clone)]
pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<IdentifierId>,
    pub(super) user_types: IntMap<IdentifierId, Rc<Type>>,
    //todo: function_types and constant_types could be part of the same map
    pub(super) function_types: IntMap<IdentifierId, Rc<Type>>,
    pub(super) constant_types: Rc<IntMap<IdentifierId, Rc<Type>>>,
}

impl TypeInfo {
    pub fn new() -> Self {
        let primitive_types = HashSet::from([INT_ID, STRING_ID, BOOL_ID, VOID_ID, FLOAT_ID]);
        let bool_type = Rc::new(Type::SimpleType(BOOL_ID));
        let int_type = Rc::new(Type::SimpleType(INT_ID));
        let void_type = Rc::new(Type::SimpleType(VOID_ID));
        let t_type = Rc::new(Type::TypeParameter(T_TYPE_PARAM_ID));
        let unknown_type = Rc::new(Type::Unknown);
        let int_list_type = Rc::new(Type::ParametrizedType(LIST_ID, vec![Rc::clone(&int_type)]));

        let binary_int_type = Rc::new(Type::FunctionType(
            BINARY_INT_INT_FUNC,
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&int_type),
            None,
        ));
        let int_comparison_type = Rc::new(Type::FunctionType(
            BINARY_INT_BOOL_FUNC,
            vec![Rc::clone(&int_type), Rc::clone(&int_type)],
            Rc::clone(&bool_type),
            None,
        ));
        let t_void_type = Rc::new(Type::FunctionType(
            T_VOID_FUNC,
            vec![Rc::clone(&t_type)],
            Rc::clone(&void_type),
            None,
        ));
        let t_bool_type = Rc::new(Type::FunctionType(
            T_BOOL_FUNC,
            vec![Rc::clone(&t_type)],
            Rc::clone(&bool_type),
            None,
        ));
        let t_unknown_type = Rc::new(Type::FunctionType(
            T_UNKNOWN_FUNC,
            vec![Rc::clone(&t_type)],
            Rc::clone(&unknown_type),
            None,
        ));
        let t_t_type = Rc::new(Type::FunctionType(
            T_T_FUNC,
            vec![Rc::clone(&t_type)],
            Rc::clone(&t_type),
            None,
        ));
        Self {
            primitive_types,
            user_types: IntMap::default(),
            function_types: to_int_map(HashMap::from([
                (PRINT_ID, Rc::clone(&t_void_type)),
                (PRINTLN_ID, Rc::clone(&t_void_type)),
                (ERROR_ID, Rc::clone(&t_unknown_type)),
                (
                    WHILE_ID,
                    Rc::new(Type::FunctionType(
                        WHILE_FUNC_SIGNATURE,
                        vec![
                            Rc::clone(&t_type),
                            Rc::clone(&t_t_type),
                            Rc::clone(&t_bool_type),
                        ],
                        Rc::clone(&t_type),
                        None,
                    )),
                ),
                (
                    RANGE_ID,
                    Rc::new(Type::FunctionType(
                        RANGE_SIGNATURE,
                        vec![Rc::clone(&int_type)],
                        Rc::clone(&int_list_type),
                        None,
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
            ])),
            constant_types: Rc::new(to_int_map(HashMap::from([
                (TRUE_ID, Rc::clone(&bool_type)),
                (FALSE_ID, Rc::clone(&bool_type)),
            ]))),
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

#[derive(Debug)]
struct TypeParameterBindings {
    bindings: IntMap<IdentifierId, Rc<Type>>,
}

impl TypeParameterBindings {
    fn new() -> Self {
        Self {
            bindings: IntMap::default(),
        }
    }

    fn concretize(&self, _type: &Rc<Type>) -> Rc<Type> {
        match _type.as_ref() {
            Type::FunctionType(id, inputs, outputs, captures) => Rc::new(Type::FunctionType(
                *id,
                inputs.iter().map(|t| self.concretize(t)).collect(),
                self.concretize(outputs),
                captures.clone(),
            )),
            Type::TypeParameter(id) => {
                let concrete = self.bindings.get(id);
                if concrete.is_none() {
                    return Rc::clone(_type);
                }
                Rc::clone(concrete.unwrap())
            }
            Type::ParametrizedType(id, items) => Rc::new(Type::ParametrizedType(
                *id,
                items.iter().map(|t| self.concretize(t)).collect(),
            )),
            _ => Rc::clone(_type),
        }
    }

    fn are_compatible(&mut self, first: &Rc<Type>, second: &Rc<Type>) -> bool {
        //todo: this concretization should only work one way! List['T] as argument for List[Int] is not valid

        match (first.as_ref(), second.as_ref()) {
            (Type::Unknown, Type::Unknown) => false,
            (Type::Unknown, _) => true,
            (_, Type::Unknown) => true,
            (Type::TypeParameter(id1), Type::TypeParameter(id2)) => {
                if *id1 == *id2 {
                    return true;
                }

                self.bindings.insert(first.id(), Rc::clone(second));
                self.bindings.insert(second.id(), Rc::clone(first));
                true
            }
            (Type::TypeParameter(_), _) => {
                let binding = self.bindings.get(&first.id());
                if binding.is_some() {
                    return binding.unwrap().id() == second.id();
                }
                self.bindings.insert(first.id(), Rc::clone(second));
                true
            }
            (_, Type::TypeParameter(_)) => {
                let binding = self.bindings.get(&second.id());
                if binding.is_some() {
                    return binding.unwrap().id() == first.id();
                }
                self.bindings.insert(second.id(), Rc::clone(first));
                true
            }
            (
                Type::FunctionType(_, first_inputs, first_output, _),
                Type::FunctionType(_, second_inputs, second_output, _),
            ) => {
                if first_inputs.len() != second_inputs.len() {
                    return false;
                }

                for i in 0..first_inputs.len() {
                    if !self.are_compatible(&first_inputs[i], &second_inputs[i]) {
                        return false;
                    }
                }

                self.are_compatible(first_output, second_output)
            }
            (
                Type::ParametrizedType(id_first, items_first),
                Type::ParametrizedType(id_second, items_second),
            ) => {
                if *id_first != *id_second || items_first.len() != items_second.len() {
                    return false;
                }

                for i in 0..items_first.len() {
                    if !self.are_compatible(&items_first[i], &items_second[i]) {
                        return false;
                    }
                }
                true
            }
            _ => first.id() == second.id(),
        }
    }
}

fn concretize_function_type(
    program: &mut Program,
    type_info: &TypeInfo,
    identifier_types: &Rc<IntMap<IdentifierId, Rc<Type>>>,
    caller_id: &IdentifierId,
    function_id: &IdentifierId,
    function_type: &Rc<Type>,
    arguments: &Vec<Rc<Type>>,
    parameters: &Vec<Expression>,
) -> Result<Rc<Type>, String> {
    let mut type_parameters_bindings = TypeParameterBindings::new();
    if parameters.len() != arguments.len() {
        return Err(format!(
            "Function '{}' in caller '{}' expects {} arguments but {} were provided",
            program.var_name(function_id),
            program.var_name(caller_id),
            arguments.len(),
            parameters.len()
        ));
    }

    for i in 0..parameters.len() {
        let arg_expr = &parameters[i];
        let provided_type =
            infer_expression_type(program, type_info, identifier_types, caller_id, arg_expr)?;

        let arg_type = &arguments[i];
        if !type_parameters_bindings.are_compatible(arg_type, &provided_type) {
            return Err(format!(
                "Argument {} in function '{}' in caller '{}' has type '{}' ('{}') but '{}' ('{}') was provided",
                i,
                program.var_name(function_id),
                program.var_name(caller_id),
                type_parameters_bindings
                    .concretize(arg_type)
                    .full_repr(&program.identifier_id_map),
                arg_type.full_repr(&program.identifier_id_map),
                type_parameters_bindings
                    .concretize(&provided_type)
                    .full_repr(&program.identifier_id_map),
                provided_type.full_repr(&program.identifier_id_map),
            ));
        }
    }

    Ok(type_parameters_bindings
        .concretize(function_type)
        .as_return_type())
}

pub fn verify_type_definition(
    program: &mut Program,
    type_info: &TypeInfo,
    type_definition: &TypeDefinition,
) -> Result<(), String> {
    let type_params = type_definition.def.type_parameters();
    let type_id = type_definition.def.id();
    for (id, variant) in &type_definition.variants {
        match variant.as_ref() {
            TypeVariant::Constant(_) => {}
            TypeVariant::Cartesian(_, types) => {
                for t in types {
                    if t.id() != type_id && !type_info.type_exists(&t.id()) && !type_params.contains(&t.id()) {
                        return Err(format!(
                            "Undefined type '{}' for {}::{}",
                            program.var_name(&t.id()),
                            program.var_name(&type_id),
                            program.var_name(id)
                        ));
                    }
                }
            }
        }
    }

    Ok(())
}

pub fn infer_expression_type(
    program: &mut Program,
    type_info: &TypeInfo,
    identifier_types: &Rc<IntMap<IdentifierId, Rc<Type>>>,
    current_function_id: &IdentifierId,
    expression: &Expression,
) -> Result<Rc<Type>, String> {
    match expression {
        Expression::TypeConstructor(type_id, variant, expressions) => {
            let type_variant = {
                let result = program.types.get(type_id);
                if result.is_none() {
                    return Err(format!(
                        "Undefined type '{}' in function '{}'",
                        program.var_name(type_id),
                        program.var_name(current_function_id),
                    ));
                }

                let definition = result.unwrap();

                let type_variant = definition.variants.get(variant);
                if type_variant.is_none() {
                    return Err(format!(
                        "Undefined variant '{}' for type '{}' in function '{}'",
                        program.var_name(variant),
                        program.var_name(type_id),
                        program.var_name(current_function_id)
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
                        let mut type_parameter_bindings = TypeParameterBindings::new();
                        for i in 0..expressions.len() {
                            let expression_type = infer_expression_type(
                                program,
                                type_info,
                                identifier_types,
                                current_function_id,
                                &expressions[i],
                            )?;

                            if !type_parameter_bindings.are_compatible(&expression_type, &items[i])
                            {
                                return Err(format!(
                                    "Expecting '{}' in constructor for {}::{variant} but got '{}'",
                                    &type_parameter_bindings
                                        .concretize(&items[i])
                                        .full_repr(&program.identifier_id_map),
                                    program.var_name(type_id),
                                    type_parameter_bindings
                                        .concretize(&expression_type)
                                        .full_repr(&program.identifier_id_map),
                                ));
                            }
                        }

                        return Ok(Rc::clone(
                            &type_parameter_bindings
                                .concretize(&program.types.get(type_id).unwrap().def),
                        ));
                    }
                    _ => panic!("BUG"),
                };
            }

            Ok(Rc::clone(&program.types.get(type_id).unwrap().def))
        }
        Expression::FunctionCall(function_call) => {
            let function_type = type_info.function_types.get(&function_call.id);
            if function_type.is_some() {
                let function_type = function_type.unwrap();

                let function_definition = program.functions.get(&function_call.id);
                if function_definition.is_some() {
                    let function_definition = Rc::clone(function_definition.unwrap());
                    concretize_function_type(
                        program,
                        type_info,
                        identifier_types,
                        current_function_id,
                        &function_call.id,
                        function_type,
                        &function_definition
                            .arguments
                            .iter()
                            .map(|arg| Rc::clone(&arg.typing))
                            .collect(),
                        &function_call.parameters,
                    )
                } else {
                    match function_type.as_ref() {
                        Type::FunctionType(_, inputs, _, captures) => {
                            return concretize_function_type(
                                program,
                                type_info,
                                if captures.is_none() {
                                    identifier_types
                                } else {
                                    captures.as_ref().unwrap()
                                },
                                current_function_id,
                                &function_call.id,
                                function_type,
                                &inputs,
                                &function_call.parameters,
                            );
                        }
                        _ => panic!("BUG"),
                    }
                }
            } else if *current_function_id == function_call.id {
                Ok(Rc::new(Type::Unknown))
            } else {
                let id_type = identifier_types.get(&function_call.id);
                if id_type.is_some() {
                    match id_type.unwrap().as_ref() {
                        Type::FunctionType(_, _, return_type, _) => {
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
                    let function_definition = Rc::clone(function_definition.unwrap());
                    let function_type = infer_function_type(
                        program,
                        type_info,
                        &Rc::clone(&function_definition),
                        Some(Rc::clone(identifier_types)),
                    )?;

                    return match function_type.as_ref() {
                        Type::FunctionType(_, _, return_type, _) => Ok(Rc::clone(&return_type)),
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
            let mut inner_types = identifier_types.as_ref().clone();
            for (id, expr) in items {
                let result = infer_expression_type(
                    program,
                    type_info,
                    &Rc::new(inner_types.clone()),
                    current_function_id,
                    expr,
                )?;
                inner_types.insert(*id, Rc::clone(&result));
            }
            infer_expression_type(
                program,
                type_info,
                &Rc::new(inner_types),
                current_function_id,
                expression.as_ref(),
            )
        }
        Expression::If(condition, when_true, when_false) => {
            let result = infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function_id,
                condition,
            )?;

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
                current_function_id,
                &when_true,
            )?;
            let false_type = infer_expression_type(
                program,
                type_info,
                identifier_types,
                current_function_id,
                &when_false,
            )?;

            let is_true_unknown = true_type.is_unknown();
            let is_false_unknown = false_type.is_unknown();
            if is_true_unknown && is_false_unknown {
                return Err(format!(
                    "Could not infer return types of if expression in function '{}'",
                    program.var_name(current_function_id),
                ));
            }
            if is_false_unknown {
                return Ok(true_type);
            }
            if is_true_unknown {
                return Ok(false_type);
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
        Expression::Identifier(id) => {
            let var_type = identifier_types.get(id);
            if var_type.is_some() {
                return Ok(var_type.unwrap().to_owned());
            }

            let function_def = program.functions.get(id);
            if function_def.is_none() {
                return Err(format!(
                    "Undefined identifier '{}' in function '{}'. Known {:?}",
                    program.var_name(id),
                    program.var_name(current_function_id),
                    identifier_types
                        .iter()
                        .map(|(k, v)| (
                            program.var_name(k),
                            v.full_repr(&program.identifier_id_map)
                        ))
                        .collect::<Vec<(&Rc<String>, Rc<String>)>>(),
                ));
            }
            let function_def = Rc::clone(&function_def.unwrap());

            infer_function_type(program, type_info, &function_def, None)
        }
        Expression::Integer(_) => Ok(Rc::new(Type::SimpleType(INT_ID))),
        Expression::String(_) => Ok(Rc::new(Type::SimpleType(STRING_ID))),
        Expression::Float(_) => Ok(Rc::new(Type::SimpleType(FLOAT_ID))),
        Expression::LambdaExpression(function_definition) => infer_function_type(
            program,
            type_info,
            &function_definition,
            Some(Rc::clone(identifier_types)),
        ),
    }
}

pub fn infer_function_type(
    program: &mut Program,
    type_info: &TypeInfo,
    current_function: &FunctionDefinition,
    captures: Option<Rc<IntMap<IdentifierId, Rc<Type>>>>,
) -> Result<Rc<Type>, String> {
    let mut arg_types = type_info.constant_types.as_ref().clone();
    if captures.is_some() {
        for (k, v) in captures.as_ref().unwrap().as_ref() {
            arg_types.insert(*k, Rc::clone(v));
        }
    }

    for argument in &current_function.arguments {
        match &argument.typing.as_ref() {
            Type::TypeParameter(_) => {}
            Type::SimpleType(_) | Type::ParametrizedType(_, _) => {
                let id = argument.typing.id();
                if !type_info.type_exists(&id) {
                    return Err(format!("[1] '{}' doesn't exist", program.var_name(&id)));
                }
            }
            Type::Unknown => panic!(
                "Unknown type is invalid for argument '{}' of function '{}'",
                argument.identifier, current_function.id
            ),
            Type::FunctionType(_, args, return_type, _) => {
                for arg_type in args {
                    if !arg_type.is_type_parameter() && !type_info.type_exists(&arg_type.id()) {
                        return Err(format!(
                            "[2] '{}' doesn't exist",
                            program.var_name(&arg_type.id())
                        ));
                    }
                }

                let return_id = return_type.id();
                if !return_type.is_type_parameter() && !type_info.type_exists(&return_id) {
                    return Err(format!(
                        "[3] '{}' doesn't exist",
                        program.var_name(&return_id)
                    ));
                }
            }
        }

        arg_types.insert(argument.identifier, Rc::clone(&argument.typing));
    }
    let arg_types = Rc::new(arg_types);

    let function_id = &current_function.id;
    let function_type_args = current_function
        .arguments
        .iter()
        .map(|arg| Rc::clone(&arg.typing))
        .collect();
    if current_function.is_not_pattern_matched() {
        let return_type = infer_expression_type(
            program,
            type_info,
            &arg_types,
            &current_function.id,
            &current_function.bodies[0].1,
        )?;

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
            Some(arg_types),
        )))
    } else {
        let mut branch_types = Vec::new();

        let mut type_parameters_bindings = TypeParameterBindings::new();
        let mut all_compatible = true;
        let mut previous_branch_type = None;
        for (destructuring, expression) in &current_function.bodies {
            let mut identifier_types = arg_types.as_ref().clone();

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

            let branch_type = infer_expression_type(
                program,
                type_info,
                &Rc::new(identifier_types),
                &current_function.id,
                &expression,
            )?;

            if previous_branch_type.is_some()
                && !type_parameters_bindings
                    .are_compatible(&previous_branch_type.unwrap(), &branch_type)
            {
                all_compatible = false;
            }

            let branch_type = type_parameters_bindings.concretize(&branch_type);
            branch_types.push(Rc::clone(&branch_type));
            previous_branch_type = Some(Rc::clone(&branch_type));
        }

        match (branch_types.len(), all_compatible) {
            (1, false) => Err(format!(
                "Cannot infer type of function {}, possibly infinite recursion?",
                program.var_name(function_id),
            )),
            (1, true) => Ok(Rc::new(Type::create_function_type(
                &mut program.identifier_id_map,
                function_type_args,
                Rc::clone(&type_parameters_bindings.concretize(&branch_types[0])),
                Some(arg_types),
            ))),
            (_, true) => Ok(Rc::new(Type::create_function_type(
                &mut program.identifier_id_map,
                function_type_args,
                Rc::clone(
                    &type_parameters_bindings.concretize(
                        &branch_types
                            .into_iter()
                            .filter(|t| !t.is_unknown())
                            .next()
                            .unwrap(),
                    ),
                ),
                Some(arg_types),
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
