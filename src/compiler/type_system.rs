use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use nohash_hasher::IntMap;

use super::{
    identifier_map::{
        IdentifierId, BOOL_ID, FALSE_ID, FLOAT_ID, INT_ID, STRING_ID, TRUE_ID, UNDECIDED_ID,
        VOID_ID, WILDCARD_ID,
    },
    internal_repr::{
        DestructuringComponent, Expression, FunctionDefinition, Program, RcType, Type,
        TypeDefinition, TypeVariant,
    },
    utils::to_int_map,
};

#[derive(Debug, Clone)]
pub(super) struct TypeInfo {
    pub(super) primitive_types: HashSet<IdentifierId>,
    pub(super) user_types: IntMap<IdentifierId, RcType>,
    //todo: function_types and constant_types could be part of the same map
    pub(super) function_types: IntMap<IdentifierId, RcType>,
    pub(super) constant_types: Rc<IntMap<IdentifierId, RcType>>,
}

impl TypeInfo {
    pub fn new() -> Self {
        let primitive_types = HashSet::from([INT_ID, STRING_ID, BOOL_ID, VOID_ID, FLOAT_ID]);
        let bool_type = Rc::new(Type::PrimitiveType(BOOL_ID));
        let void_type = Rc::new(Type::PrimitiveType(VOID_ID));
        let undecided_type = Rc::new(Type::Undecided);
        Self {
            primitive_types,
            user_types: IntMap::default(),
            function_types: IntMap::default(),
            constant_types: Rc::new(to_int_map(HashMap::from([
                (TRUE_ID, Rc::clone(&bool_type)),
                (FALSE_ID, Rc::clone(&bool_type)),
                (VOID_ID, Rc::clone(&void_type)),
                (UNDECIDED_ID, Rc::clone(&undecided_type)),
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
    bindings: IntMap<IdentifierId, RcType>,
}

impl TypeParameterBindings {
    fn new() -> Self {
        Self {
            bindings: IntMap::default(),
        }
    }

    fn invert_substitutions(
        external_type_vars: &HashSet<IdentifierId>,
        substitutions: &IntMap<IdentifierId, RcType>,
    ) -> IntMap<IdentifierId, RcType> {
        let mut inverted_type_vars = IntMap::default();
        for id in external_type_vars {
            let t = Rc::new(Type::UnboundTypeVariable(*id));
            let concrete_t = TypeParameterBindings::find_most_concrete(&t, substitutions);
            match concrete_t.as_ref() {
                Type::UnboundTypeVariable(id) => {
                    if !external_type_vars.contains(id) {
                        inverted_type_vars.insert(*id, t);
                    }
                }
                _ => {}
            }
        }
        inverted_type_vars
    }

    fn concretize(_type: &RcType, bindings: &IntMap<IdentifierId, RcType>) -> RcType {
        match _type.as_ref() {
            Type::FunctionType(id, arg_types, return_type, captures) => {
                Rc::new(Type::FunctionType(
                    *id,
                    arg_types
                        .iter()
                        .map(|t| TypeParameterBindings::concretize(t, bindings))
                        .collect(),
                    TypeParameterBindings::concretize(return_type, bindings),
                    captures.clone(),
                ))
            }
            Type::CompositeType(id, items) => Rc::new(Type::CompositeType(
                *id,
                items
                    .iter()
                    .map(|t| TypeParameterBindings::concretize(t, bindings))
                    .collect(),
            )),
            _ => TypeParameterBindings::find_most_concrete(_type, bindings),
        }
    }

    fn find_most_concrete(type_t: &RcType, bindings: &IntMap<IdentifierId, RcType>) -> RcType {
        let mut substitutions = vec![type_t];
        let mut stop = false;
        let mut visited = HashSet::from([type_t.id()]);
        while !stop {
            let n = substitutions.len();
            let sub = bindings.get(&substitutions[n - 1].id());
            if sub.is_none() {
                stop = true;
            } else {
                let t = sub.unwrap();
                match t.as_ref() {
                    Type::PrimitiveType(_) => {
                        return Rc::clone(t);
                    }
                    Type::CompositeType(_, _) | Type::FunctionType(_, _, _, _) => {
                        return TypeParameterBindings::concretize(t, bindings);
                    }
                    _ => {
                        substitutions.push(t);
                        if visited.contains(&t.id()) {
                            stop = true;
                            println!("cycle break {:?}", substitutions);
                        }
                        visited.insert(t.id());
                    }
                }
            }
        }
        Rc::clone(substitutions[substitutions.len() - 1])
    }

    fn is_subtype(&mut self, concrete_t: &RcType, abstract_t: &RcType) -> bool {
        match (concrete_t.as_ref(), abstract_t.as_ref()) {
            (Type::Undecided, Type::Undecided) => false,
            (Type::Undecided, _) => true,
            (_, Type::Undecided) => true,
            (Type::UnboundTypeVariable(concrete_id), Type::UnboundTypeVariable(abstract_id)) => {
                if *concrete_id == *abstract_id {
                    return true;
                }

                let abstract_binding =
                    TypeParameterBindings::find_most_concrete(&abstract_t, &self.bindings);
                let concrete_binding =
                    TypeParameterBindings::find_most_concrete(&concrete_t, &self.bindings);
                if &abstract_binding == abstract_t && &concrete_binding == concrete_t {
                    self.bindings.insert(*abstract_id, Rc::clone(concrete_t));
                    return true;
                }

                if &abstract_binding != abstract_t && &concrete_binding != concrete_t {
                    return abstract_binding.id() == concrete_binding.id();
                }

                if &abstract_binding == abstract_t {
                    // prevents cycles
                    if concrete_binding != abstract_binding {
                        self.bindings.insert(abstract_t.id(), Rc::clone(concrete_t));
                    }
                    return true;
                }

                if &concrete_binding == concrete_t {
                    // prevents cycles
                    if concrete_binding != abstract_binding {
                        self.bindings
                            .insert(concrete_t.id(), Rc::clone(&abstract_binding));
                    }
                    return true;
                }

                false
            }
            (Type::UnboundTypeVariable(id), _) => {
                if abstract_t.type_parameters().contains(id) {
                    return false;
                }

                let binding =
                    TypeParameterBindings::find_most_concrete(&concrete_t, &self.bindings);
                if &binding != concrete_t {
                    return binding.id() == abstract_t.id();
                }
                //todo: no? it's used as surrogate for unification, fix up in pattern matching comparison
                self.bindings.insert(concrete_t.id(), Rc::clone(abstract_t));
                true
            }
            (_, Type::UnboundTypeVariable(id)) => {
                if concrete_t.type_parameters().contains(id) {
                    return false;
                }

                let binding =
                    TypeParameterBindings::find_most_concrete(&abstract_t, &self.bindings);
                if &binding != abstract_t {
                    return self.is_subtype(concrete_t, &binding);
                }
                self.bindings.insert(abstract_t.id(), Rc::clone(concrete_t));
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
                    if !self.is_subtype(&first_inputs[i], &second_inputs[i]) {
                        return false;
                    }
                }

                self.is_subtype(first_output, second_output)
            }
            (
                Type::CompositeType(id_first, items_first),
                Type::CompositeType(id_second, items_second),
            ) => {
                if *id_first != *id_second || items_first.len() != items_second.len() {
                    return false;
                }

                for i in 0..items_first.len() {
                    if !self.is_subtype(&items_first[i], &items_second[i]) {
                        return false;
                    }
                }
                true
            }
            _ => concrete_t.id() == abstract_t.id(),
        }
    }
}

fn get_identifier_type(
    program: &mut Program,
    type_info: &TypeInfo,
    identifier_types: &Rc<IntMap<IdentifierId, RcType>>,
    caller_id: &IdentifierId,
    id: &IdentifierId,
) -> Result<RcType, String> {
    let function_type = identifier_types.get(id);
    if function_type.is_some() {
        return Ok(Rc::clone(function_type.unwrap()));
    }

    let function_definition = program.functions.get(id);
    if function_definition.is_some() {
        let function_definition = Rc::clone(&function_definition.unwrap());
        let function_type = infer_function_definition_type(
            program,
            type_info,
            &function_definition,
            Some(Rc::clone(identifier_types)),
        )?;
        return Ok(function_type);
    }

    let builtin = type_info.function_types.get(id);
    if builtin.is_some() {
        return Ok(Rc::clone(builtin.unwrap()));
    }

    Err(format!(
        "Undefined identifier '{}' in function '{}', known '{:?}'",
        program.var_name(id),
        program.var_name(caller_id),
        identifier_types
            .iter()
            .map(|(k, v)| (program.var_name(k), v.full_repr(&program.identifier_id_map)))
            .collect::<Vec<(&Rc<String>, Rc<String>)>>()
    ))
}

fn concretize_function_type(
    program: &mut Program,
    identifier_types: &Rc<IntMap<IdentifierId, RcType>>,
    caller_id: &IdentifierId,
    function_id: &IdentifierId,
    external_type_vars: &HashSet<IdentifierId>,
    function_type: &RcType,
    parameters: &Vec<RcType>,
) -> Result<RcType, String> {
    let mut type_parameters_bindings = TypeParameterBindings::new();
    match function_type.as_ref() {
        Type::FunctionType(_, arg_types, _, _) => {
            if arg_types.len() != parameters.len() {
                return Err(format!(
                    "Function '{}' in caller '{}' expects {} arguments but {} were provided",
                    program.var_name(function_id),
                    program.var_name(caller_id),
                    arg_types.len(),
                    parameters.len()
                ));
            }

            for i in 0..parameters.len() {
                let arg_type = &arg_types[i];
                if !type_parameters_bindings.is_subtype(&parameters[i], &arg_type) {
                    return Err(format!(
                        "Argument {} in function '{}' in caller '{}' expects type {} (bound to {}) but {} (bound to {}) was provided",
                        i+1,
                        program.var_name(function_id),
                        program.var_name(caller_id),
                        TypeParameterBindings::concretize(arg_type, &type_parameters_bindings.bindings)
                            .full_repr(&program.identifier_id_map),
                        arg_type.full_repr(&program.identifier_id_map),
                        TypeParameterBindings::concretize(&parameters[i], &type_parameters_bindings.bindings)
                            .full_repr(&program.identifier_id_map),
                        parameters[i].full_repr(&program.identifier_id_map),
                    ));
                }
            }
        }
        _ => panic!("Bug! Bug!"),
    }

    let inverted_type_vars = TypeParameterBindings::invert_substitutions(
        external_type_vars,
        &type_parameters_bindings.bindings,
    );

    let result = TypeParameterBindings::concretize(
        &function_type.as_return_type(),
        &type_parameters_bindings.bindings,
    );

    Ok(TypeParameterBindings::concretize(
        &result,
        &inverted_type_vars,
    ))
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
                    if t.id() != type_id
                        && !type_info.type_exists(&t.id())
                        && !type_params.contains(&t.id())
                    {
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

//value or variable -> return type of value or variable
//function call -> find abstract type of function, substitute arguments one by one until it either fails or it's fully
//composite type constructor -> treat constructor as function
pub fn infer_expression_type(
    expression: &Expression,
    program: &mut Program,
    type_info: &TypeInfo,
    identifier_types: &Rc<IntMap<IdentifierId, RcType>>,
    external_type_vars: &HashSet<IdentifierId>,
    current_function_id: &IdentifierId,
) -> Result<RcType, String> {
    match expression {
        Expression::Identifier(id) => get_identifier_type(
            program,
            type_info,
            identifier_types,
            current_function_id,
            id,
        ),
        Expression::Integer(_) => Ok(Rc::new(Type::PrimitiveType(INT_ID))),
        Expression::String(_) => Ok(Rc::new(Type::PrimitiveType(STRING_ID))),
        Expression::Float(_) => Ok(Rc::new(Type::PrimitiveType(FLOAT_ID))),
        Expression::FunctionCall(function_call) => {
            // Recursive call type is left undecided
            if *current_function_id == function_call.id {
                return Ok(Rc::new(Type::Undecided));
            }

            let function_type = get_identifier_type(
                program,
                type_info,
                identifier_types,
                current_function_id,
                &function_call.id,
            );
            if function_type.is_ok() {
                let function_type = function_type.unwrap();
                match function_type.as_ref() {
                    Type::FunctionType(_, arguments, return_type, captures) => {
                        let provided_types = &function_call.arguments;
                        let parameter_len = arguments.len();
                        if provided_types.len() != parameter_len {
                            return Err(format!(
                                "Function '{}' in caller '{}' expects {} arguments but {} were provided",
                                program.var_name(&function_call.id),
                                program.var_name(current_function_id),
                                arguments.len(),
                                provided_types.len()
                            ));
                        }

                        let mut arg_types = Vec::new();
                        for expr in &function_call.arguments {
                            let arg_t = infer_expression_type(
                                expr,
                                program,
                                type_info,
                                identifier_types,
                                &function_type.type_parameters(),
                                current_function_id,
                            )?;
                            arg_types.push(arg_t);
                        }

                        return concretize_function_type(
                            program,
                            if captures.is_none() {
                                identifier_types
                            } else {
                                captures.as_ref().unwrap()
                            },
                            current_function_id,
                            &function_call.id,
                            external_type_vars,
                            &function_type,
                            &arg_types,
                        );
                    }
                    _ => {
                        return Err(format!(
                            "'{}' with type '{} is not callable",
                            program.var_name(&function_call.id),
                            program.var_name(&function_type.id())
                        ));
                    }
                }
            }

            let function_type = type_info.function_types.get(&function_call.id);
            if function_type.is_some() {
                let function_type = function_type.unwrap();

                let mut arg_types = Vec::new();
                for expr in &function_call.arguments {
                    let arg_t = infer_expression_type(
                        expr,
                        program,
                        type_info,
                        identifier_types,
                        &function_type.type_parameters(),
                        current_function_id,
                    )?;
                    arg_types.push(arg_t);
                }

                //todo: unnecessary if?
                let function_definition = program.functions.get(&function_call.id);
                if function_definition.is_some() {
                    return concretize_function_type(
                        program,
                        identifier_types,
                        current_function_id,
                        &function_call.id,
                        external_type_vars,
                        function_type,
                        &arg_types,
                    );
                } else {
                    match function_type.as_ref() {
                        Type::FunctionType(_, _, _, captures) => {
                            return concretize_function_type(
                                program,
                                if captures.is_none() {
                                    identifier_types
                                } else {
                                    captures.as_ref().unwrap()
                                },
                                current_function_id,
                                &function_call.id,
                                external_type_vars,
                                function_type,
                                &arg_types,
                            );
                        }
                        _ => panic!("BUG"),
                    }
                }
            }

            let dispatch = program.dispatches.get(&function_call.id);
            if dispatch.is_some() {
                // Clone needed due to immutable borrow of program
                let dispatch = Rc::clone(dispatch.unwrap());

                if function_call.arguments.len() != dispatch.arguments.len() {
                    return Err(format!(
                        "Dispatch {} expects {} arguments but {} were provided",
                        program.var_name(&function_call.id),
                        dispatch.arguments.len(),
                        function_call.arguments.len(),
                    ));
                }

                let mut arg_types = Vec::new();
                for arg in &function_call.arguments {
                    let arg_type = infer_expression_type(
                        arg,
                        program,
                        type_info,
                        identifier_types,
                        external_type_vars,
                        current_function_id,
                    )?;
                    arg_types.push(arg_type);
                }

                for (pattern, expression) in &dispatch.bodies {
                    if pattern.components.len() != dispatch.arguments.len() {
                        return Err(format!(
                            "Dispatch {} has {} arguments but {} found in pattern",
                            program.var_name(&function_call.id),
                            dispatch.arguments.len(),
                            pattern.components.len(),
                        ));
                    }

                    let mut matching_args = 0;
                    for i in 0..pattern.components.len() {
                        match &pattern.components[i] {
                            DestructuringComponent::Identifier(type_id) => {
                                if *type_id != WILDCARD_ID && arg_types[i].id() != *type_id {
                                    continue;
                                }
                                matching_args += 1
                            }
                            _ => {
                                return Err(format!(
                                    "Expected only type names in dispatch {}",
                                    program.var_name(&function_call.id),
                                ));
                            }
                        }
                    }

                    if matching_args == dispatch.arguments.len() {
                        let mut identifier_types = identifier_types.as_ref().clone();
                        for i in 0..dispatch.arguments.len() {
                            let arg_type = Rc::clone(&arg_types[i]);
                            identifier_types.insert(dispatch.arguments[i], arg_type);
                        }
                        return infer_expression_type(
                            expression,
                            program,
                            type_info,
                            &Rc::new(identifier_types),
                            external_type_vars,
                            current_function_id,
                        );
                    }
                }

                return Err(format!(
                    "Dispatch '{}' is not defined for arguments '{}'",
                    program.var_name(&function_call.id),
                    arg_types
                        .iter()
                        .map(|t| program
                            .identifier_id_map
                            .get_identifier(&t.id())
                            .unwrap()
                            .as_ref()
                            .to_owned())
                        .collect::<Vec<String>>()
                        .join(", ")
                ));
            }

            Err(format!(
                "Undefined identifier '{}' in function '{}', known '{:?}'",
                program.var_name(&function_call.id),
                program.var_name(current_function_id),
                identifier_types
                    .iter()
                    .map(|(k, v)| (program.var_name(k), v.full_repr(&program.identifier_id_map)))
                    .collect::<Vec<(&Rc<String>, Rc<String>)>>()
            ))
        }
        Expression::TypeConstructor(type_id, variant_id, expressions) => {
            let (type_variant, type_vars) = {
                let result = program.types.get(type_id);
                if result.is_none() {
                    return Err(format!(
                        "Undefined type '{}' in function '{}'",
                        program.var_name(type_id),
                        program.var_name(current_function_id),
                    ));
                }

                let definition = result.unwrap();

                let type_variant = definition.variants.get(variant_id);
                if type_variant.is_none() {
                    return Err(format!(
                        "Undefined variant '{}' of type '{}' in function '{}'",
                        program.var_name(variant_id),
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
                    program.var_name(variant_id),
                    program.var_name(type_id),
                    expressions.len()));
                }

                //necessary to avoid mut borrowing of program while another borrowing is active
                //the borrowing (variant as internal state of program) is dropped after the end of this block
                (Rc::clone(type_variant), definition.def.type_parameters())
            };

            if !expressions.is_empty() {
                match type_variant.as_ref() {
                    TypeVariant::Cartesian(variant, items) => {
                        let mut type_parameter_bindings = TypeParameterBindings::new();
                        for i in 0..expressions.len() {
                            let expression_type = infer_expression_type(
                                &expressions[i],
                                program,
                                type_info,
                                identifier_types,
                                &external_type_vars.union(&type_vars).cloned().collect(),
                                current_function_id,
                            )?;

                            if !type_parameter_bindings.is_subtype(&expression_type, &items[i]) {
                                return Err(format!(
                                    "Expecting {} in constructor for {}::{variant} but got {}",
                                    TypeParameterBindings::find_most_concrete(
                                        &items[i],
                                        &type_parameter_bindings.bindings
                                    )
                                    .full_repr(&program.identifier_id_map),
                                    program.var_name(type_id),
                                    TypeParameterBindings::find_most_concrete(
                                        &expression_type,
                                        &type_parameter_bindings.bindings
                                    )
                                    .full_repr(&program.identifier_id_map),
                                ));
                            }
                        }

                        return Ok(Rc::clone(&TypeParameterBindings::concretize(
                            &program.types.get(type_id).unwrap().def,
                            &type_parameter_bindings.bindings,
                        )));
                    }
                    _ => panic!("BUG"),
                };
            }

            Ok(Rc::clone(&program.types.get(type_id).unwrap().def))
        }
        Expression::WithBlock(items, expression) => {
            let mut inner_types = identifier_types.as_ref().clone();
            for (id, expr) in items {
                let result = infer_expression_type(
                    expr,
                    program,
                    type_info,
                    &Rc::new(inner_types.clone()),
                    external_type_vars,
                    current_function_id,
                )?;
                inner_types.insert(*id, Rc::clone(&result));
            }
            infer_expression_type(
                expression.as_ref(),
                program,
                type_info,
                &Rc::new(inner_types),
                external_type_vars,
                current_function_id,
            )
        }
        Expression::Cast(expression, type_cast) => {
            let mut bindings = TypeParameterBindings::new();
            let expr_type = infer_expression_type(
                expression,
                program,
                type_info,
                identifier_types,
                external_type_vars,
                current_function_id,
            )?;
            if !bindings.is_subtype(type_cast, &expr_type) {
                return Err(format!(
                    "Cannot cast {} as {}",
                    expr_type.full_repr(&program.identifier_id_map),
                    type_cast.full_repr(&program.identifier_id_map)
                ));
            }
            Ok(TypeParameterBindings::concretize(
                type_cast,
                &bindings.bindings,
            ))
        }
        Expression::If(condition, when_true, when_false) => {
            let result = infer_expression_type(
                condition,
                program,
                type_info,
                identifier_types,
                external_type_vars,
                current_function_id,
            )?;

            match result.as_ref() {
                Type::PrimitiveType(x) if *x == BOOL_ID => {}
                _ => {
                    return Err(format!(
                        "If condition must be boolean but got '{}'",
                        program.var_name(&result.id())
                    ));
                }
            }

            let true_type = infer_expression_type(
                &when_true,
                program,
                type_info,
                identifier_types,
                external_type_vars,
                current_function_id,
            )?;
            let false_type = infer_expression_type(
                &when_false,
                program,
                type_info,
                identifier_types,
                external_type_vars,
                current_function_id,
            )?;

            let is_true_unknown = true_type.is_undecided();
            let is_false_unknown = false_type.is_undecided();
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
        Expression::LambdaExpression(function_definition) => infer_function_definition_type(
            program,
            type_info,
            &function_definition,
            Some(Rc::clone(identifier_types)),
        ),
    }
}

pub fn infer_function_definition_type(
    program: &mut Program,
    type_info: &TypeInfo,
    current_function: &FunctionDefinition,
    captures: Option<Rc<IntMap<IdentifierId, RcType>>>,
) -> Result<RcType, String> {
    let mut arg_types = type_info.constant_types.as_ref().clone();
    if captures.is_some() {
        for (k, v) in captures.as_ref().unwrap().as_ref() {
            arg_types.insert(*k, Rc::clone(v));
        }
    }

    for argument in &current_function.arguments {
        match &argument.typing.as_ref() {
            Type::UnboundTypeVariable(_) => {}
            Type::PrimitiveType(_) | Type::CompositeType(_, _) => {
                let id = argument.typing.id();
                if !type_info.type_exists(&id) {
                    return Err(format!("Type '{}' doesn't exist", program.var_name(&id)));
                }
            }
            Type::Undecided => panic!(
                "Undecided type is invalid for argument '{}' of function '{}'",
                argument.identifier, current_function.id
            ),
            Type::FunctionType(_, args, return_type, _) => {
                //todo: should use capture types?
                for arg_type in args {
                    if !arg_type.is_type_parameter() && !type_info.type_exists(&arg_type.id()) {
                        return Err(format!(
                            "Type '{}' doesn't exist",
                            program.var_name(&arg_type.id())
                        ));
                    }
                }

                let return_id = return_type.id();
                if !return_type.is_type_parameter() && !type_info.type_exists(&return_id) {
                    return Err(format!(
                        "Type '{}' doesn't exist",
                        program.var_name(&return_id)
                    ));
                }
            }
        }

        arg_types.insert(argument.identifier, Rc::clone(&argument.typing));
    }
    let arg_types = Rc::new(arg_types);

    let function_id = &current_function.id;
    let function_type_args: Vec<Rc<Type>> = current_function
        .arguments
        .iter()
        .map(|arg| Rc::clone(&arg.typing))
        .collect();
    let mut external_type_vars = HashSet::new();
    for arg_t in &function_type_args {
        for t in &arg_t.type_parameters() {
            external_type_vars.insert(*t);
        }
    }
    if current_function.is_not_pattern_matched() {
        let return_type = infer_expression_type(
            &current_function.bodies[0].1,
            program,
            type_info,
            &arg_types,
            &external_type_vars,
            &current_function.id,
        )?;

        if return_type.is_undecided() {
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

            if destructuring.components.len() != current_function.arguments.len() {
                return Err(format!(
                    "Expected {} destructuring components but got {} in function '{}'",
                    current_function.arguments.len(),
                    destructuring.components.len(),
                    program.var_name(&function_id),
                ));
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
                    DestructuringComponent::Destructuring(destructuring) => {
                        let result = program.types.get(&arg.typing.id());
                        if result.is_none() {
                            return Err(format!(
                                "Cannot find type '{}' for argument '{}' of function '{}' when destructuring '{}'",
                                program.var_name(&arg.typing.id()),
                                program.var_name(&arg.identifier),
                                program.var_name(&function_id),
                                program.var_name(&destructuring.0),
                            ));
                        }
                        let type_def = Rc::clone(&result.unwrap().def);
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
                                    return Err(format!("Variant '{name}' for type '{}' expects 0 components but got {} in function '{}'",
                                            program.var_name(&arg.typing.id()),
                                            destructuring.1.len(),
                                            program.var_name(&function_id)
                                        ));
                                }
                            }
                            TypeVariant::Cartesian(name, component_types) => {
                                if component_types.len() != destructuring.1.len() {
                                    return Err(format!("Variant '{name}' for type '{}' expects {} components but got {} in function '{}'",
                                            program.var_name(&arg.typing.id()),
                                            component_types.len(),
                                            destructuring.1.len(),
                                            program.var_name(&function_id)
                                        ));
                                }

                                let mut bindings = TypeParameterBindings::new();
                                //binds argument to abstract type definition
                                if !bindings.is_subtype(&arg.typing, &type_def) {
                                    return Err(format!(
                                        "Typing error: {} is not subtype of {}",
                                        arg.typing.full_repr(&program.identifier_id_map),
                                        type_def.full_repr(&program.identifier_id_map),
                                    ));
                                }

                                let mut k = 0;
                                for inner_component in &destructuring.1 {
                                    match inner_component {
                                        DestructuringComponent::Identifier(identifier)
                                            if *identifier != WILDCARD_ID =>
                                        {
                                            let resolved = TypeParameterBindings::concretize(
                                                &component_types[k],
                                                &bindings.bindings,
                                            );
                                            identifier_types.insert(*identifier, resolved);
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
                &expression,
                program,
                type_info,
                &Rc::new(identifier_types),
                &external_type_vars,
                &current_function.id,
            )?;

            if previous_branch_type.is_some()
                && !type_parameters_bindings
                    .is_subtype(&previous_branch_type.unwrap(), &branch_type)
            {
                all_compatible = false;
            }

            let branch_type =
                TypeParameterBindings::concretize(&branch_type, &type_parameters_bindings.bindings);

            let inverted_type_vars = TypeParameterBindings::invert_substitutions(
                &external_type_vars,
                &type_parameters_bindings.bindings,
            );

            let branch_type = TypeParameterBindings::concretize(&branch_type, &inverted_type_vars);

            branch_types.push(Rc::clone(&branch_type));
            previous_branch_type = Some(Rc::clone(&branch_type));
        }

        match (branch_types.len(), all_compatible) {
            (1, false) => Err(format!(
                "Cannot infer type of function {}, possibly infinite recursion?",
                program.var_name(function_id),
            )),
            (_, true) => {
                let inverted_type_vars = TypeParameterBindings::invert_substitutions(
                    &external_type_vars,
                    &type_parameters_bindings.bindings,
                );
                let result = Rc::new(Type::create_function_type(
                    &mut program.identifier_id_map,
                    function_type_args,
                    Rc::clone(&TypeParameterBindings::concretize(
                        &branch_types
                            .into_iter()
                            .filter(|t| !t.is_undecided())
                            .next()
                            .unwrap(),
                        &type_parameters_bindings.bindings,
                    )),
                    Some(arg_types),
                ));

                let result = TypeParameterBindings::concretize(&result, &inverted_type_vars);

                Ok(result)
            }
            //todo: for N types, as long as none is Unknown, we need to check what's the supertype of all of them (common ancestor in hierarchy tree)
            _ => Err(format!(
                "Function '{}' has matched patterns with different types: {:?}",
                program.var_name(function_id),
                branch_types
            )),
        }
    }
}
