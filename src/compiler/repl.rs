use std::collections::HashMap;
use std::fmt::Display;
use std::mem;
use std::rc::Rc;
use std::time::Instant;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::parser::combinators::ParserCombinator;
use crate::{compiler::grammar, parser::token_stream::TokenStream};

use super::identifier_map::{
    IdentifierId, ADD_ID, DIV_ID, EMPTY_LIST_ID, EQ_ID, ERROR_ID, FALSE_ID, GE_ID, GT_ID, LE_ID,
    LIST_ID, LT_ID, MUL_ID, PRINTLN_ID, PRINT_ID, RANGE_ID, REPL_ID, SUB_ID, TRUE_ID, WHILE_ID,
    WILDCARD_ID,
};
use super::internal_repr::{
    expression_repr, DestructuringComponent, Expression, FunctionCall, FunctionDefinition, Pattern,
    Program,
};
use super::type_system::{infer_expression_type, TypeInfo};

#[derive(Clone, Debug)]
enum Value {
    Typed(IdentifierId, IdentifierId, Vec<Rc<Value>>),
    Integer(i64),
    Float(f32),
    Bool(bool),
    String(Rc<String>),
    Function(Rc<FunctionDefinition>, Rc<HashMap<IdentifierId, Rc<Value>>>),
    Void,
}

// Like for linked lists, we need to avoid recursive drop().
// Here it's enough for now to only handle Typed variant as Function chains of million elements are highly unlikely
// see https://rust-unofficial.github.io/too-many-lists/first-drop.html
impl Drop for Value {
    fn drop(&mut self) {
        let mut stack = Vec::new();
        let mut current: *mut Value = self;

        while let Some(node) = unsafe { current.as_mut() } {
            if let Value::Typed(_, _, values) = node {
                for rc in mem::take(values) {
                    // Only executes when there reference count is 0
                    if let Ok(inner) = Rc::try_unwrap(rc) {
                        stack.push(Box::new(inner));
                    }
                }
            }

            current = match stack.pop() {
                Some(boxed) => Box::leak(boxed),
                None => std::ptr::null_mut(),
            };
        }
    }
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
            Value::Function(_, _) => write!(f, "Function"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{}", &s.to_string()[1..s.len() - 1]),
            Value::Void => write!(f, "Void"),
        }
    }
}

impl Value {
    fn value_to_str(
        &self,
        id_to_name: &impl Fn(&IdentifierId) -> Rc<String>,
    ) -> Result<String, String> {
        match self {
            Value::Typed(id, variant, values) => {
                let mut args = Vec::new();
                for value in values {
                    let result = value.value_to_str(id_to_name);
                    if result.is_err() {
                        return result;
                    }
                    args.push(result.unwrap());
                }
                Ok(format!(
                    "{}::{}({})",
                    id_to_name(id),
                    id_to_name(variant),
                    args.join(", ")
                ))
            }
            Value::Function(def, _) => Ok(format!("Function {}", id_to_name(&def.id))),
            Value::Integer(x) => Ok(format!("{x}")),
            Value::Float(x) => Ok(format!("{x}")),
            Value::Bool(x) => Ok(format!("{x}")),
            Value::String(x) => Ok(format!("{}", &x.to_string()[1..x.len() - 1])),
            Value::Void => Ok("Void".to_string()),
        }
    }

    fn as_boolean(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Not a boolean"),
        }
    }

    fn as_int(&self) -> i64 {
        match self {
            Value::Integer(x) => *x,
            _ => panic!("Not an Int"),
        }
    }
}

#[derive(Clone, Debug)]
struct PatternMatchResult {
    is_match: bool,
    bindings: HashMap<IdentifierId, Rc<Value>>,
}

impl PatternMatchResult {
    fn no_match() -> Self {
        Self {
            is_match: false,
            bindings: HashMap::new(),
        }
    }

    fn with_match(bindings: HashMap<IdentifierId, Rc<Value>>) -> Self {
        Self {
            is_match: true,
            bindings,
        }
    }
}

pub struct REPL {
    program: Program,
    type_info: TypeInfo,
}

impl REPL {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            type_info: TypeInfo::new(),
        }
    }

    pub fn handle_input(&mut self, input: &str) {
        let tokens: Vec<&str> = input.split_whitespace().collect();
        let tokens = TokenStream::from_str(tokens);

        //todo: parse should return Result<ParserResult>
        let mut result = grammar::program_parser().parse(&tokens);
        if !result.result || result.remaining.not_done() {
            result = grammar::expression_parser().parse(&tokens);
        }

        if !result.result || result.remaining.not_done() {
            println!("ERROR: Failed to parse! {:?}", result.remaining);
            return;
        }

        let ast = result.ast;
        let result = to_repr(&ast, &mut self.program.identifier_id_map);
        if result.is_ok() {
            let Program {
                functions,
                types,
                identifier_id_map: _,
            } = result.unwrap();
            for (id, definition) in types {
                println!("Defined type {}", self.var_name(&id));

                self.type_info.add_user_type(id, &definition);
                self.program.types.insert(id, definition);
            }
            for key in functions.keys() {
                let (id, definition) = (key, functions.get(key.as_ref()).unwrap());
                let result =
                    infer_function_type(&mut self.program, &self.type_info, &definition, None);
                if result.is_err() {
                    println!("ERROR: {}", result.unwrap_err());
                } else {
                    let function_type = result.unwrap();
                    println!(
                        "Defined function {}: {}",
                        self.program.var_name(id),
                        self.program.var_name(&function_type.id()),
                    );

                    self.program.functions.insert(**id, Rc::clone(definition));
                    self.type_info.function_types.insert(**id, function_type);
                }
            }
        } else {
            let original_error = result.unwrap_err();
            let result = expression_repr(&ast, &mut self.program.identifier_id_map);
            if result.is_ok() {
                let values = HashMap::from([
                    (TRUE_ID, Rc::new(Value::Bool(true))),
                    (FALSE_ID, Rc::new(Value::Bool(false))),
                ]);
                let start = Instant::now();
                let expression = result.unwrap();
                let expr_type = infer_expression_type(
                    &mut self.program,
                    &self.type_info,
                    &Rc::clone(&self.type_info.constant_types),
                    &REPL_ID,
                    &expression,
                );
                let result = self.evaluate_expression(&Rc::new(values), &expression);

                if result.is_ok() && expr_type.is_ok() {
                    println!(
                        "\nType: {}",
                        &expr_type
                            .unwrap()
                            .full_repr(&self.program.identifier_id_map)
                    );
                    println!("Evaluated in {}us", start.elapsed().as_micros());
                } else if result.is_err() {
                    println!("ERROR: {}", result.unwrap_err());
                } else {
                    println!("ERROR: {}", expr_type.unwrap_err());
                }
            } else {
                println!("ERROR(s): {} {}", original_error, result.unwrap_err());
            }
        }
    }

    fn pattern_match(
        &self,
        function_id: &IdentifierId,
        pattern: &Pattern,
        args: &Vec<Rc<Value>>,
    ) -> Result<PatternMatchResult, String> {
        if pattern.components.len() != args.len() {
            return Err(format!(
                "Found pattern with {} elements but function '{function_id}' has {} arguments",
                pattern.components.len(),
                args.len()
            ));
        }

        let mut bindings = HashMap::new();
        for i in 0..pattern.components.len() {
            let element = &pattern.components[i];
            let arg = &args[i];
            match element {
                DestructuringComponent::Identifier(identifier) => {
                    match arg.as_ref() {
                        Value::Typed(id, variant, values) => {
                            let identifier = *identifier;
                            if identifier != WILDCARD_ID {
                                if identifier != *variant {
                                    return Ok(PatternMatchResult::no_match());
                                }

                                if !values.is_empty() {
                                    return Err(format!("Cannot destructure variant '{}' of type '{}' with zero elements, because constructor requires {} arguments",
                                self.program.var_name(variant),
                                self.program.var_name(id),
                                values.len()));
                                }
                            }
                        }
                        Value::Integer(_)
                        | Value::Float(_)
                        | Value::Function(_, _)
                        | Value::String(_) => {
                            //todo: multiple "_" should also be considered wildcard
                            if *identifier != WILDCARD_ID {
                                return Err(format!(
                                    "Redundant re-binding '{}' of argument {i} in function '{}'",
                                    self.program.var_name(identifier),
                                    self.program.var_name(function_id)
                                ));
                            }
                        }
                        Value::Bool(bool_val) => {
                            if (*identifier == TRUE_ID && !bool_val)
                                || (*identifier == FALSE_ID && *bool_val)
                            {
                                return Ok(PatternMatchResult::no_match());
                            }
                        }
                        Value::Void => return Err(format!("Arg is Void")),
                    }
                }
                DestructuringComponent::Destructuring(destructuring) => match arg.as_ref() {
                    Value::Typed(id, variant, values) => {
                        if *variant != destructuring.0 {
                            return Ok(PatternMatchResult::no_match());
                        }

                        if values.len() != destructuring.1.len() {
                            return Err(format!("Cannot destructure variant '{}' of type '{}' in {} elements, because constructor requires {} arguments", 
                            self.program.var_name(variant),
                            self.program.var_name(id),
                            values.len(),
                            destructuring.1.len()));
                        }

                        for i in 0..values.len() {
                            let value = &values[i];
                            let pattern = &destructuring.1[i];

                            match (pattern, value.as_ref()) {
                                (DestructuringComponent::Identifier(i), _) if *i == WILDCARD_ID => {
                                }
                                (DestructuringComponent::Identifier(i), Value::Typed(_, _, _)) => {
                                    bindings.insert(*i, Rc::clone(value));
                                }
                                (DestructuringComponent::Integer(x), Value::Integer(y)) => {
                                    if x != y {
                                        return Ok(PatternMatchResult::no_match());
                                    }
                                }
                                (DestructuringComponent::Identifier(i), Value::Integer(_)) => {
                                    bindings.insert(*i, Rc::clone(value));
                                }
                                (DestructuringComponent::Identifier(x), Value::Bool(y)) => {
                                    let x = *x;
                                    if (x == TRUE_ID && !y) || (x == FALSE_ID && *y) {
                                        return Ok(PatternMatchResult::no_match());
                                    }
                                    bindings.insert(x, Rc::clone(value));
                                }
                                _ => {
                                    return Err(format!(
                                        "Unsupported destructuring in function '{}'",
                                        self.program.var_name(function_id)
                                    ))
                                }
                            }
                        }
                    }
                    Value::Integer(_) => {
                        return Err(format!(
                            "Cannot destructure Int in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                    Value::String(_) => {
                        return Err(format!(
                            "Cannot destructure String in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                    Value::Float(_) => {
                        return Err(format!(
                            "Cannot destructure Float in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                    Value::Bool(_) => {
                        return Err(format!(
                            "Cannot destructure Bool in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                    Value::Function(_, _) => {
                        return Err(format!(
                            "Cannot destructure Function in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                    Value::Void => {
                        return Err(format!(
                            "Arg is Void in function '{}'",
                            self.program.var_name(function_id)
                        ))
                    }
                },
                DestructuringComponent::Integer(pattern_val) => match arg.as_ref() {
                    Value::Typed(id, _, _) => {
                        return Err(format!(
                            "Type '{}' cannot be matched to Int",
                            self.program.var_name(&id)
                        ));
                    }
                    Value::Integer(arg_val) => {
                        if pattern_val != arg_val {
                            return Ok(PatternMatchResult::no_match());
                        }
                    }
                    Value::String(_) => return Err(format!("String cannot be matched to Int")),
                    Value::Float(_) => return Err(format!("Float cannot be matched to Int")),
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to Int")),
                    Value::Function(_, _) => {
                        return Err(format!("Function cannot be matched to Int"))
                    }
                    Value::Void => return Err(format!("Arg is Void")),
                },
                DestructuringComponent::Float(pattern_val) => match arg.as_ref() {
                    Value::Typed(id, _, _) => {
                        return Err(format!(
                            "Type '{}' cannot be matched to Float",
                            self.program.var_name(&id)
                        ));
                    }
                    Value::Float(arg_val) => {
                        if pattern_val != arg_val {
                            return Ok(PatternMatchResult::no_match());
                        }
                    }
                    Value::Integer(_) => return Err(format!("Int cannot be matched to Float")),
                    Value::String(_) => return Err(format!("String cannot be matched to Float")),
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to Float")),
                    Value::Function(_, _) => {
                        return Err(format!("Function cannot be matched to Float"))
                    }
                    Value::Void => return Err(format!("Arg is Void")),
                },
                DestructuringComponent::String(pattern_val) => match arg.as_ref() {
                    Value::Typed(id, _, _) => {
                        return Err(format!(
                            "Type '{}' cannot be matched to Float",
                            self.program.var_name(&id)
                        ));
                    }
                    Value::String(arg_val) => {
                        if pattern_val != arg_val {
                            return Ok(PatternMatchResult::no_match());
                        }
                    }
                    Value::Integer(_) => return Err(format!("Int cannot be matched to String")),
                    Value::Float(_) => return Err(format!("Float cannot be matched to String")),
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to String")),
                    Value::Function(_, _) => {
                        return Err(format!("Function cannot be matched to String"))
                    }
                    Value::Void => return Err(format!("Arg is Void")),
                },
            }
        }
        return Ok(PatternMatchResult::with_match(bindings));
    }

    fn evaluate_function_call(
        &mut self,
        function_call: &FunctionCall,
        identifier_values: &Rc<HashMap<IdentifierId, Rc<Value>>>,
    ) -> Result<Rc<Value>, String> {
        match function_call.id {
            ADD_ID | SUB_ID | MUL_ID | DIV_ID => {
                if function_call.parameters.len() < 2 {
                    return Err(format!(
                        "Function '{}' requires at least 2 parameters but got '{}'",
                        self.program.var_name(&function_call.id),
                        function_call.parameters.len()
                    ));
                }

                let mut ints = Vec::new();
                let mut floats = Vec::new();
                let mut i = 1;
                for param in &function_call.parameters {
                    let result = self.evaluate_expression(identifier_values, param);
                    if result.is_err() {
                        return result;
                    }
                    match result.unwrap().as_ref() {
                        Value::Integer(x) => ints.push(*x),
                        Value::Float(x) => floats.push(*x),
                        _ => {
                            return Err(format!(
                                "Argument {i} of function '{}' is not numeric",
                                self.program.var_name(&function_call.id)
                            ))
                        }
                    }
                    i += 1;
                    if !ints.is_empty() && !floats.is_empty() {
                        return Err(format!(
                            "Arguments for function '{}' have different numeric types",
                            self.program.var_name(&function_call.id)
                        ));
                    }
                }
                if !ints.is_empty() {
                    let values = ints;
                    match function_call.id {
                        ADD_ID => Ok(Rc::new(Value::Integer(
                            values.into_iter().reduce(|acc, x| acc + x).unwrap(),
                        ))),
                        SUB_ID => Ok(Rc::new(Value::Integer(
                            values.into_iter().reduce(|acc, x| acc - x).unwrap(),
                        ))),
                        MUL_ID => Ok(Rc::new(Value::Integer(
                            values.into_iter().reduce(|acc, x| acc * x).unwrap(),
                        ))),
                        DIV_ID => Ok(Rc::new(Value::Integer(
                            values.into_iter().reduce(|acc, x| acc / x).unwrap(),
                        ))),
                        _ => panic!("Unhandled arithmetic function {}", function_call.id),
                    }
                } else {
                    let values = floats;
                    match function_call.id {
                        ADD_ID => Ok(Rc::new(Value::Float(
                            values.into_iter().reduce(|acc, x| acc + x).unwrap(),
                        ))),
                        SUB_ID => Ok(Rc::new(Value::Float(
                            values.into_iter().reduce(|acc, x| acc - x).unwrap(),
                        ))),
                        MUL_ID => Ok(Rc::new(Value::Float(
                            values.into_iter().reduce(|acc, x| acc * x).unwrap(),
                        ))),
                        DIV_ID => Ok(Rc::new(Value::Float(
                            values.into_iter().reduce(|acc, x| acc / x).unwrap(),
                        ))),
                        _ => panic!("Unhandled arithmetic function {}", function_call.id),
                    }
                }
            }
            GT_ID | LT_ID | EQ_ID | LE_ID | GE_ID => {
                if function_call.parameters.len() != 2 {
                    return Err(format!(
                        "Function '{}' requires 2 parameters but got '{}'",
                        self.program.var_name(&function_call.id),
                        function_call.parameters.len()
                    ));
                }

                let result =
                    self.evaluate_expression(identifier_values, &function_call.parameters[0]);
                if result.is_err() {
                    return result;
                }
                let left = result.unwrap();

                let result =
                    self.evaluate_expression(identifier_values, &function_call.parameters[1]);
                if result.is_err() {
                    return result;
                }
                let right = result.unwrap();

                let (left, right) = match (left.as_ref(), right.as_ref()) {
                    (Value::Integer(x), Value::Integer(y)) => (*x, *y),
                    (_, _) => {
                        return Err(format!(
                            "Arguments of '{}' are not numeric",
                            self.program.var_name(&function_call.id),
                        ))
                    }
                };

                match function_call.id {
                    GT_ID => Ok(Rc::new(Value::Bool(left > right))),
                    GE_ID => Ok(Rc::new(Value::Bool(left >= right))),
                    LT_ID => Ok(Rc::new(Value::Bool(left < right))),
                    LE_ID => Ok(Rc::new(Value::Bool(left <= right))),
                    EQ_ID => Ok(Rc::new(Value::Bool(left == right))),
                    _ => panic!(
                        "Unhandled boolean function {}",
                        self.program.var_name(&function_call.id)
                    ),
                }
            }
            WHILE_ID => {
                let condition =
                    self.evaluate_expression(identifier_values, &function_call.parameters[2])?;
                let (condition_def, condition_caps) = match condition.as_ref() {
                    Value::Function(def, captures) => (def, captures),
                    _ => {
                        return Err(format!(
                            "Expected function as condition in while but got '{:?}'",
                            condition.value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                        ))
                    }
                };

                let mut acc =
                    self.evaluate_expression(identifier_values, &function_call.parameters[0])?;
                let update =
                    self.evaluate_expression(identifier_values, &function_call.parameters[1])?;
                let (update_def, update_caps) = match update.as_ref() {
                    Value::Function(def, captures) => (def, captures),
                    _ => {
                        return Err(format!(
                            "Expected function as update in while but got '{:?}'",
                            update.value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                        ))
                    }
                };

                while self
                    .evaluate_function_definition(
                        condition_def,
                        &vec![Rc::clone(&acc)],
                        condition_caps,
                    )?
                    .as_boolean()
                {
                    acc = self.evaluate_function_definition(update_def, &vec![acc], update_caps)?;
                }

                Ok(acc)
            }
            RANGE_ID => {
                let n = self
                    .evaluate_expression(identifier_values, &function_call.parameters[0])?
                    .as_int();
                let mut result = Rc::new(Value::Typed(LIST_ID, EMPTY_LIST_ID, vec![]));

                for i in 0..n {
                    result = Rc::new(Value::Typed(
                        LIST_ID,
                        EMPTY_LIST_ID,
                        vec![Rc::new(Value::Integer(i)), result],
                    ));
                }

                Ok(result)
            }
            PRINT_ID | ERROR_ID | PRINTLN_ID => {
                let mut values = Vec::new();
                let mut i = 1;
                for param in &function_call.parameters {
                    let result = self.evaluate_expression(identifier_values, param);
                    if result.is_err() {
                        return result;
                    }
                    match result
                        .unwrap()
                        .value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                    {
                        Ok(val) => values.push(val),
                        Err(err) => return Err(format!("Cannot print argument {i}: {err}")),
                    }
                    i += 1;
                }

                match function_call.id {
                    PRINT_ID => {
                        print!("{}", values.join(" "));
                        Ok(Rc::new(Value::Void))
                    }
                    PRINTLN_ID => {
                        println!("{}", values.join(" "));
                        Ok(Rc::new(Value::Void))
                    }
                    ERROR_ID => Err(format!("{}", values.join(" "))),
                    _ => panic!("Shouldn't happen"),
                }
            }
            _ => {
                let mut captures = identifier_values.as_ref().clone();

                let definition = {
                    let mut definition = self.program.functions.get(&function_call.id);
                    if definition.is_none() {
                        let function_value = identifier_values.get(&function_call.id);
                        if function_value.is_none() {
                            return Err(format!(
                                "Function '{}' is not defined",
                                self.program.var_name(&function_call.id)
                            ));
                        }

                        match function_value.unwrap().as_ref() {
                            Value::Function(function_definition, lambda_captures) => {
                                definition = Some(function_definition);
                                for (k, v) in lambda_captures.as_ref() {
                                    captures.insert(*k, Rc::clone(v));
                                }
                            }
                            _ => {
                                return Err(format!(
                                    "'{}' is not a function",
                                    self.program.var_name(&function_call.id)
                                ));
                            }
                        }
                    }
                    Rc::clone(definition.unwrap())
                };

                //todo should be static checks
                let expected_arg_num = definition.arguments.len();
                let actual_arg_num = function_call.parameters.len();
                if expected_arg_num != actual_arg_num {
                    return Err(format!(
                        "Function '{}' expects {} arguments, but {} were provided",
                        self.program.var_name(&definition.id),
                        expected_arg_num,
                        actual_arg_num
                    ));
                }

                let captures = Rc::new(captures);
                let mut values = Vec::new();
                for param in &function_call.parameters {
                    values.push(self.evaluate_expression(&captures, &param)?);
                }
                self.evaluate_function_definition(&definition, &values, &captures)
            }
        }
    }

    fn evaluate_function_definition(
        &mut self,
        definition: &Rc<FunctionDefinition>,
        parameter_values: &Vec<Rc<Value>>,
        captures: &Rc<HashMap<IdentifierId, Rc<Value>>>,
    ) -> Result<Rc<Value>, String> {
        let mut arg_values = captures.as_ref().clone();
        let mut ordered_arg_values = Vec::new();
        for i in 0..parameter_values.len() {
            // Evaluate parameter value
            let value = Rc::clone(&parameter_values[i]);
            let arg_id = &definition.arguments[i].identifier;
            arg_values.insert(*arg_id, Rc::clone(&value));
            ordered_arg_values.push(value);
        }
        let arg_values = Rc::new(arg_values);

        if definition.is_not_pattern_matched() {
            return self.evaluate_expression(&arg_values, &definition.bodies[0].1);
        }

        for (pattern, expression) in &definition.bodies {
            let PatternMatchResult { is_match, bindings } =
                self.pattern_match(&definition.id, pattern, &ordered_arg_values)?;
            if is_match {
                if bindings.is_empty() {
                    return self.evaluate_expression(&arg_values, expression);
                }

                let mut new_bindings: HashMap<i32, Rc<Value>> = arg_values.as_ref().clone();
                for (k, v) in &bindings {
                    new_bindings.insert(*k, Rc::clone(v));
                }
                return self.evaluate_expression(&Rc::new(new_bindings), expression);
            }
        }

        //todo: should be caught at compile time
        Err(format!(
            "Non-exhaustive patterns in function '{}' for values: {}",
            self.program.var_name(&definition.id),
            ordered_arg_values
                .iter()
                .map(|v| v
                    .value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                    .unwrap())
                .collect::<Vec<String>>()
                .join(", "),
        ))
    }

    fn evaluate_expression(
        &mut self,
        identifier_values: &Rc<HashMap<IdentifierId, Rc<Value>>>,
        expression: &Expression,
    ) -> Result<Rc<Value>, String> {
        match expression {
            Expression::TypeConstructor(id, variant, expressions) => {
                let mut values = Vec::new();
                for expr in expressions {
                    let result = self.evaluate_expression(identifier_values, expr);
                    if result.is_err() {
                        return result;
                    }

                    values.push(result.unwrap());
                }

                Ok(Rc::new(Value::Typed(*id, *variant, values)))
            }
            Expression::FunctionCall(function_call) => {
                self.evaluate_function_call(function_call, identifier_values)
            }
            Expression::WithBlock(items, expression) => {
                let mut bindings = identifier_values.as_ref().clone();
                for (id, expr) in items {
                    let result = self.evaluate_expression(&Rc::new(bindings.clone()), expr);
                    if result.is_err() {
                        return result;
                    }
                    bindings.insert(*id, Rc::clone(&result.unwrap()));
                }
                return self.evaluate_expression(&Rc::new(bindings), expression);
            }
            Expression::If(condition, true_branch, false_branch) => {
                let condition = self.evaluate_expression(identifier_values, condition);
                if condition.is_err() {
                    return condition;
                }

                match condition.unwrap().as_ref() {
                    Value::Bool(condition) => {
                        if *condition {
                            self.evaluate_expression(identifier_values, true_branch)
                        } else {
                            self.evaluate_expression(identifier_values, false_branch)
                        }
                    }
                    _ => return Err(format!("Bug! If condition should be Bool")),
                }
            }
            Expression::Identifier(identifier) => {
                let result = identifier_values.get(identifier);
                if result.is_some() {
                    return Ok(Rc::clone(result.unwrap()));
                }

                let result = self.program.functions.get(identifier);
                if result.is_some() {
                    return Ok(Rc::new(Value::Function(
                        Rc::clone(result.unwrap()),
                        Rc::clone(&identifier_values),
                    )));
                }

                Err(format!(
                    "Unknown identifier '{}', known {:?}",
                    self.program.var_name(identifier),
                    identifier_values
                        .iter()
                        .map(|(k, _)| self.program.var_name(k))
                        .collect::<Vec<&Rc<String>>>()
                ))
            }
            Expression::Integer(x) => Ok(Rc::new(Value::Integer(*x))),
            Expression::String(x) => Ok(Rc::new(Value::String(Rc::clone(x)))),
            Expression::Float(x) => Ok(Rc::new(Value::Float(*x))),
            Expression::LambdaExpression(function_definition) => Ok(Rc::new(Value::Function(
                Rc::clone(function_definition),
                Rc::clone(&identifier_values),
            ))),
        }
    }

    fn var_name(&self, id: &i32) -> &Rc<String> {
        self.program.identifier_id_map.get_identifier(id).unwrap()
    }
}
