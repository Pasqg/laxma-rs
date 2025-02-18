use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;
use std::time::Instant;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::parser::combinators::ParserCombinator;
use crate::{compiler::grammar, parser::token_stream::TokenStream};

use super::identifier_map::{
    IdentifierId, ADD_ID, BOOL_ID, DIV_ID, EQ_ID, FALSE_ID, FLOAT_ID, GE_ID, GT_ID, INT_ID, LE_ID,
    LT_ID, MUL_ID, PRINT_ID, SUB_ID, TRUE_ID, WILDCARD_ID,
};
use super::internal_repr::{
    expression_repr, DestructuringComponent, Expression, FunctionCall, FunctionDefinition, Pattern,
    Program, Type,
};
use super::type_system::TypeInfo;

#[derive(Clone, Debug)]
//todo: add Rc<Type> to value to simply error checks
enum Value {
    Typed(IdentifierId, IdentifierId, Vec<Rc<Value>>),
    Integer(i64),
    Float(f32),
    Bool(bool),
    Function(Rc<FunctionDefinition>),
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
            Value::Function(_) => write!(f, "Function"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
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
            Value::Function(def) => Ok(format!("Function {}", id_to_name(&def.id))),
            Value::Integer(x) => Ok(format!("{x}")),
            Value::Float(x) => Ok(format!("{x}")),
            Value::Bool(x) => Ok(format!("{x}")),
            Value::Void => Ok("Void".to_string()),
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

fn range(n: usize) -> Vec<i64> {
    if n == 0 {
        return Vec::new();
    }
    let mut v = range(n - 1);
    v.push(n as i64);
    return v;
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
                let result = infer_function_type(&mut self.program, &self.type_info, &definition);
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
                let result = self.evaluate_expression(&values, &result.unwrap());

                if result.is_ok() {
                    //println!("{}", result.unwrap());
                    println!("Evaluated in {}us", start.elapsed().as_micros());

                    let start = Instant::now();
                    let x = range(1000);
                    println!("Evaluated{} in {}us", x.len(), start.elapsed().as_micros());
                } else {
                    println!("ERROR: {}", result.unwrap_err());
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
                        Value::Integer(_) => {
                            //todo: multiple "_" should also be considered wildcard
                            if *identifier != WILDCARD_ID {
                                return Err(format!("Redundant re-binding '{}' of integer argument {i} in function '{}'", self.program.var_name(identifier), self.program.var_name(function_id)));
                            }
                        }
                        Value::Float(_) => {
                            //todo: multiple "_" should also be considered wildcard
                            if *identifier != WILDCARD_ID {
                                return Err(format!("Redundant re-binding '{}' of float argument {i} in function '{}'", self.program.var_name(identifier), self.program.var_name(function_id)));
                            }
                        }
                        Value::Function(_) => {
                            //todo: multiple "_" should also be considered wildcard
                            if *identifier != WILDCARD_ID {
                                return Err(format!("Redundant re-binding '{}' of function argument {i} in function '{}'", self.program.var_name(identifier), self.program.var_name(function_id)));
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
                                _ => return Err("Unsupported".to_string()),
                            }
                        }
                    }
                    Value::Integer(_) => return Err(format!("Cannot destructure Int")),
                    Value::Float(_) => return Err(format!("Cannot destructure Float")),
                    Value::Bool(_) => return Err(format!("Cannot destructure Bool")),
                    Value::Function(_) => return Err(format!("Cannot destructure Function")),
                    Value::Void => return Err(format!("Arg is Void")),
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
                    Value::Float(_) => return Err(format!("Float cannot be matched to Int")),
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to Int")),
                    Value::Function(_) => return Err(format!("Function cannot be matched to Int")),
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
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to Float")),
                    Value::Function(_) => {
                        return Err(format!("Function cannot be matched to Float"))
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
        identifier_values: &HashMap<IdentifierId, Rc<Value>>,
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
            PRINT_ID => {
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
                println!("{}", values.join(" "));
                Ok(Rc::new(Value::Void))
            }
            _ => {
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
                        Value::Function(function_definition) => {
                            definition = Some(function_definition);
                        }
                        _ => {
                            return Err(format!(
                                "'{}' is not a function",
                                self.program.var_name(&function_call.id)
                            ));
                        }
                    }
                }
                let definition = Rc::clone(&definition.unwrap());

                //todo should be static checks
                let expected_arg_num = definition.arguments.len();
                let actual_arg_num = function_call.parameters.len();
                if expected_arg_num != actual_arg_num {
                    return Err(format!(
                        "Function '{}' expects {} arguments, but {} were provided",
                        self.program.var_name(&function_call.id),
                        expected_arg_num,
                        actual_arg_num
                    ));
                }

                let mut arg_values = identifier_values.clone();
                let mut ordered_arg_values = Vec::new();
                for i in 0..actual_arg_num {
                    // Evaluate parameter value
                    let result =
                        self.evaluate_expression(&identifier_values, &function_call.parameters[i]);
                    if result.is_err() {
                        return result;
                    }

                    // Verify type matches
                    // todo: move to type system, this is about 5-10% of evaluation
                    let value = result.unwrap();
                    let arg_id = &definition.arguments[i].identifier;
                    let arg_type = &definition.arguments[i].typing;
                    match value.as_ref() {
                        Value::Typed(type_id, variant, _) => {
                            match arg_type.as_ref() {
                                Type::SimpleType(simple_type_id) => {
                                    if simple_type_id != type_id {
                                        return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_type.id()), self.var_name(&simple_type_id)));
                                    }
                                    if !self.program.types.contains_key(simple_type_id) {
                                        return Err(format!(
                                            "Type '{}' is not defined",
                                            self.program.var_name(simple_type_id)
                                        ));
                                    }
                                    let definition =
                                        self.program.types.get(simple_type_id).unwrap();
                                    if !definition.variants.contains_key(variant) {
                                        return Err(format!("Argument '{}' in function call '{}' has undefined variant '{}' for type '{}'", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&variant), self.var_name(&simple_type_id)));
                                    }
                                }
                                //todo: should track that the same type is bound to the same type parameter
                                Type::TypeParameter(_) => {}
                                Type::ParametrizedType(_, _) => {}
                                Type::FunctionType(_, _, _) => {
                                    panic!("What to do 2")
                                }
                                Type::Unknown => {
                                    return Err(format!("Unknown value is not supported for argument '{}' in function '{}'", self.var_name(&arg_id), self.var_name(&function_call.id)));
                                }
                            }
                        }
                        Value::Integer(_) => match arg_type.as_ref() {
                            Type::SimpleType(id) if *id == INT_ID => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Int' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_id)));
                            }
                        },
                        Value::Float(_) => match arg_type.as_ref() {
                            Type::SimpleType(id) if *id == FLOAT_ID => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Float' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_id)));
                            }
                        },
                        Value::Bool(_) => match arg_type.as_ref() {
                            Type::SimpleType(id) if *id == BOOL_ID => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Bool' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_id)));
                            }
                        },
                        Value::Function(def) => match arg_type.as_ref() {
                            Type::FunctionType(_, _, _) => {
                                let function_type =
                                    infer_function_type(&mut self.program, &self.type_info, def);
                                if function_type.is_err() {
                                    return Err(function_type.unwrap_err());
                                }
                                let function_type = function_type.unwrap();

                                if function_type != Rc::clone(&arg_type) {
                                    return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_id), self.var_name(&function_type.id())));
                                }
                            }
                            _ => {
                                let function_type =
                                    infer_function_type(&mut self.program, &self.type_info, def);
                                if function_type.is_err() {
                                    return Err(function_type.unwrap_err());
                                }
                                let function_type = function_type.unwrap();

                                return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", self.var_name(&arg_id), self.var_name(&function_call.id), self.var_name(&arg_id), self.var_name(&function_type.id())));
                            }
                        },
                        Value::Void => {
                            return Err(format!(
                                "Void is not a valid argument type in function '{}'",
                                self.var_name(&function_call.id)
                            ))
                        }
                    };
                    arg_values.insert(*arg_id, Rc::clone(&value));
                    ordered_arg_values.push(value);
                }

                if definition.is_not_pattern_matched() {
                    return self.evaluate_expression(&arg_values, &definition.bodies[0].1);
                }

                for (pattern, expression) in &definition.bodies {
                    let result =
                        self.pattern_match(&function_call.id, pattern, &ordered_arg_values);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    let result = result.unwrap();
                    if result.is_match {
                        return if !result.bindings.is_empty() {
                            let mut bindings = arg_values.clone();
                            for (k, v) in &result.bindings {
                                bindings.insert(*k, Rc::clone(v));
                            }
                            self.evaluate_expression(&bindings, expression)
                        } else {
                            self.evaluate_expression(&arg_values, expression)
                        };
                    }
                }

                //todo: should be caught at compile time
                Err(format!(
                    "Non-exhaustive patterns in function '{}' for values: {}",
                    self.program.var_name(&function_call.id),
                    ordered_arg_values
                        .iter()
                        .map(|v| v
                            .value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                            .unwrap())
                        .collect::<Vec<String>>()
                        .join(", "),
                ))
            }
        }
    }

    fn evaluate_expression(
        &mut self,
        identifier_values: &HashMap<IdentifierId, Rc<Value>>,
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
                let mut bindings = identifier_values.clone();
                for (id, expr) in items {
                    let result = self.evaluate_expression(&bindings, expr);
                    if result.is_err() {
                        return result;
                    }
                    bindings.insert(*id, Rc::clone(&result.unwrap()));
                }
                return self.evaluate_expression(&bindings, expression);
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
                    return Ok(Rc::new(Value::Function(Rc::clone(result.unwrap()))));
                }

                Err(format!(
                    "Unknown identifier '{}'",
                    self.program.var_name(identifier)
                ))
            }
            Expression::Integer(x) => Ok(Rc::new(Value::Integer(*x))),
            Expression::Float(x) => Ok(Rc::new(Value::Float(*x))),
            Expression::LambdaExpression(function_definition) => {
                Ok(Rc::new(Value::Function(Rc::clone(function_definition))))
            }
        }
    }

    fn var_name(&self, id: &i32) -> &Rc<String> {
        self.program.identifier_id_map.get_identifier(id).unwrap()
    }
}
