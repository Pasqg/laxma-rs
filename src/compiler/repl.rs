use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::infer_function_type;
use crate::parser::combinators::ParserCombinator;
use crate::utils::InsertionOrderHashMap;
use crate::{compiler::grammar, parser::token_stream::TokenStream};

use super::internal_repr::{
    expression_repr, DestructuringComponent, Expression, FunctionCall, FunctionDefinition, Pattern,
    Program, Type, TypeVariant,
};
use super::type_system::TypeInfo;

#[derive(Clone, Debug)]
enum Value {
    Typed(String, String, Vec<Rc<Value>>),
    Num(i64),
    Bool(bool),
    Function(FunctionDefinition),
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
            Value::Num(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Void => write!(f, "Void"),
        }
    }
}

impl Value {
    fn value_to_str(&self) -> Result<String, String> {
        match self {
            Value::Typed(name, variant, values) => {
                let mut args = Vec::new();
                for value in values {
                    let result = value.value_to_str();
                    if result.is_err() {
                        return result;
                    }
                    args.push(result.unwrap());
                }
                Ok(format!("{name}::{variant}({})", args.join(", ")))
            }
            Value::Function(_) => Ok(format!("Function")),
            Value::Num(x) => Ok(format!("{x}")),
            Value::Bool(x) => Ok(format!("{x}")),
            Value::Void => Ok("Void".to_string()),
        }
    }
}

#[derive(Clone, Debug)]
struct PatternMatchResult {
    is_match: bool,
    bindings: HashMap<Rc<String>, Rc<Value>>,
}

impl PatternMatchResult {
    fn no_match() -> Self {
        Self {
            is_match: false,
            bindings: HashMap::new(),
        }
    }

    fn with_match(bindings: HashMap<Rc<String>, Rc<Value>>) -> Self {
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
            program: Program {
                functions: InsertionOrderHashMap::new(),
                types: HashMap::new(),
            },
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
            println!("{:?}", result);
        }

        if !result.result || result.remaining.not_done() {
            println!("ERROR: Failed to parse! {:?}", result.remaining);
            return;
        }

        let ast = result.ast;
        let result = to_repr(&ast);
        if result.is_ok() {
            let Program { functions, types } = result.unwrap();
            for (name, definition) in types {
                println!("Defined type {}", name);

                self.type_info.add_user_type(name.clone(), &definition);
                self.program.types.insert(name, definition);
            }
            for key in functions.keys() {
                let (name, definition) = (key, functions.get(key.as_ref()).unwrap());
                let result = infer_function_type(&self.program, &self.type_info, &definition);
                if result.is_err() {
                    println!("ERROR: {}", result.unwrap_err());
                } else {
                    let function_type = result.unwrap();
                    let return_type = match function_type.as_ref() {
                        Type::FunctionType(_, return_type) => Rc::clone(return_type),
                        _ => {
                            println!("ERROR: Expected function type but got {}", function_type.name());
                            return;
                        }
                    };
                    println!("Defined function {}: {}", name, function_type.name());

                    self.program.functions.insert(name.clone(), definition);
                    self.type_info.function_return_types.insert(name, return_type);
                }
            }
        } else {
            let original_error = result.unwrap_err();
            let result = expression_repr(&ast);
            if result.is_ok() {
                let result = self.evaluate_expression(
                    &HashMap::from([
                        (Rc::new("true".to_string()), Rc::new(Value::Bool(true))),
                        (Rc::new("false".to_string()), Rc::new(Value::Bool(false))),
                    ]),
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

    fn pattern_match(
        function_name: &str,
        pattern: &Pattern,
        args: &Vec<Rc<Value>>,
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
                DestructuringComponent::Identifier(identifier) => match arg.as_ref() {
                    Value::Typed(name, variant, values) => {
                        if identifier.as_str() != "_" {
                            if identifier.as_str() != variant.as_str() {
                                return Ok(PatternMatchResult::no_match());
                            }

                            if !values.is_empty() {
                                return Err(format!("Cannot destructure variant '{variant}' of type '{name}' with zero elements, because constructor requires {} arguments", values.len()));
                            }
                        }
                    }
                    Value::Num(_) => {
                        //todo: multiple "_" should also be considered wildcard
                        if identifier.as_str() != "_" {
                            return Err(format!("Redundant re-binding '{identifier}' of numerical argument {i} in function '{function_name}'"));
                        }
                    }
                    Value::Function(_) => {
                        //todo: multiple "_" should also be considered wildcard
                        if identifier.as_str() != "_" {
                            return Err(format!("Redundant re-binding '{identifier}' of function argument {i} in function '{function_name}'"));
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
                DestructuringComponent::Destructuring(destructuring) => match arg.as_ref() {
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

                            match (pattern, value.as_ref()) {
                                (DestructuringComponent::Identifier(i), _) if i.as_str() == "_" => {
                                }
                                (DestructuringComponent::Identifier(i), Value::Typed(_, _, _)) => {
                                    bindings.insert(i.clone(), value.clone());
                                }
                                (DestructuringComponent::Number(x), Value::Num(y)) => {
                                    if x != y {
                                        return Ok(PatternMatchResult::no_match());
                                    }
                                }
                                (DestructuringComponent::Identifier(i), Value::Num(_)) => {
                                    bindings.insert(i.clone(), value.clone());
                                }
                                (DestructuringComponent::Identifier(x), Value::Bool(y)) => {
                                    if (x.as_str() == "true" && !y)
                                        || (x.as_str() == "false" && y.to_owned())
                                    {
                                        return Ok(PatternMatchResult::no_match());
                                    }
                                    bindings.insert(x.clone(), value.clone());
                                }
                                _ => return Err("Unsupported".to_string()),
                            }
                        }
                    }
                    Value::Num(_) => return Err(format!("Cannot destructure Int")),
                    Value::Bool(_) => return Err(format!("Cannot destructure Bool")),
                    Value::Function(_) => return Err(format!("Cannot destructure Function")),
                    Value::Void => return Err(format!("Arg is Void")),
                },
                DestructuringComponent::Number(pattern_val) => match arg.as_ref() {
                    Value::Typed(name, _, _) => {
                        return Err(format!("Type '{name}' cannot be matched to Int"));
                    }
                    Value::Num(arg_val) => {
                        if pattern_val != arg_val {
                            return Ok(PatternMatchResult::no_match());
                        }
                    }
                    Value::Bool(_) => return Err(format!("Bool cannot be matched to Int")),
                    Value::Function(_) => return Err(format!("Function cannot be matched to Int")),
                    Value::Void => return Err(format!("Arg is Void")),
                },
            }
        }
        return Ok(PatternMatchResult::with_match(bindings));
    }

    fn evaluate_function_call(
        &self,
        function_call: &FunctionCall,
        identifier_values: &HashMap<Rc<String>, Rc<Value>>,
    ) -> Result<Rc<Value>, String> {
        match function_call.name.as_str() {
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
                    let result = self.evaluate_expression(identifier_values, param);
                    if result.is_err() {
                        return result;
                    }
                    match result.unwrap().as_ref() {
                        Value::Num(x) => values.push(x.to_owned()),
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
                    "+" => Ok(Rc::new(Value::Num(
                        values.into_iter().reduce(|acc, x| acc + x).unwrap(),
                    ))),
                    "-" => Ok(Rc::new(Value::Num(
                        values.into_iter().reduce(|acc, x| acc - x).unwrap(),
                    ))),
                    "*" => Ok(Rc::new(Value::Num(
                        values.into_iter().reduce(|acc, x| acc * x).unwrap(),
                    ))),
                    "/" => Ok(Rc::new(Value::Num(
                        values.into_iter().reduce(|acc, x| acc / x).unwrap(),
                    ))),
                    _ => panic!("Unhandled arithmetic function {}", function_call.name),
                }
            }
            ">" | "<" | "==" => {
                if function_call.parameters.len() != 2 {
                    return Err(format!(
                        "Function '{}' requires 2 parameters but got '{}'",
                        function_call.name,
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
                    (Value::Num(x), Value::Num(y)) => (x.to_owned(), y.to_owned()),
                    (_, _) => {
                        return Err(format!(
                            "Arguments of '{}' are not numeric",
                            function_call.name
                        ))
                    }
                };

                match function_call.name.as_str() {
                    ">" => Ok(Rc::new(Value::Bool(left > right))),
                    "<" => Ok(Rc::new(Value::Bool(left < right))),
                    "==" => Ok(Rc::new(Value::Bool(left == right))),
                    _ => panic!("Unhandled boolean function {}", function_call.name),
                }
            }
            "print" => {
                let mut values = Vec::new();
                let mut i = 1;
                for param in &function_call.parameters {
                    let result = self.evaluate_expression(identifier_values, param);
                    if result.is_err() {
                        return result;
                    }
                    match result.unwrap().value_to_str() {
                        Ok(val) => values.push(val),
                        Err(err) => return Err(format!("Cannot print argument {i}: {err}")),
                    }
                    i += 1;
                }
                println!("{}", values.join(" "));
                Ok(Rc::new(Value::Void))
            }
            _ => {
                let definition;
                if self.program.functions.contains_key(&function_call.name) {
                    definition = self.program.functions.get(&function_call.name).unwrap();
                } else {
                    let function_value = identifier_values.get(&function_call.name);
                    if function_value.is_none() {
                        return Err(format!("Function '{}' is not defined", function_call.name));
                    }

                    match function_value.unwrap().as_ref() {
                        Value::Function(function_definition) => {
                            definition = function_definition;
                        }
                        _ => {
                            return Err(format!("'{}' is not a function", function_call.name));
                        }
                    }
                }

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
                    let result =
                        self.evaluate_expression(&identifier_values, &function_call.parameters[i]);
                    if result.is_err() {
                        return result;
                    }

                    // Verify type matches
                    let value = result.unwrap();
                    let arg_name = &definition.arguments[i].identifier;
                    let arg_type = &definition.arguments[i].typing;
                    match value.as_ref() {
                        Value::Typed(name, variant, values) => {
                            match arg_type.as_ref() {
                                Type::SimpleType(simple_type) => {
                                    if simple_type.as_str() != name {
                                        return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", arg_name, function_call.name, arg_type.name(), simple_type));
                                    }
                                    if !self.program.types.contains_key(simple_type) {
                                        return Err(format!("Type '{simple_type}' is not defined"));
                                    }
                                    let definition = self.program.types.get(simple_type).unwrap();
                                    if !definition.variants.contains_key(variant) {
                                        return Err(format!("Argument '{arg_name}' in function call '{}' has undefined variant '{variant}' for type '{simple_type}'", function_call.name));
                                    }
                                }
                                //todo: should track that the same type is bound to the same type parameter
                                Type::TypeParameter(name) => {}
                                Type::ParametrizedType(name, items) => {}
                                Type::FunctionType(items, _) => {
                                    panic!("What to do 2")
                                }
                                Type::Unknown => {
                                    return Err(format!("Unknown value is not supported for argument '{arg_name}' in function '{}'", function_call.name));
                                }
                            }
                        }
                        Value::Num(_) => match arg_type.as_ref() {
                            Type::SimpleType(name) if name.as_str() == "Int" => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Int' but '{}' was provided", arg_name, function_call.name, arg_type.name()));
                            }
                        },
                        Value::Bool(_) => match arg_type.as_ref() {
                            Type::SimpleType(name) if name.as_str() == "Bool" => {}
                            _ => {
                                return Err(format!("Argument '{}' in function '{}' has type 'Bool' but '{}' was provided", arg_name, function_call.name, arg_type.name()));
                            }
                        },
                        Value::Function(def) => match arg_type.as_ref() {
                            Type::FunctionType(_, _) => {
                                let function_type = infer_function_type(&self.program, &self.type_info, def);
                                if function_type.is_err() {
                                   return Err(function_type.unwrap_err()); 
                                }
                                let function_type = function_type.unwrap();

                                if function_type != Rc::clone(&arg_type) {
                                    return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", arg_name, function_call.name, arg_type.name(), function_type.name()));
                                }
                            }
                            _ => {
                                let function_type = infer_function_type(&self.program, &self.type_info, def);
                                if function_type.is_err() {
                                   return Err(function_type.unwrap_err()); 
                                }
                                let function_type = function_type.unwrap();

                                return Err(format!("Argument '{}' in function '{}' has type '{}' but '{}' was provided", arg_name, function_call.name, arg_type.name(), function_type.name()));
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
                    return self.evaluate_expression(&arg_values, &definition.bodies[0].1);
                }

                for (pattern, expression) in &definition.bodies {
                    let result =
                        REPL::pattern_match(&function_call.name, pattern, &ordered_arg_values);
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    let result = result.unwrap();
                    if result.is_match {
                        let mut bindings = arg_values.clone();
                        for (k, v) in &result.bindings {
                            bindings.insert(k.clone(), v.clone());
                        }
                        return self.evaluate_expression(&bindings, expression);
                    }
                }

                //todo: should be caught at compile time
                Err(format!(
                    "Non-exhaustive patterns in function '{}'",
                    function_call.name
                ))
            }
        }
    }

    fn evaluate_expression(
        &self,
        identifier_values: &HashMap<Rc<String>, Rc<Value>>,
        expression: &Expression,
    ) -> Result<Rc<Value>, String> {
        match expression {
            Expression::TypeConstructor(name, variant, expressions) => {
                if !self.type_info.user_types.contains_key(name) {
                    return Err(format!("Type '{name}' is not defined"));
                }

                let type_definition = self.program.types.get(name).unwrap();
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
                    let result = self.evaluate_expression(identifier_values, expr);
                    if result.is_err() {
                        return result;
                    }

                    values.push(result.unwrap());
                }
                if values.len() > 0 {
                    match type_variant {
                        TypeVariant::Cartesian(variant, items) => {
                            for i in 0..values.len() {
                                match (values[i].as_ref(), items[i].as_ref()) {
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
                Ok(Rc::new(Value::Typed(name.clone(), variant.clone(), values)))
            }
            Expression::FunctionCall(function_call) => {
                self.evaluate_function_call(function_call, identifier_values)
            }
            Expression::WithBlock(items, expression) => {
                let mut bindings = identifier_values.clone();
                for (identifier, expr) in items {
                    let result = self.evaluate_expression(&bindings, expr);
                    if result.is_err() {
                        return result;
                    }
                    bindings.insert(identifier.clone(), result.unwrap());
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
                        if condition.to_owned() {
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
                    return Ok(Rc::new(Value::Function(result.unwrap().clone())));
                }
               
                 Err(format!(
                    "Unknown identifier '{}'  | {:?}",
                    identifier, identifier_values
                ))
            }
            Expression::Number(x) => Ok(Rc::new(Value::Num(x.to_owned()))),
        }
    }
}
