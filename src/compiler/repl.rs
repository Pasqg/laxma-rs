use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;
use std::vec;

use nohash_hasher::IntMap;

use crate::compiler::grammar;
use crate::compiler::identifier_map::{UNDECIDED_ID, VOID_ID};
use crate::compiler::internal_repr::to_repr;
use crate::compiler::type_system::{infer_function_type, verify_type_definition};
use crate::parser::combinators::ParserCombinator;
use crate::parser::token_stream::TokenStream;

use super::identifier_map::{
    IdentifierId, ADD_ID, DIV_ID, EMPTY_LIST_ID, EQ_ID, ERROR_ID, FALSE_ID, FOLDL_ID, GE_ID, GT_ID,
    LE_ID, LIST_ID, LT_ID, MUL_ID, PRINTLN_ID, PRINT_ID, RANGE_ID, REPL_ID, SUB_ID, TRUE_ID,
    WHILE_ID, WILDCARD_ID,
};
use super::internal_repr::{
    expression_repr, DestructuringComponent, Expression, FunctionCall, FunctionDefinition, Pattern,
    Program,
};
use super::lexer::Lexer;
use super::type_system::{infer_expression_type, TypeInfo, infer_function_definition_type, verify_type_definition};
use super::utils::to_int_map;
use super::value::{RcValue, Value};

#[derive(Clone, Debug)]
struct PatternMatchResult {
    is_match: bool,
    bindings: IntMap<IdentifierId, RcValue>,
}

impl PatternMatchResult {
    fn no_match() -> Self {
        Self {
            is_match: false,
            bindings: IntMap::default(),
        }
    }

    fn with_match(bindings: IntMap<IdentifierId, RcValue>) -> Self {
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

    pub fn handle_input(&mut self, input: &str) -> Result<String, String> {
        let tokens = Lexer::token_stream(input);
        self.handle_tokens(&tokens)
    }

    pub fn handle_tokens(&mut self, tokens: &TokenStream) -> Result<String, String> {
        //todo: parse should return Result<ParserResult>
        let mut result = grammar::program_parser().parse(tokens);
        if !result.result || result.remaining.not_done() {
            result = grammar::expression_parser().parse(tokens);
        }

        if !result.result || result.remaining.not_done() {
            return Err(format!("ERROR: Failed to parse! {:?}", result.remaining));
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
                verify_type_definition(&mut self.program, &self.type_info, &definition)?;

                println!("Defined type {}", self.var_name(&id));

                self.type_info.add_user_type(id, &definition);
                self.program.types.insert(id, definition);
            }

            for key in functions.keys() {
                let (id, definition) = (key, functions.get(key.as_ref()).unwrap());
                let function_type =
                    infer_function_definition_type(&mut self.program, &self.type_info, &definition, None)?;

                println!(
                    "Defined function {}: {}",
                    self.program.var_name(id),
                    function_type.full_repr(&self.program.identifier_id_map)
                );

                self.program.functions.insert(**id, Rc::clone(definition));
                self.type_info.function_types.insert(**id, function_type);
            }

            return Ok(String::new());
        }

        let original_error = result.unwrap_err();
        let result = expression_repr(&ast, &mut self.program.identifier_id_map);
        if result.is_ok() {
            //todo dedup with type system
            let values = to_int_map(HashMap::from([
                (TRUE_ID, Rc::new(Value::Bool(true))),
                (FALSE_ID, Rc::new(Value::Bool(false))),
                (VOID_ID, Rc::new(Value::Void)),
                (UNDECIDED_ID, Rc::new(Value::Undecided)),
            ]));
            let start = Instant::now();
            let expression = result.unwrap();
            let expr_type = infer_expression_type(
                &expression,
                &mut self.program,
                &self.type_info,
                &Rc::clone(&self.type_info.constant_types),
                &REPL_ID,
            )?;
            let result = self.evaluate_expression(&REPL_ID, &Rc::new(values), &expression)?;

            println!(
                "\nType: {}",
                &expr_type.full_repr(&self.program.identifier_id_map)
            );
            println!("Evaluated in {}us", start.elapsed().as_micros());
            return Ok(result.value_to_str(&|id| Rc::clone(self.program.var_name(id)))?);
        }

        let expr_error = result.unwrap_err();
        if expr_error.as_str() == "Expected Expression but got Some(Program)" {
            return Err(original_error);
        }

        Err(format!("ERROR(s): {} {}", original_error, expr_error))
    }

    fn pattern_match(
        &self,
        function_id: &IdentifierId,
        pattern: &Pattern,
        args: &Vec<RcValue>,
    ) -> Result<PatternMatchResult, String> {
        if pattern.components.len() != args.len() {
            return Err(format!(
                "Found pattern with {} elements but function '{function_id}' has {} arguments",
                pattern.components.len(),
                args.len()
            ));
        }

        let mut bindings = IntMap::default();
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
                        Value::Void => return Err(format!("Argument is Void")),
                        Value::Undecided => return Err(format!("Argument has undecided type")),
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
                    Value::Undecided => {
                        return Err(format!(
                            "Arg has undecided type in function '{}'",
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
                    Value::Void => return Err(format!("Argument is Void")),
                    Value::Undecided => return Err(format!("Argument has undecided type")),
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
                    Value::Undecided => return Err(format!("Arg has undecided type")),
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
                    Value::Undecided => return Err(format!("Arg has undecided type")),
                },
            }
        }
        return Ok(PatternMatchResult::with_match(bindings));
    }

    fn evaluate_function_call(
        &self,
        caller_id: &IdentifierId,
        function_call: &FunctionCall,
        identifier_values: &Rc<IntMap<IdentifierId, RcValue>>,
    ) -> Result<RcValue, String> {
        let mut captures = identifier_values.as_ref().clone();

        let definition = {
            let function_value = identifier_values.get(&function_call.id);
            let definition;
            if function_value.is_some() && *caller_id != function_call.id {
                let function_value = function_value.unwrap();
                match function_value.as_ref() {
                    Value::Function(function_definition, lambda_captures) => {
                        definition = Some(function_definition);
                        for (k, v) in lambda_captures.as_ref() {
                            captures.insert(*k, Rc::clone(v));
                        }
                    }
                    _ => {
                        return Err(format!(
                            "'{}' is not a function: '{}'",
                            self.program.var_name(&function_call.id),
                            function_value
                                .value_to_str(&|id| Rc::clone(self.program.var_name(id)))?,
                        ));
                    }
                }
            } else {
                definition = self.program.functions.get(&function_call.id);
            }

            if definition.is_none() {
                if function_value.is_none() {
                    return Err(format!(
                        "Function '{}' is not defined",
                        self.program.var_name(&function_call.id)
                    ));
                }
            }
            Rc::clone(definition.unwrap())
        };

        //todo should be static checks
        let expected_arg_num = definition.arguments.len();
        let actual_arg_num = function_call.arguments.len();
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
        for param in &function_call.arguments {
            values.push(self.evaluate_expression(caller_id, &captures, &param)?);
        }
        self.evaluate_function_definition(&definition, &values, &captures)
    }

    fn evaluate_math_operator(
        &self,
        id: &IdentifierId,
        ordered_arg_values: &[RcValue],
    ) -> Result<RcValue, String> {
        let a = ordered_arg_values[0].as_int();
        let b = ordered_arg_values[1].as_int();
        match *id {
            ADD_ID => Ok(Rc::new(Value::Integer(a + b))),
            SUB_ID => Ok(Rc::new(Value::Integer(a - b))),
            MUL_ID => Ok(Rc::new(Value::Integer(a * b))),
            DIV_ID => Ok(Rc::new(Value::Integer(a / b))),
            _ => Err(format!(
                "Unhandled arithmetic function {}",
                self.program.var_name(id)
            )),
        }
    }

    fn evaluate_boolean_operator(
        &self,
        id: &IdentifierId,
        ordered_arg_values: &[RcValue],
    ) -> Result<RcValue, String> {
        let left = ordered_arg_values[0].as_int();
        let right = ordered_arg_values[1].as_int();

        match *id {
            GT_ID => Ok(Rc::new(Value::Bool(left > right))),
            GE_ID => Ok(Rc::new(Value::Bool(left >= right))),
            LT_ID => Ok(Rc::new(Value::Bool(left < right))),
            LE_ID => Ok(Rc::new(Value::Bool(left <= right))),
            EQ_ID => Ok(Rc::new(Value::Bool(left == right))),
            _ => Err(format!(
                "Unhandled boolean function {}",
                self.program.var_name(&id)
            )),
        }
    }

    fn evaluate_while(&self, ordered_arg_values: &[RcValue]) -> Result<RcValue, String> {
        let condition = &ordered_arg_values[2];
        let (condition_def, condition_caps) = match condition.as_ref() {
            Value::Function(def, captures) => (def, captures),
            _ => {
                return Err(format!(
                    "Expected function as condition in while but got '{:?}'",
                    condition.value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                ));
            }
        };

        let mut acc = Rc::clone(&ordered_arg_values[0]);
        let update = &ordered_arg_values[1];
        let (update_def, update_caps) = match update.as_ref() {
            Value::Function(def, captures) => (def, captures),
            _ => {
                return Err(format!(
                    "Expected function as update in while but got '{:?}'",
                    update.value_to_str(&|id| Rc::clone(self.program.var_name(id)))
                ));
            }
        };

        while self
            .evaluate_function_definition(condition_def, &vec![Rc::clone(&acc)], condition_caps)?
            .as_boolean()
        {
            acc = self.evaluate_function_definition(update_def, &vec![acc], update_caps)?;
        }

        Ok(acc)
    }

    fn evaluate_foldl(&self, ordered_arg_values: &[RcValue]) -> Result<RcValue, String> {
        let (f, captures) = ordered_arg_values[0].as_function();
        let mut z = Rc::clone(&ordered_arg_values[1]);
        let mut xs = Rc::clone(&ordered_arg_values[2]);

        let empty_list = Rc::new(Value::Typed(LIST_ID, EMPTY_LIST_ID, vec![]));
        while xs != empty_list {
            let (new_z, new_xs) = match xs.as_ref() {
                Value::Typed(_, _, values) => {
                    let x = Rc::clone(&values[0]);
                    let xs = Rc::clone(&values[1]);
                    (
                        self.evaluate_function_definition(f, &vec![z, x], captures)?,
                        xs,
                    )
                }
                _ => {
                    return Err(format!("Expected List in foldl but got '{:?}'", xs));
                }
            };
            z = new_z;
            xs = new_xs;
        }
        Ok(z)
    }

    fn try_evaluate_builtin(
        &self,
        id: &IdentifierId,
        ordered_arg_values: &[RcValue],
    ) -> Option<Result<RcValue, String>> {
        match *id {
            ADD_ID | SUB_ID | MUL_ID | DIV_ID => {
                Some(self.evaluate_math_operator(id, ordered_arg_values))
            }
            GT_ID | LT_ID | EQ_ID | LE_ID | GE_ID => {
                Some(self.evaluate_boolean_operator(id, ordered_arg_values))
            }
            WHILE_ID => Some(self.evaluate_while(ordered_arg_values)),
            FOLDL_ID => Some(self.evaluate_foldl(ordered_arg_values)),
            RANGE_ID => {
                let n = ordered_arg_values[0].as_int();
                let mut result = Rc::new(Value::Typed(LIST_ID, EMPTY_LIST_ID, vec![]));

                for i in 0..n {
                    result = Rc::new(Value::Typed(
                        LIST_ID,
                        LIST_ID,
                        vec![Rc::new(Value::Integer(i)), result],
                    ));
                }

                Some(Ok(result))
            }
            PRINT_ID | ERROR_ID | PRINTLN_ID => {
                let mut values = Vec::new();
                let mut i = 1;
                for param in ordered_arg_values {
                    match param.value_to_str(&|id| Rc::clone(self.program.var_name(id))) {
                        Ok(val) => values.push(val),
                        Err(err) => return Some(Err(format!("Cannot print argument {i}: {err}"))),
                    }
                    i += 1;
                }

                let result = match *id {
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
                };
                Some(result)
            }
            _ => None,
        }
    }

    fn evaluate_function_definition(
        &self,
        definition: &Rc<FunctionDefinition>,
        parameter_values: &Vec<RcValue>,
        captures: &Rc<IntMap<IdentifierId, RcValue>>,
    ) -> Result<RcValue, String> {
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

        let builtin_result = self.try_evaluate_builtin(&definition.id, &ordered_arg_values);
        if builtin_result.is_some() {
            return builtin_result.unwrap();
        }

        let function_id = &definition.id;
        if definition.is_not_pattern_matched() {
            return self.evaluate_expression(function_id, &arg_values, &definition.bodies[0].1);
        }

        for (pattern, expression) in &definition.bodies {
            let PatternMatchResult { is_match, bindings } =
                self.pattern_match(function_id, pattern, &ordered_arg_values)?;
            if is_match {
                if bindings.is_empty() {
                    return self.evaluate_expression(function_id, &arg_values, expression);
                }

                let mut new_bindings = arg_values.as_ref().clone();
                for (k, v) in &bindings {
                    new_bindings.insert(*k, Rc::clone(v));
                }
                return self.evaluate_expression(function_id, &Rc::new(new_bindings), expression);
            }
        }

        //todo: should be caught at compile time
        Err(format!(
            "Non-exhaustive patterns in function '{}' for values: {}",
            self.program.var_name(function_id),
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
        &self,
        caller_id: &IdentifierId,
        identifier_values: &Rc<IntMap<IdentifierId, RcValue>>,
        expression: &Expression,
    ) -> Result<RcValue, String> {
        match expression {
            Expression::TypeConstructor(id, variant, expressions) => {
                let mut values = Vec::new();
                for expr in expressions {
                    values.push(self.evaluate_expression(caller_id, identifier_values, expr)?);
                }

                Ok(Rc::new(Value::Typed(*id, *variant, values)))
            }
            Expression::FunctionCall(function_call) => {
                self.evaluate_function_call(caller_id, function_call, identifier_values)
            }
            Expression::WithBlock(items, expression) => {
                let mut bindings = identifier_values.as_ref().clone();
                for (id, expr) in items {
                    let result =
                        self.evaluate_expression(caller_id, &Rc::new(bindings.clone()), expr)?;
                    bindings.insert(*id, Rc::clone(&result));
                }
                return self.evaluate_expression(caller_id, &Rc::new(bindings), expression);
            }
            Expression::If(condition, true_branch, false_branch) => {
                let condition =
                    self.evaluate_expression(caller_id, identifier_values, condition)?;
                match condition.as_ref() {
                    Value::Bool(condition) => {
                        if *condition {
                            self.evaluate_expression(caller_id, identifier_values, true_branch)
                        } else {
                            self.evaluate_expression(caller_id, identifier_values, false_branch)
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

#[cfg(test)]
mod tests {
    use crate::utils::InsertionOrderHashMap;

    use super::REPL;

    #[test]
    fn test_primitive_type() {
        run_test("type A -> A", ok(""));
        run_test("type A -> A | B", ok(""));
        run_test("type A -> A | B | C", ok(""));
    }

    #[test]
    fn test_primitive_type_errors() {
        run_test("type A -> A | A", err("Duplicate variant 'A' of type 'A'"));
    }

    #[test]
    fn test_primitive_type_construction() {
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A", ok("")),
            ("A::A()", ok("A::A()")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A | B", ok("")),
            ("A::A()", ok("A::A()")),
            ("A::B()", ok("A::B()")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A | B | C", ok("")),
            ("A::A()", ok("A::A()")),
            ("A::B()", ok("A::B()")),
            ("A::C()", ok("A::C()")),
        ]));
    }

    #[test]
    fn test_primitive_type_construction_errors() {
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A", ok("")),
            (
                "A::A(0)",
                err("Variant 'A' of type 'A' expects 0 arguments but constructor provided 1"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A | B", ok("")),
            (
                "A::A(0)",
                err("Variant 'A' of type 'A' expects 0 arguments but constructor provided 1"),
            ),
            (
                "A::B(0)",
                err("Variant 'B' of type 'A' expects 0 arguments but constructor provided 1"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A", ok("")),
            (
                "A::B()",
                err("Undefined variant 'B' of type 'A' in function 'REPL'"),
            ),
        ]));
        run_test("A::A()", err("Undefined type 'A' in function 'REPL'"));
    }

    #[test]
    fn test_primitive_cartesian_type() {
        run_test("type A -> A Int", ok(""));
        run_test("type A -> A Float", ok(""));
        run_test("type A -> A String", ok(""));
        run_test("type A -> A Int Int", ok(""));
        run_test("type A -> A Int Int Int", ok(""));
    }

    #[test]
    fn test_primitive_cartesian_type_errors() {
        run_test(
            "type A -> A B",
            Err("Undefined type 'B' for A::A".to_string()),
        );
        run_test(
            "type A -> A B | B",
            Err("Undefined type 'B' for A::A".to_string()),
        );
    }

    #[test]
    fn test_primitive_cartesian_type_construction() {
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int", ok("")),
            ("A::A(0)", ok("A::A(0)")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Float", ok("")),
            ("A::A(0.0)", ok("A::A(0.0)")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A String", ok("")),
            ("A::A(\"a string\")", ok("A::A(\"a string\")")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int Int", ok("")),
            ("A::A(1 2)", ok("A::A(1, 2)")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int Int Int", ok("")),
            ("A::A(1 2 3)", ok("A::A(1, 2, 3)")),
        ]));
    }

    #[test]
    fn test_primitive_cartesian_type_construction_errors() {
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int", ok("")),
            (
                "A::A()",
                err("Variant 'A' of type 'A' expects 1 arguments but constructor provided 0"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int", ok("")),
            (
                "A::A(0 0)",
                err("Variant 'A' of type 'A' expects 1 arguments but constructor provided 2"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int", ok("")),
            (
                "A::A(0.0)",
                err("Expecting Int in constructor for A::A but got Float"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Float", ok("")),
            (
                "A::A(0)",
                err("Expecting Float in constructor for A::A but got Int"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A String", ok("")),
            (
                "A::A(0)",
                err("Expecting String in constructor for A::A but got Int"),
            ),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("type A -> A Int String", ok("")),
            (
                "A::A(0 0)",
                err("Expecting String in constructor for A::A but got Int"),
            ),
        ]));
    }

    #[test]
    fn test_primitive_recursive_type() {
        run_test("type T -> VarA | VarB T", ok(""));
        //todo: this kind of recursion shouldn't be allowed because it's infinite and the type can't actually be constructed
        run_test("type T -> VarA T", ok(""));
        run_test("type T -> VarA T | VarB", ok(""));
    }

    #[test]
    fn test_composite_type_errors() {
        let mut repl = REPL::new();

        assert_eq!(repl.handle_input("type Type['T] -> A | B"), ok(""));
        assert_eq!(repl.handle_input("type Type['T] -> A | B 'T"), ok(""));
        assert_eq!(
            repl.handle_input("type Type['T] -> A | B 'P"),
            Err("Undefined type ''P' for Type::B".to_string())
        );
    }

    #[test]
    fn test_function_types() {
        run_tests(&InsertionOrderHashMap::from([
            ("fn test -> 2.3", ok("")),
            ("test()", ok("2.3")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("fn test x:Int -> x", ok("")),
            ("test(2)", ok("2")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("fn + a:Int b:Int -> 0", ok("")),
            ("fn test x:Int y:Int -> +(x y)", ok("")),
            ("test(2 3)", ok("5")),
        ]));
    }

    #[test]
    fn test_function_type_errors() {
        run_test(
            "fn test x:Int -> List::A(x)",
            err("Undefined type 'List' in function 'test'"),
        );
        run_tests(&InsertionOrderHashMap::from([
            ("fn test x:Int -> x", ok("")),
            ("test()", err("Function 'test' in caller 'REPL' expects 1 arguments but 0 were provided")),
            ("test(1 2)", err("Function 'test' in caller 'REPL' expects 1 arguments but 2 were provided")),
            ("test(1.0)", err("Argument 0 in function 'test' in caller 'REPL' has type Int (bound to Int) but Float (bound to Float) was provided")),
        ]));
        run_test(
            "fn test x:Int y:Int -> List::A(x)",
            err("Undefined type 'List' in function 'test'"),
        );
    }

    #[test]
    fn test_parametrised_function_types() {
        run_tests(&InsertionOrderHashMap::from([
            ("fn test x:'T -> x", ok("")),
            ("test(2)", ok("2")),
            ("test(2.2)", ok("2.2")),
        ]));
        run_tests(&InsertionOrderHashMap::from([
            ("fn test x:'T y:'P -> x", ok("")),
            ("test(2 \"\")", ok("2")),
            ("test(2.2 3.0)", ok("2.2")),
        ]));
    }

    #[test]
    fn test_parametrised_function_type_errors() {
        run_tests(&InsertionOrderHashMap::from([
            ("fn + a:Int b:Int -> 0", ok("")),
            ("fn test x:'T y:'T -> +(x y)", ok("")),
            ("test(2 1)", ok("3")),
            ("test(2.2 1)", err("Argument 1 in function 'test' in caller 'REPL' has type Float (bound to 'T) but Int (bound to Int) was provided")),
        ]));
        //todo: test fn test x:'T y:'S -> +(x y) because this crashes when calling test(1 1.1) (shouldn't allow 'T to bind 'S, they should always be considered different)
    }

    #[test]
    fn test_lambdas() {
        run_tests(&InsertionOrderHashMap::from([
            ("fn + a:Int b:Int -> 0", ok("")),
            ("fn * a:Int b:Int -> 0", ok("")),
            (
                "fn compose f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))",
                ok(""),
            ),
            ("fn f2 x:Int -> *(x 3)", ok("")),
            ("fn g2 x:Int -> +(x 2)", ok("")),
            ("compose(f2 g2)", ok("Function (x:'P) -> f(g(x))")),
            ("with h = compose(f2 g2) h(7)", ok("27")),
        ]));

        run_tests(&InsertionOrderHashMap::from([
            (
                "fn compose f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))",
                ok(""),
            ),
            ("fn f2 x:Float -> \"ok\"", ok("")),
            ("fn g2 x:Int -> 2.2", ok("")),
            ("compose(f2 g2)", ok("Function (x:'P) -> f(g(x))")),
            ("with h = compose(f2 g2) h(7)", ok("\"ok\"")),
        ]));
    }

    #[test]
    fn test_lambda_errors() {
        run_tests(&InsertionOrderHashMap::from([
            ("fn + a:Int b:Int -> 0", ok("")),
            ("fn * a:Int b:Int -> 0", ok("")),
            (
                "fn compose f:('P)->'Q g:('Q)->'R -> (x:'P) -> g(f(x))",
                ok(""),
            ),
            //todo: when calling f, g there are shadowing problems!
            ("fn f2 x:Int -> *(x 3)", ok("")),
            ("fn g2 x:Int -> +(x 2)", ok("")),
            ("compose(f2 g2)", ok("Function (x:'P) -> g(f(x))")),
            ("with h = compose(f2 g2) h(7)", ok("23")),
            ("with h = compose(g2 f2) h(7)", ok("27")),
            ("fn f3 x:Float -> \"ok\"", ok("")),
            ("fn g3 x:Int -> 2.2", ok("")),
            ("compose(f3 g3)", err("Argument 1 in function 'compose' in caller 'REPL' has type ('Q) -> 'R (bound to ('Q) -> 'R) but (Int) -> Float (bound to (Int) -> Float) was provided")),
            ("compose(g3 f3)", ok("Function (x:'P) -> g(f(x))")),
            ("with h = compose(g3 f3) h(2)", ok("\"ok\""))
        ]));

        run_tests(&InsertionOrderHashMap::from([
            //todo: should error out
            (
                "fn compose f:('P)->'Q g:('Q)->'R -> (x:'P) -> f(g(x))",
                ok(""),
            ),
            ("fn f3 x:Float -> \"ok\"", ok("")),
            ("fn g3 x:Int -> 2.2", ok("")),
            ("compose(f3 g3)", err("Argument 1 in function 'compose' in caller 'REPL' has type ('Q) -> 'R (bound to ('Q) -> 'R) but (Int) -> Float (bound to (Int) -> Float) was provided")),
            //todo: should error out
            ("compose(g3 f3)", ok("Function (x:'P) -> f(g(x))")),
        ]));

        run_tests(&InsertionOrderHashMap::from([
            (
                "fn compose f:('Q)->'R g:('P)->'Q -> (x:'P)->f(g(x))",
                ok(""),
            ),
            ("fn f2 x:Int -> 2.2", ok("")),
            ("fn g2 x:Float -> \"ok\"", ok("")),
            ("compose(f2 g2)", err("Argument 1 in function 'compose' in caller 'REPL' has type ('P) -> 'Q (bound to ('P) -> 'Q) but (Float) -> String (bound to (Float) -> String) was provided")),
            ("compose(g2 f2)", ok("Function (x:'P) -> f(g(x))")),
            ("compose(f2 f2)", err("Argument 1 in function 'compose' in caller 'REPL' has type ('P) -> 'Q (bound to ('P) -> 'Q) but (Int) -> Float (bound to (Int) -> Float) was provided")),
            ("compose(g2 g2)", err("Argument 1 in function 'compose' in caller 'REPL' has type ('P) -> 'Q (bound to ('P) -> 'Q) but (Float) -> String (bound to (Float) -> String) was provided")),
        ]));
    }

    #[test]
    fn test_inner_bindings_do_not_shadow_self() {
        let mut repl = REPL::new();

        assert_eq!(
            repl.handle_input(
                "fn weird_f x:Int = 0 -> 0 _ -> with weird_f = (x:Float s:String)->123 weird_f(0)"
            ),
            ok("")
        );
        assert_eq!(repl.handle_input("weird_f(0)"), Ok("0".to_string()));
        assert_eq!(repl.handle_input("weird_f(1)"), Ok("0".to_string()));
        assert_eq!(repl.handle_input("weird_f(2)"), Ok("0".to_string()));
    }

    #[test]
    fn test_inner_bindings_shadow_outer_bindings() {
        let mut repl = REPL::new();

        assert_eq!(repl.handle_input("fn h -> 0"), ok(""));
        assert_eq!(
            repl.handle_input("with h = (x:Int)->2 h(1)"),
            Ok("2".to_string())
        );
        assert_eq!(
            repl.handle_input("with h = (x:Int)->2 h()"),
            Err(
                "Function 'h' in caller 'REPL' expects 1 arguments but 0 were provided".to_string()
            )
        );
        assert_eq!(
            repl.handle_input("with h = (x:Int y:Int)->x h(3)"),
            Err(
                "Function 'h' in caller 'REPL' expects 2 arguments but 1 were provided".to_string()
            )
        );
        assert_eq!(
            repl.handle_input("with h = (x:Int y:Int)->x h(3, 4)"),
            Ok("3".to_string())
        );
    }

    fn run_test(input: &str, expected: Result<String, String>) {
        run_tests(&InsertionOrderHashMap::from([(input, expected)]));
    }

    fn run_tests(tests: &InsertionOrderHashMap<&str, Result<String, String>>) {
        let mut repl: REPL = REPL::new();

        for input in tests.keys() {
            assert_eq!(
                repl.handle_input(input),
                tests.get(input.as_ref()).unwrap().to_owned()
            );
        }
    }

    fn ok(s: &str) -> Result<String, String> {
        Ok(s.to_string())
    }

    fn err(s: &str) -> Result<String, String> {
        Err(s.to_string())
    }
}
