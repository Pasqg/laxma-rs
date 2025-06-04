use std::rc::Rc;

use nohash_hasher::IntMap;

use super::{
    identifier_map::{
        IdentifierId, IdentifierIdMap
    },
    internal_repr::{Expression, FunctionDefinition, Program},
};

pub fn optimise_function(
    program: &Program,
    definition: &Rc<FunctionDefinition>,
) -> Rc<FunctionDefinition> {
    Rc::new(FunctionDefinition {
        id: definition.id,
        arguments: definition.arguments.clone(),
        bodies: definition
            .bodies
            .iter()
            .map(|(pattern, expr)| {
                (
                    pattern.clone(),
                    optimise_expression(program, definition.id, &IntMap::default(), expr),
                )
            })
            .collect(),
    })
}

fn optimise_expression(
    program: &Program,
    caller_id: IdentifierId,
    caller_bindings: &IntMap<IdentifierId, Rc<Expression>>,
    expr: &Rc<Expression>,
) -> Rc<Expression> {
    let expr = expression_inline_optimiser(program, caller_id, caller_bindings, &expr);
    expr
}

/*
 * Returns Some if an optimised expression can be created, otherwise None
 */
fn expression_inline_optimiser(
    program: &Program,
    caller_id: IdentifierId,
    caller_bindings: &IntMap<IdentifierId, Rc<Expression>>,
    expr: &Rc<Expression>,
) -> Rc<Expression> {
    match expr.as_ref() {
        Expression::FunctionCall(function_call) => {
            // Skip builtins
            if IdentifierIdMap::new()
                .get_identifier(&function_call.id)
                .is_some()
            {
                return Rc::clone(expr);
            }

            // Skip recursive calls
            if function_call.id == caller_id {
                return Rc::clone(expr);
            }

            let function_definition = program.functions.get(&function_call.id);
            if function_definition.is_none() {
                return Rc::clone(expr);
            }
            let function_definition = function_definition.unwrap();

            if function_definition.bodies.len() != 1
                || !function_definition.bodies[0].0.components.is_empty()
            {
                return Rc::clone(expr);
            }

            if function_call.arguments.is_empty() {
                expression_inline_optimiser(
                    program,
                    function_call.id,
                    caller_bindings,
                    &function_definition.bodies[0].1,
                )
            } else {
                if function_call.arguments.len() != function_definition.arguments.len() {
                    return Rc::clone(expr);
                }

                let mut bindings = caller_bindings.clone();
                for i in 0..function_call.arguments.len() {
                    bindings.insert(
                        function_definition.arguments[i].identifier,
                        Rc::clone(&function_call.arguments[i]),
                    );
                }

                expression_inline_optimiser(
                    program,
                    function_call.id,
                    &bindings,
                    &function_definition.bodies[0].1,
                )
            }
        }
        /*
         * With-block bindings should only be substituted in the final expression if there is only one occurrence
         * Naive inlining might result in worse performance by discarding cached computation
         */
        Expression::WithBlock(items, expression) => Rc::new(Expression::WithBlock(
            items
                .iter()
                .map(|(id, expr)| {
                    (
                        *id,
                        optimise_expression(program, caller_id, caller_bindings, expr),
                    )
                })
                .collect(),
            Rc::clone(expression),
        )),
        Expression::Integer(_) | Expression::Float(_) | Expression::String(_) => Rc::clone(expr),
        Expression::Identifier(id) => {
            if let Some(binding) = caller_bindings.get(id) {
                Rc::clone(binding)
            } else {
                Rc::clone(expr)
            }
        }
        Expression::TypeConstructor(type_id, variant_id, constructor_id, expressions) => {
            Rc::new(Expression::TypeConstructor(
                *type_id,
                *variant_id,
                *constructor_id,
                expressions
                    .iter()
                    .map(|expr| optimise_expression(program, caller_id, caller_bindings, expr))
                    .collect(),
            ))
        }
        Expression::Cast(expression, t) => Rc::new(Expression::Cast(
            optimise_expression(program, caller_id, caller_bindings, expression),
            Rc::clone(t),
        )),
        Expression::If(expression, expression1, expression2) => Rc::new(Expression::If(
            optimise_expression(program, caller_id, caller_bindings, expression),
            optimise_expression(program, caller_id, caller_bindings, expression1),
            optimise_expression(program, caller_id, caller_bindings, expression2),
        )),
        Expression::LambdaExpression(function_definition) => Rc::new(Expression::LambdaExpression(
            optimise_function(program, function_definition),
        )),
    }
}