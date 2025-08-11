use std::{collections::HashSet, rc::Rc};

use nohash_hasher::IntMap;

use super::{
    identifier_map::{
        IdentifierId, IdentifierIdMap, EXP_ID, FADD_ID, FDIV_ID, FMUL_ID, FSUB_ID, IADD_ID,
        IDIV_ID, IMUL_ID, ISUB_ID, LOG_ID, POW_ID,
    },
    internal_repr::{Expression, FunctionCall, FunctionDefinition, Program},
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
                    optimise_expression(program, &HashSet::from([definition.id]), &IntMap::default(), expr),
                )
            })
            .collect(),
    })
}

fn optimise_expression(
    program: &Program,
    caller_ids: &HashSet<IdentifierId>,
    caller_bindings: &IntMap<IdentifierId, Rc<Expression>>,
    expr: &Rc<Expression>,
) -> Rc<Expression> {
    let expr = expression_inline_optimiser(program, caller_ids, caller_bindings, &expr);
    let expr = expression_const_expr_optimiser(&expr);
    expr
}

fn substitute_bindings(
    bindings: &IntMap<IdentifierId, Rc<Expression>>,
    expr: &Rc<Expression>,
) -> Rc<Expression> {
    match expr.as_ref() {
        Expression::TypeConstructor(type_id, variant_id, expressions) => {
            Rc::new(Expression::TypeConstructor(
                *type_id,
                *variant_id,
                expressions
                    .iter()
                    .map(|expr| substitute_bindings(bindings, expr))
                    .collect(),
            ))
        }
        Expression::FunctionCall(function_call) => {
            Rc::new(Expression::FunctionCall(FunctionCall {
                id: function_call.id,
                arguments: function_call
                    .arguments
                    .iter()
                    .map(|expr| substitute_bindings(bindings, expr))
                    .collect(),
            }))
        }
        Expression::Identifier(id) => bindings
            .get(id)
            .map_or_else(|| Rc::clone(expr), |expr| Rc::clone(expr)),
        Expression::WithBlock(items, expression) => Rc::new(Expression::WithBlock(
            items
                .iter()
                .map(|(binding_id, expr)| (*binding_id, substitute_bindings(bindings, expr)))
                .collect(),
            substitute_bindings(bindings, expression),
        )),
        Expression::Cast(expression, t) => Rc::new(Expression::Cast(
            substitute_bindings(bindings, expression),
            Rc::clone(t),
        )),
        Expression::If(condition, true_branch, false_branch) => Rc::new(Expression::If(
            substitute_bindings(bindings, condition),
            substitute_bindings(bindings, true_branch),
            substitute_bindings(bindings, false_branch),
        )),
        //todo: might need to replace expressions?
        Expression::LambdaExpression(function_definition) => Rc::clone(expr),
        _ => Rc::clone(expr),
    }
}

/*
 * Substitutes function calls with the contained expression
 */
fn expression_inline_optimiser(
    program: &Program,
    caller_ids: &HashSet<IdentifierId>,
    caller_bindings: &IntMap<IdentifierId, Rc<Expression>>,
    expr: &Rc<Expression>,
) -> Rc<Expression> {
    match expr.as_ref() {
        Expression::FunctionCall(function_call) => {
            // Skip recursive calls
            if caller_ids.contains(&function_call.id) {
                return Rc::clone(expr);
            }

            // Skip builtins
            if IdentifierIdMap::new()
                .get_identifier(&function_call.id)
                .is_some()
            {
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

            let mut caller_ids = caller_ids.clone();
            if function_call.arguments.is_empty() {
                caller_ids.insert(function_call.id);
                let optimised = expression_inline_optimiser(
                    program,
                    &caller_ids,
                    caller_bindings,
                    &function_definition.bodies[0].1,
                );

                substitute_bindings(&caller_bindings, &optimised)
            } else {
                if function_call.arguments.len() != function_definition.arguments.len() {
                    return Rc::clone(expr);
                }

                let mut bindings = caller_bindings.clone();
                for i in 0..function_call.arguments.len() {
                    bindings.insert(
                        function_definition.arguments[i].identifier,
                        optimise_expression(
                            program,
                            &caller_ids,
                            caller_bindings,
                            &function_call.arguments[i],
                        ),
                    );
                }
                
                caller_ids.insert(function_call.id);
                let optimised = expression_inline_optimiser(
                    program,
                    &caller_ids,
                    &bindings,
                    &function_definition.bodies[0].1,
                );

                substitute_bindings(&bindings, &optimised)
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
                        optimise_expression(program, caller_ids, caller_bindings, expr),
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
        Expression::TypeConstructor(type_id, variant_id, expressions) => {
            Rc::new(Expression::TypeConstructor(
                *type_id,
                *variant_id,
                expressions
                    .iter()
                    .map(|expr| optimise_expression(program, caller_ids, caller_bindings, expr))
                    .collect(),
            ))
        }
        Expression::Cast(expression, t) => Rc::new(Expression::Cast(
            optimise_expression(program, caller_ids, caller_bindings, expression),
            Rc::clone(t),
        )),
        Expression::If(expression, expression1, expression2) => Rc::new(Expression::If(
            optimise_expression(program, caller_ids, caller_bindings, expression),
            optimise_expression(program, caller_ids, caller_bindings, expression1),
            optimise_expression(program, caller_ids, caller_bindings, expression2),
        )),
        Expression::LambdaExpression(function_definition) => Rc::new(Expression::LambdaExpression(
            optimise_function(program, function_definition),
        )),
    }
}

// Computes compile time constant expressions and substitutes its value directly
fn expression_const_expr_optimiser(expr: &Rc<Expression>) -> Rc<Expression> {
    match expr.as_ref() {
        Expression::FunctionCall(function_call) => {
            let args = function_call
                .arguments
                .iter()
                .map(|expr| expression_const_expr_optimiser(expr))
                .collect::<Vec<Rc<Expression>>>();
            let result = match function_call.id {
                EXP_ID => float_expr(expr, &[&args[0]], |args| args[0].exp()),
                LOG_ID => float_expr(expr, &[&args[0]], |args| args[0].ln()),
                POW_ID => float_expr(expr, &[&args[0], &args[1]], |args| args[0].powf(args[1])),
                IADD_ID => int_expr(expr, &[&args[0], &args[1]], |args| args[0] + args[1]),
                IMUL_ID => int_expr(expr, &[&args[0], &args[1]], |args| args[0] * args[1]),
                ISUB_ID => int_expr(expr, &[&args[0], &args[1]], |args| args[0] - args[1]),
                IDIV_ID => int_expr(expr, &[&args[0], &args[1]], |args| args[0] / args[1]),
                FADD_ID => float_expr(expr, &[&args[0], &args[1]], |args| args[0] + args[1]),
                FMUL_ID => float_expr(expr, &[&args[0], &args[1]], |args| args[0] * args[1]),
                FSUB_ID => float_expr(expr, &[&args[0], &args[1]], |args| args[0] - args[1]),
                FDIV_ID => float_expr(expr, &[&args[0], &args[1]], |args| args[0] / args[1]),
                _ => Rc::clone(expr),
            };
            return result;
        }
        _ => Rc::clone(expr),
    }
}

fn float_expr<F>(default: &Rc<Expression>, expressions: &[&Rc<Expression>], f: F) -> Rc<Expression>
where
    F: FnOnce(&[f32]) -> f32,
{
    for expr in expressions {
        match expr.as_ref() {
            Expression::Float(_) => {}
            _ => return Rc::clone(default),
        }
    }

    let args = expressions
        .iter()
        .map(|expr| expr.as_float())
        .collect::<Vec<f32>>();
    Rc::new(Expression::Float(f(&args)))
}

fn int_expr<F>(default: &Rc<Expression>, expressions: &[&Rc<Expression>], f: F) -> Rc<Expression>
where
    F: FnOnce(&[i64]) -> i64,
{
    for expr in expressions {
        match expr.as_ref() {
            Expression::Integer(_) => {}
            _ => return Rc::clone(default),
        }
    }

    let args = expressions
        .iter()
        .map(|expr| expr.as_int())
        .collect::<Vec<i64>>();
    Rc::new(Expression::Integer(f(&args)))
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, rc::Rc};

    use nohash_hasher::IntMap;

    use crate::{
        compiler::{
            grammar,
            identifier_map::{IdentifierId, IdentifierIdMap, FMUL_ID, IADD_ID, INT_ID},
            internal_repr::{
                to_repr, Expression, FunctionArgument, FunctionCall, FunctionDefinition, Pattern,
                Program, Type,
            },
            lexer::Lexer,
            optimiser::{
                expression_const_expr_optimiser, expression_inline_optimiser, optimise_expression,
            }, repl::REPL,
        },
        parser::combinators::ParserCombinator,
        utils::InsertionOrderHashMap,
    };

    #[test]
    pub fn test_const_expr_optimise_iadd() {
        assert_eq!(
            expression_const_expr_optimiser(&Rc::new(Expression::FunctionCall(FunctionCall {
                id: IADD_ID,
                arguments: vec![
                    Rc::new(Expression::Integer(12)),
                    Rc::new(Expression::Integer(11)),
                ],
            }))),
            Rc::new(Expression::Integer(23)),
        );
    }

    #[test]
    pub fn test_const_expr_optimise_fmul() {
        assert_eq!(
            expression_const_expr_optimiser(&Rc::new(Expression::FunctionCall(FunctionCall {
                id: FMUL_ID,
                arguments: vec![
                    Rc::new(Expression::Float(12.0)),
                    Rc::new(Expression::Float(11.0)),
                ],
            }))),
            Rc::new(Expression::Float(132.0)),
        );
    }

    // func -> iadd(12 11)
    // func() becomes iadd(12 11)
    #[test]
    pub fn test_inline_optimiser_no_recursion_no_arguments() {
        let function_id = 987;
        let mut program = Program {
            functions: InsertionOrderHashMap::from([(
                function_id,
                Rc::new(FunctionDefinition {
                    id: function_id,
                    arguments: vec![],
                    bodies: vec![(
                        Pattern::empty(),
                        Rc::new(Expression::FunctionCall(FunctionCall {
                            id: IADD_ID,
                            arguments: vec![
                                Rc::new(Expression::Integer(12)),
                                Rc::new(Expression::Integer(11)),
                            ],
                        })),
                    )],
                }),
            )]),
            dispatches: InsertionOrderHashMap::new(),
            types: IntMap::default(),
            identifier_id_map: IdentifierIdMap::new(),
        };

        assert_eq!(
            expression_inline_optimiser(
                &program,
                &HashSet::from([123]),
                &IntMap::default(),
                &Rc::new(Expression::FunctionCall(FunctionCall {
                    id: function_id,
                    arguments: vec![],
                }))
            ),
            Rc::new(Expression::FunctionCall(FunctionCall {
                id: IADD_ID,
                arguments: vec![
                    Rc::new(Expression::Integer(12)),
                    Rc::new(Expression::Integer(11)),
                ],
            })),
        );
    }

    // func(x) -> iadd(x 11)
    // func(3) becomes iadd(3 11)
    #[test]
    pub fn test_inline_optimiser_no_recursion_one_arg() {
        let function_id = 987;
        let x_id = function_id + 1;
        let mut program = Program {
            functions: InsertionOrderHashMap::from([(
                function_id,
                Rc::new(FunctionDefinition {
                    id: function_id,
                    arguments: vec![FunctionArgument {
                        identifier: x_id,
                        typing: Rc::new(Type::PrimitiveType(INT_ID)),
                    }],
                    bodies: vec![(
                        Pattern::empty(),
                        Rc::new(Expression::FunctionCall(FunctionCall {
                            id: IADD_ID,
                            arguments: vec![
                                Rc::new(Expression::Identifier(x_id)),
                                Rc::new(Expression::Integer(11)),
                            ],
                        })),
                    )],
                }),
            )]),
            dispatches: InsertionOrderHashMap::new(),
            types: IntMap::default(),
            identifier_id_map: IdentifierIdMap::new(),
        };

        assert_eq!(
            expression_inline_optimiser(
                &program,
                &HashSet::from([123]),
                &IntMap::default(),
                &Rc::new(Expression::FunctionCall(FunctionCall {
                    id: function_id,
                    arguments: vec![Rc::new(Expression::Integer(3))],
                }))
            ),
            Rc::new(Expression::FunctionCall(FunctionCall {
                id: IADD_ID,
                arguments: vec![
                    Rc::new(Expression::Integer(3)),
                    Rc::new(Expression::Integer(11)),
                ],
            })),
        );
    }

    // func -> iadd(12 func())
    // func() becomes iadd(12 func())
    #[test]
    pub fn test_inline_optimiser_simple_recursion() {
        let function_id = 987;
        let mut program = Program {
            functions: InsertionOrderHashMap::from([(
                function_id,
                Rc::new(FunctionDefinition {
                    id: function_id,
                    arguments: vec![],
                    bodies: vec![(
                        Pattern::empty(),
                        Rc::new(Expression::FunctionCall(FunctionCall {
                            id: IADD_ID,
                            arguments: vec![
                                Rc::new(Expression::Integer(12)),
                                Rc::new(Expression::FunctionCall(FunctionCall {
                                    id: function_id,
                                    arguments: vec![],
                                })),
                            ],
                        })),
                    )],
                }),
            )]),
            dispatches: InsertionOrderHashMap::new(),
            types: IntMap::default(),
            identifier_id_map: IdentifierIdMap::new(),
        };

        assert_eq!(
            expression_inline_optimiser(
                &program,
                &HashSet::from([123]),
                &IntMap::default(),
                &Rc::new(Expression::FunctionCall(FunctionCall {
                    id: function_id,
                    arguments: vec![],
                }))
            ),
            Rc::new(Expression::FunctionCall(FunctionCall {
                id: IADD_ID,
                arguments: vec![
                    Rc::new(Expression::Integer(12)),
                    Rc::new(Expression::FunctionCall(FunctionCall {
                        id: function_id,
                        arguments: vec![],
                    })),
                ],
            })),
        );
    }

    #[test]
    pub fn test_inline_optimiser_recursion_in_another_call() {
        let mut program = setup(
            "
            type List['T] -> Empty | List 'T List['T]
            fn empty -> List::Empty()
            fn cons x:'T xs:List['T] -> List::List(x xs)
            fn singleton x:'T -> cons(x empty())
            fn concat xs:List['T] ys:List['T] =
                _, Empty -> xs
                Empty , _ -> ys
                List x xs , _ -> cons(x concat(xs ys))
            ",
        ).unwrap();
        let concat_id = get_id(&mut program, "concat");
        let singleton_id = get_id(&mut program, "singleton");
        let expression = Rc::new(Expression::FunctionCall(FunctionCall {
                    id: concat_id,
                    arguments: vec![
                        Rc::new(Expression::FunctionCall(FunctionCall {
                            id: singleton_id,
                            arguments: vec![Rc::new(Expression::Integer(5))]
                        })),
                        Rc::new(Expression::FunctionCall(FunctionCall {
                            id: singleton_id,
                            arguments: vec![Rc::new(Expression::Integer(6))]
                        })),
                    ],
                }),);

        assert_eq!(
            optimise_expression(
                &program,
                &HashSet::from([concat_id]),
                &IntMap::default(),
                &expression,
            ),
            Rc::new(Expression::FunctionCall(FunctionCall {
                id: concat_id,
                arguments: vec![
                    Rc::new(Expression::FunctionCall(FunctionCall {
                        id: singleton_id,
                        arguments: vec![Rc::new(Expression::Integer(5))]
                    })),
                    Rc::new(Expression::FunctionCall(FunctionCall {
                        id: singleton_id,
                        arguments: vec![Rc::new(Expression::Integer(6))]
                    })),
                ],
            }),)
        );
    }

    fn get_id(program: &mut Program, str: &str) -> IdentifierId {
        program.identifier_id_map.get_id(&Rc::new(format!("{}", str)))
    }

    fn setup(code: &str) -> Result<Program, String> {
        let mut id_map = IdentifierIdMap::new();
        let tokens = Lexer::token_stream(code);
        let program_result = grammar::program_parser().parse(&tokens);
        to_repr(&program_result.ast, &mut id_map)
    }
}
