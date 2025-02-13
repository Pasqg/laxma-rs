use crate::parser::combinators::{
    and_match, at_least_n, at_least_one, exclude, many, optional, or_match, or_match_flat,
    parser_ref, slit, Combinators, MatchRegex,
};

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum Rules {
    String,
    Number,
    Identifier,
    TypeName,
    TypeParameter,
    ParametrizedType,
    Argument,
    Destructuring,

    TypeConstructor,
    FunctionCall,
    Arguments,
    Expression,
    WithBlock,
    IfExpression,

    TypeDef,
    FunctionSignature,
    FunctionBody,

    FunctionDef,
    Pattern,
    PatternMatching,

    Element,
    Elements,

    Program,
}

fn identifier() -> Combinators<Rules> {
    exclude(
        Combinators::MatchRegex(MatchRegex::new(
            Some(Rules::Identifier),
            r"[a-zA-Z?_\+\-\*\/:][a-zA-Z?_\+\-\*\/:0-9]*",
        )),
        or_match_flat(vec![slit("fn"), slit("type")]),
    )
}

fn number() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::Number), r"[0-9]+"))
}

fn basic_type_name() -> Combinators<Rules> {
    or_match_flat(vec![type_parameter(), identifier()])
}

fn type_parameter() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::TypeParameter), r"'[a-zA-Z]+"))
}

fn destructuring() -> Combinators<Rules> {
    at_least_n(
        Some(Rules::Destructuring),
        identifier(),
        Some(optional(slit("."))),
        2,
    )
}

fn type_name() -> Combinators<Rules> {
    let type_name = parser_ref();
    let parametrized_type = || {
        and_match(
            Rules::ParametrizedType,
            vec![
                identifier(),
                slit("["),
                at_least_one(Some(Rules::Elements), type_name.clone(), Some(slit(","))),
                slit("]"),
            ],
        )
    };
    type_name.bind(or_match_flat(vec![parametrized_type(), basic_type_name()]));
    type_name
}

fn argument() -> Combinators<Rules> {
    and_match(Rules::Argument, vec![identifier(), slit(":"), type_name()])
}

fn function_signature() -> Combinators<Rules> {
    and_match(
        Rules::FunctionSignature,
        vec![
            slit("fn"),
            identifier(),
            many(Some(Rules::Arguments), argument(), None),
        ],
    )
}

pub fn expression_parser() -> Combinators<Rules> {
    let expression = parser_ref();
    let function_call = || {
        and_match(
            Rules::FunctionCall,
            vec![
                identifier(),
                slit("("),
                many(
                    Some(Rules::Arguments),
                    expression.clone(),
                    Some(optional(slit(","))),
                ),
                slit(")"),
            ],
        )
    };

    let type_constructor = || {
        and_match(
            Rules::TypeConstructor,
            vec![
                identifier(),
                slit("::"),
                identifier(),
                slit("("),
                many(
                    Some(Rules::Elements),
                    expression.clone(),
                    Some(optional(slit(","))),
                ),
                slit(")"),
            ],
        )
    };

    let with_expression = || {
        and_match(
            Rules::WithBlock,
            vec![
                slit("with"),
                many(
                    Some(Rules::Elements),
                    and_match(
                        Rules::Element,
                        vec![identifier(), slit("="), expression.clone()],
                    ),
                    None,
                ),
                expression.clone(),
            ],
        )
    };

    let if_expression = || {
        and_match(
            Rules::IfExpression,
            vec![
                slit("if"),
                expression.clone(),
                expression.clone(),
                expression.clone(),
            ],
        )
    };

    let expression_body = or_match(
        Rules::Expression,
        vec![
            with_expression(),
            if_expression(),
            function_call(),
            type_constructor(),
            identifier(),
            number(),
        ],
    );
    expression.bind(expression_body);
    expression
}

fn function_body() -> Combinators<Rules> {
    and_match(Rules::FunctionBody, vec![slit("->"), expression_parser()])
}

fn function_pattern_matching() -> Combinators<Rules> {
    and_match(
        Rules::PatternMatching,
        vec![
            slit("="),
            at_least_one(
                None,
                and_match(
                    Rules::Pattern,
                    vec![
                        at_least_one(
                            None,
                            or_match_flat(vec![destructuring(), identifier(), number()]),
                            Some(slit(",")),
                        ),
                        function_body(),
                    ],
                ),
                None,
            ),
        ],
    )
}

fn function_def() -> Combinators<Rules> {
    and_match(
        Rules::FunctionDef,
        vec![
            function_signature(),
            or_match_flat(vec![function_pattern_matching(), function_body()]),
        ],
    )
}

fn type_def() -> Combinators<Rules> {
    and_match(
        Rules::TypeDef,
        vec![
            slit("type"),
            type_name(),
            slit("->"),
            at_least_one(
                None,
                and_match(
                    Rules::TypeDef,
                    vec![identifier(), many(Some(Rules::Elements), type_name(), None)],
                ),
                Some(slit("|")),
            ),
        ],
    )
}

pub fn program_parser() -> Combinators<Rules> {
    many(
        Some(Rules::Program),
        or_match_flat(vec![function_def(), type_def()]),
        None,
    )
}
