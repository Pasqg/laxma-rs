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

    TypeDef,
    FunctionSignature,
    FunctionBody,

    FunctionDef,
    Pattern,
    PatternMatching,

    Elements,

    Program,
}

pub fn parser() -> Combinators<Rules> {
    let identifier = || {
        exclude(
            Combinators::MatchRegex(MatchRegex::new(
                Some(Rules::Identifier),
                r"[a-zA-Z?_\+\-\*\/:][a-zA-Z?_\+\-\*\/:0-9]*",
            )),
            or_match_flat(vec![slit("fn"), slit("type")]),
        )
    };
    let number = || Combinators::MatchRegex(MatchRegex::new(Some(Rules::Number), r"[0-9]+"));
    let type_parameter = || Combinators::MatchRegex(MatchRegex::new(
        Some(Rules::TypeParameter),
        r"'[a-zA-Z]+",
    ));

    let destructuring: Combinators<Rules> = at_least_n(
        Some(Rules::Destructuring),
        identifier(),
        Some(optional(slit("."))),
        2,
    );

    let basic_type_name = || or_match_flat(vec![type_parameter(), identifier()]);

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

    let argument: Combinators<Rules> = and_match(
        Rules::Argument,
        vec![
            identifier(),
            slit(":"),
            type_name.clone(),
        ],
    );

    let expression = parser_ref();
    let function_call = || and_match(
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
    );

    let type_constructor = || and_match(
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
    );

    let expression_body= or_match(
        Rules::Expression,
        vec![function_call(), type_constructor(), identifier(), number()],
    );
    expression.bind(expression_body);

    let function_signature = || and_match(
        Rules::FunctionSignature,
        vec![
            slit("fn"),
            identifier(),
            many(Some(Rules::Arguments), argument.clone(), None),
        ],
    );
    let function_body = || and_match(Rules::FunctionBody, vec![slit("->"), expression.clone()]);

    let function_pattern_matching = || and_match(
        Rules::PatternMatching,
        vec![
            slit("="),
            at_least_one(
                None,
                and_match(
                    Rules::Pattern,
                    vec![
                        at_least_one(None, or_match_flat(vec![destructuring.clone(), identifier(), number()]), Some(slit(","))),
                        function_body(),
                    ],
                ),
                None,
            ),
        ],
    );

    let function_def: Combinators<Rules> = and_match(
        Rules::FunctionDef,
        vec![
            function_signature(),
            or_match_flat(vec![function_pattern_matching(), function_body()]),
        ],
    );

    let type_def = and_match(
        Rules::TypeDef,
        vec![
            slit("type"),
            type_name.clone(),
            slit("->"),
            at_least_one(
                None,
                and_match(
                    Rules::TypeDef,
                    vec![
                        identifier(),
                        many(Some(Rules::Elements), type_name.clone(), None),
                    ],
                ),
                Some(slit("|")),
            ),
        ],
    );

    let program: Combinators<Rules> = many(
        Some(Rules::Program),
        or_match_flat(vec![function_def.clone(), type_def.clone()]),
        None,
    );

    return program;
}
