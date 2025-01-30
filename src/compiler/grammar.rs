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

fn destructuring() -> Combinators<Rules> {
    at_least_n(
        Some(Rules::Destructuring),
        identifier(),
        Some(optional(slit("."))),
        2,
    )
}

fn basic_type_name() -> Combinators<Rules> {
    or_match_flat(vec![type_parameter(), identifier()])
}

fn type_parameter() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(
        Some(Rules::TypeParameter),
        r"'[a-zA-Z]+",
    ))
}

fn parametrized_type() -> Combinators<Rules> {
    and_match(
        Rules::ParametrizedType,
        vec![
            identifier(),
            slit("["),
            at_least_one(Some(Rules::Elements), type_name(), Some(slit(","))),
            slit("]"),
        ],
    )
}

fn type_name() -> Combinators<Rules> {
    or_match_flat(vec![parametrized_type(), basic_type_name()])
}

fn function_call() -> Combinators<Rules> {
    and_match(
        Rules::FunctionCall,
        vec![
            identifier(),
            slit("("),
            many(
                Some(Rules::Arguments),
                expression(),
                Some(optional(slit(","))),
            ),
            slit(")"),
        ],
    )
}

fn type_constructor() -> Combinators<Rules> {
    and_match(
        Rules::TypeConstructor,
        vec![
            identifier(),
            slit("::"),
            identifier(),
            slit("("),
            many(
                Some(Rules::Elements),
                expression(),
                Some(optional(slit(","))),
            ),
            slit(")"),
        ],
    )
}

fn expression() -> Combinators<Rules> {
    or_match(
        Rules::Expression,
        vec![function_call(), type_constructor(), identifier(), number()],
    )
}

fn argument() -> Combinators<Rules> {
    and_match(
        Rules::Argument,
        vec![
            identifier(),
            slit(":"),
            type_name(),
        ],
    )
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

fn function_body() -> Combinators<Rules> {
    and_match(Rules::FunctionBody, vec![slit("->"), expression()])
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
                        at_least_one(None, or_match_flat(vec![destructuring(), identifier(), number()]), Some(slit(","))),
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
                    vec![
                        identifier(),
                        many(Some(Rules::Elements), type_name(), None),
                    ],
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
