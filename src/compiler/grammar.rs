use crate::parser::combinators::{
    and_match, at_least_n, at_least_one, exclude, many, optional, or_match, or_match_flat,
    parser_ref, slit, Combinators, MatchRegex,
};

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
pub enum Rules {
    String,
    Integer,
    Float,
    Identifier,
    TypeParameter,
    CompositeType,
    FunctionType,
    Argument,
    Destructuring,

    TypeConstructor,
    FunctionCall,
    Arguments,
    Expression,
    WithBlock,
    IfExpression,
    LambdaExpression,

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

const IDENTIFIER_REGEX: &'static str = r"[a-zA-Z?_\+\-\*\/><=\^\.][a-zA-Z?_\+\-\*\/0-9\^=]*";
const INTEGER_REGEX: &'static str = r"[\-\+]?\d+";
const FLOAT_REGEX: &'static str = r"[\-\+]?\d+\.\d+";
const STRING_REGEX: &'static str = r#"\"[^\"]*\""#;
const TYPE_PARAMETER_REGEX: &'static str = r"'[a-zA-Z]+";
const STANDALONE_REGEX: &'static str = r"->|::|:|\||[,\[\]\(\)]";

// Order matters
pub(super) const LEXER_REGEX: [&'static str; 7] = [
    STANDALONE_REGEX,
    FLOAT_REGEX,
    INTEGER_REGEX,
    IDENTIFIER_REGEX,
    STRING_REGEX,
    TYPE_PARAMETER_REGEX,
    "^[\n\r\t ]",
];

fn identifier() -> Combinators<Rules> {
    exclude(
        Combinators::MatchRegex(MatchRegex::new(
            Some(Rules::Identifier),
            IDENTIFIER_REGEX,
        )),
        or_match_flat(vec![slit("fn"), slit("type"), slit("with"), slit("if")]),
    )
}

fn string() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::String), STRING_REGEX))
}

fn integer() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::Integer), INTEGER_REGEX))
}

fn float() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(
        Some(Rules::Float),
        FLOAT_REGEX,
    ))
}

fn basic_type_name() -> Combinators<Rules> {
    or_match_flat(vec![type_parameter(), identifier()])
}

fn type_parameter() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::TypeParameter), TYPE_PARAMETER_REGEX))
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
    let composite_type = || {
        and_match(
            Rules::CompositeType,
            vec![
                identifier(),
                slit("["),
                at_least_one(Some(Rules::Elements), type_name.clone(), Some(optional(slit(",")))),
                slit("]"),
            ],
        )
    };

    let function_type = || {
        and_match(
            Rules::FunctionType,
            vec![
                slit("("),
                many(Some(Rules::Arguments), type_name.clone(), Some(optional(slit(",")))),
                slit(")"),
                slit("->"),
                type_name.clone(),
            ],
        )
    };
    type_name.bind(or_match_flat(vec![
        function_type(),
        composite_type(),
        basic_type_name(),
    ]));
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
                at_least_one(
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

    let lambda_expression = || {
        and_match(
            Rules::LambdaExpression,
            vec![
                slit("("),
                many(Some(Rules::Arguments), argument(), Some(optional(slit(",")))),
                slit(")"),
                slit("->"),
                expression.clone(),
            ],
        )
    };

    let expression_body = or_match(
        Rules::Expression,
        vec![
            lambda_expression(),
            with_expression(),
            if_expression(),
            function_call(),
            type_constructor(),
            float(),
            integer(),
            identifier(),
            string(),
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
                            or_match_flat(vec![destructuring(), float(), integer(), identifier(), string()]),
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
