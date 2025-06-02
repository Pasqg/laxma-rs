use crate::parser::combinators::{
    aborting, and_match, at_least_n, at_least_one, exclude, many, optional, or_match,
    or_match_flat, parser_ref, slit, Combinators, MatchRegex,
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
    CastExpression,
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

const IDENTIFIER_REGEX: &'static str = r"[a-zA-Z?_\+\-\*\/><=\^\.][a-zA-Z?_\+\-\*\/0-9\^=\.]*";
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
        Combinators::MatchRegex(MatchRegex::new(Some(Rules::Identifier), IDENTIFIER_REGEX)),
        or_match_flat(vec![
            slit("fn"),
            slit("type"),
            slit("with"),
            slit("if"),
            slit("cast"),
            slit("as"),
        ]),
    )
}

fn string() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::String), STRING_REGEX))
}

fn integer() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::Integer), INTEGER_REGEX))
}

fn float() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(Some(Rules::Float), FLOAT_REGEX))
}

fn basic_type_name() -> Combinators<Rules> {
    or_match_flat(vec![type_parameter(), identifier()])
}

fn type_parameter() -> Combinators<Rules> {
    Combinators::MatchRegex(MatchRegex::new(
        Some(Rules::TypeParameter),
        TYPE_PARAMETER_REGEX,
    ))
}

fn destructuring() -> Combinators<Rules> {
    at_least_n(Some(Rules::Destructuring), identifier(), None, 2)
}

fn type_name() -> Combinators<Rules> {
    let type_name = parser_ref();
    let composite_type = || {
        and_match(
            Rules::CompositeType,
            vec![
                identifier(),
                slit("["),
                aborting(
                    at_least_one(
                        Some(Rules::Elements),
                        type_name.clone(),
                        Some(optional(slit(","))),
                    ),
                    format!("Expected a type after '[' at {{line}}:{{col}}"),
                ),
                aborting(
                    slit("]"),
                    format!("Expected a closing ']' in type name at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let function_type = || {
        and_match(
            Rules::FunctionType,
            vec![
                slit("("),
                many(
                    Some(Rules::Arguments),
                    type_name.clone(),
                    Some(optional(slit(","))),
                ),
                aborting(
                    slit(")"),
                    format!("Expected a closing ')' in function type at {{line}}:{{col}}"),
                ),
                aborting(
                    slit("->"),
                    format!("Expected '->' in function type at {{line}}:{{col}}"),
                ),
                aborting(
                    type_name.clone(),
                    format!("Expected return type in function type at {{line}}:{{col}}"),
                ),
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
    and_match(
        Rules::Argument,
        vec![
            identifier(),
            slit(":"),
            aborting(
                type_name(),
                format!("Expected type after ':' at {{line}}:{{col}}"),
            ),
        ],
    )
}

fn function_signature() -> Combinators<Rules> {
    and_match(
        Rules::FunctionSignature,
        vec![
            slit("fn"),
            aborting(
                identifier(),
                format!("Expected identifier after 'fn' at {{line}}:{{col}}"),
            ),
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
                aborting(
                    slit(")"),
                    format!("Expected ')' in function call at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let type_constructor = || {
        and_match(
            Rules::TypeConstructor,
            vec![
                identifier(),
                slit("::"),
                aborting(
                    identifier(),
                    format!("Expected identifier after '::' at {{line}}:{{col}}"),
                ),
                aborting(
                    slit("("),
                    format!("Expected '(' in type constructor call at {{line}}:{{col}}"),
                ),
                many(
                    Some(Rules::Elements),
                    expression.clone(),
                    Some(optional(slit(","))),
                ),
                aborting(
                    slit(")"),
                    format!("Expected ')' in type constructor call at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let with_expression = || {
        and_match(
            Rules::WithBlock,
            vec![
                slit("with"),
                aborting(
                    at_least_one(
                        Some(Rules::Elements),
                        and_match(
                            Rules::Element,
                            vec![identifier(), slit("="), expression.clone()],
                        ),
                        None,
                    ),
                    format!("Expecting at least one binding in with block at {{line}}:{{col}}"),
                ),
                aborting(
                    expression.clone(),
                    format!("Expecting expression in with block at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let cast_expression = || {
        and_match(
            Rules::CastExpression,
            vec![
                slit("cast"),
                aborting(
                    expression.clone(),
                    format!("Expected expression after 'cast' at {{line}}:{{col}}"),
                ),
                aborting(
                    slit("as"),
                    format!("Expected 'as <Type>' in cast expression at {{line}}:{{col}}"),
                ),
                aborting(
                    type_name(),
                    format!("Expected Type in cast expression at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let if_expression = || {
        and_match(
            Rules::IfExpression,
            vec![
                slit("if"),
                aborting(
                    expression.clone(),
                    format!("Expected condition after 'if' at {{line}}:{{col}}"),
                ),
                aborting(
                    expression.clone(),
                    format!(
                        "Expected true and false branches after 'if condition' at {{line}}:{{col}}"
                    ),
                ),
                aborting(
                    expression.clone(),
                    format!(
                        "Expected false branch of 'if condition true-branch' at {{line}}:{{col}}"
                    ),
                ),
            ],
        )
    };

    let lambda_expression = || {
        and_match(
            Rules::LambdaExpression,
            vec![
                slit("("),
                many(
                    Some(Rules::Arguments),
                    argument(),
                    Some(optional(slit(","))),
                ),
                aborting(
                    slit(")"),
                    format!("Expected closing ')' in lambda expression at {{line}}:{{col}}"),
                ),
                aborting(
                    slit("->"),
                    format!("Expected '->' in lambda expression at {{line}}:{{col}}"),
                ),
                aborting(
                    expression.clone(),
                    format!("Expected body in lambda expression at {{line}}:{{col}}"),
                ),
            ],
        )
    };

    let expression_body = or_match(
        Rules::Expression,
        vec![
            lambda_expression(),
            with_expression(),
            if_expression(),
            cast_expression(),
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
    and_match(
        Rules::FunctionBody,
        vec![
            slit("->"),
            aborting(
                expression_parser(),
                format!("Expecting function body after '->' at {{line}}:{{col}}"),
            ),
        ],
    )
}

fn function_pattern_matching() -> Combinators<Rules> {
    and_match(
        Rules::PatternMatching,
        vec![
            slit("="),
            aborting(
                at_least_one(
                    None,
                    and_match(
                        Rules::Pattern,
                        vec![
                            at_least_one(
                                None,
                                or_match_flat(vec![
                                    destructuring(),
                                    float(),
                                    integer(),
                                    identifier(),
                                    string(),
                                ]),
                                Some(slit(",")),
                            ),
                            aborting(
                                function_body(),
                                format!(
                                    "Expecting function body after pattern at {{line}}:{{col}}"
                                ),
                            ),
                        ],
                    ),
                    None,
                ),
                format!(
                    "Expecting at least one pattern after function signature at {{line}}:{{col}}"
                ),
            ),
        ],
    )
}

fn function_def() -> Combinators<Rules> {
    and_match(
        Rules::FunctionDef,
        vec![
            function_signature(),
            aborting(
                or_match_flat(vec![function_pattern_matching(), function_body()]),
                format!("Expecting function body after function signature at {{line}}:{{col}}"),
            ),
        ],
    )
}

fn type_def() -> Combinators<Rules> {
    and_match(
        Rules::TypeDef,
        vec![
            slit("type"),
            aborting(
                type_name(),
                format!("Expected a type after 'type' at {{line}}:{{col}}"),
            ),
            aborting(
                slit("->"),
                format!("Expected '->' after type name in type definition at {{line}}:{{col}}"),
            ),
            aborting(
                at_least_one(
                    None,
                    and_match(
                        Rules::TypeDef,
                        vec![identifier(), many(Some(Rules::Elements), type_name(), None)],
                    ),
                    Some(slit("|")),
                ),
                format!("Expected at least one variant in type definition at {{line}}:{{col}}"),
            ),
        ],
    )
}

pub fn program_parser() -> Combinators<Rules> {
    many(
        Some(Rules::Program),
        aborting(or_match_flat(vec![function_def(), type_def()]),
        format!("Expected only function or type definitions but got syntax error at {{line}}:{{col}}")),
        None,
    )
}
