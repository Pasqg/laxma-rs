use crate::parser::combinators::{
    and_match, at_least_n, at_least_one, exclude, many, optional, or_match, or_match_flat,
    parser_ref, regex, slit, Combinators, MatchRegex,
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

/*
# The type List is either a Empty or tuple of T and a List
type List['T] -> Empty | NonEmpty = 'T List['T]
type Result['T, 'E] -> Error = 'E | Ok = 'T

// { } parameter destructuring matching type?
// if argument doesn't conform (i.e. empty list) -> panic!
fun first {x, xs}:List['T] -> x
fun rest {x, xs}:List['T] -> xs

fun length x:List['T] =
    Empty -> 0
    {_, xs} -> +(1, length(xs))


fun sum x:int y:int -> +(x, y)

sumlist x:List[int] =
    empty? -> 0
     -> sum(first(x), sumlist(rest(x)))
*/

pub fn parser() -> Combinators<Rules> {
    let identifier = || {
        exclude(
            Combinators::MatchRegex(MatchRegex::new(
                Some(Rules::Identifier),
                r"[a-zA-Z?_\+\-\*\/∅]+",
            )),
            or_match_flat(vec![slit("fun"), slit("type")]),
        )
    };
    let number = || Combinators::MatchRegex(MatchRegex::new(Some(Rules::Number), r"[0-9]+"));
    let type_parameter: Combinators<Rules> = Combinators::MatchRegex(MatchRegex::new(
        Some(Rules::TypeParameter),
        r"'[a-zA-Z?_\+\-\*\/]+",
    ));

    let destructuring: Combinators<Rules> = at_least_n(
        Some(Rules::Destructuring),
        identifier(),
        Some(optional(slit(","))),
        2,
    );

    let basic_type_name = || or_match_flat(vec![type_parameter.clone(), identifier()]);

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
            or_match_flat(vec![/*destructuring.clone(), */ identifier()]),
            slit(":"),
            type_name.clone(),
        ],
    );

    let expression = parser_ref();
    let function_call: Combinators<Rules> = and_match(
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

    let expression_body: Combinators<Rules> = or_match(
        Rules::Expression,
        vec![function_call.clone(), identifier(), number()],
    );
    expression.bind(expression_body);

    let function_signature: Combinators<Rules> = and_match(
        Rules::FunctionSignature,
        vec![
            slit("fun"),
            identifier(),
            many(Some(Rules::Arguments), argument.clone(), None),
        ],
    );
    let function_body = || and_match(Rules::FunctionBody, vec![slit("->"), expression.clone()]);

    let function_pattern_matching: Combinators<Rules> = and_match(
        Rules::PatternMatching,
        vec![
            slit("="),
            at_least_one(
                None,
                and_match(
                    Rules::Pattern,
                    vec![
                        or_match_flat(vec![destructuring.clone(), identifier()]),
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
            function_signature.clone(),
            or_match_flat(vec![function_pattern_matching.clone(), function_body()]),
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
