use core::panic;
use std::{
    any::{Any, TypeId},
    fmt::Debug,
};

use regex::Regex;

use super::{ast::AST, parser_result::ParserResult, token_stream::TokenStream};

pub trait ParserCombinator<RuleId, TokenType> {
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType>;
}

#[derive(Clone)]
pub enum Combinators<RuleId, TokenType> {
    MatchToken(MatchToken<RuleId, TokenType>),
    MatchRegex(MatchRegex<RuleId>),
    MatchNone(MatchNone),
    MatchAny(MatchAny<RuleId, TokenType>),
    AndMatch(AndMatch<RuleId, TokenType>),
    OrMatch(OrMatch<RuleId, TokenType>),
    AtLeastOne(AtLeastOne<RuleId, TokenType>),
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for Combinators<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + Any,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        match self {
            Combinators::MatchToken(parser) => parser.parse(tokens),
            Combinators::MatchNone(parser) => parser.parse(tokens),
            Combinators::MatchAny(parser) => parser.parse(tokens),
            Combinators::MatchRegex(parser) => parser.parse(tokens),
            Combinators::AndMatch(parser) => parser.parse(tokens),
            Combinators::OrMatch(parser) => parser.parse(tokens),
            Combinators::AtLeastOne(parser) => parser.parse(tokens),
        }
    }
}

#[derive(Clone, Copy)]
pub struct MatchToken<RuleId, TokenType> {
    id: Option<RuleId>,
    token: TokenType,
}

impl<RuleId, TokenType> MatchToken<RuleId, TokenType> {
    pub fn new(id: Option<RuleId>, token: TokenType) -> Self {
        Self { id, token }
    }
}

pub fn lit<RuleId, TokenType>(token: TokenType) -> Combinators<RuleId, TokenType> {
    Combinators::MatchToken(MatchToken::new(None, token))
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for MatchToken<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            if token == self.token {
                return ParserResult::succeeded(
                    AST::new(self.id, vec![token], Vec::new()),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

#[derive(Clone)]
pub struct MatchRegex<RuleId> {
    id: Option<RuleId>,
    regex: Regex,
}

impl<RuleId> MatchRegex<RuleId> {
    pub fn new(id: Option<RuleId>, pattern: &str) -> Self {
        Self {
            id,
            regex: Regex::new(pattern).unwrap(),
        }
    }
}

pub fn regex<RuleId>(pattern: &str) -> Combinators<RuleId, &str> {
    Combinators::MatchRegex(MatchRegex::new(None, pattern))
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for MatchRegex<RuleId>
where
    RuleId: Copy,
    TokenType: Copy + 'static,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        if TypeId::of::<TokenType>() != TypeId::of::<&str>() {
            panic!("")
        }
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            let token_str: &str = unsafe { std::mem::transmute_copy(&token) };
            let captures = self.regex.captures(token_str);
            if captures.is_some() && captures.unwrap()[0] == *token_str {
                return ParserResult::succeeded(
                    AST::new(self.id, vec![token], Vec::new()),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

#[derive(Clone)]
pub struct MatchNone;

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for MatchNone
where
    RuleId: Copy,
    TokenType: Copy,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        ParserResult::succeeded(AST::empty(), tokens.clone())
    }
}

pub fn match_none<RuleId, TokenType>() -> Combinators<RuleId, TokenType> {
    Combinators::MatchNone(MatchNone)
}

/*
    Matches anything, as long as the current and following tokens to not match the 'excluded' combinator (if provided).
*/
#[derive(Clone)]
pub struct MatchAny<RuleId, TokenType> {
    id: Option<RuleId>,
    excluded: Option<Box<Combinators<RuleId, TokenType>>>,
}

impl<RuleId, TokenType> MatchAny<RuleId, TokenType> {
    pub fn new(id: Option<RuleId>, excluded: Option<Combinators<RuleId, TokenType>>) -> Self {
        Self {
            id,
            excluded: excluded.map(|x| Box::new(x)),
        }
    }
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for MatchAny<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        if tokens.not_done() {
            if self.excluded.is_some() && self.excluded.as_ref().unwrap().parse(tokens).is_ok() {
                return ParserResult::failed(tokens.clone());
            }

            let (token, remaining) = tokens.advance();
            return ParserResult::succeeded(AST::new(self.id, vec![token], Vec::new()), remaining);
        }
        ParserResult::failed(tokens.clone())
    }
}

/*
    Returns a match if all the input rules match, otherwise it fails (and backtracks).
*/
#[derive(Clone)]
pub struct AndMatch<RuleId, TokenType> {
    id: Option<RuleId>,
    rules: Vec<Combinators<RuleId, TokenType>>,
}

impl<RuleId, TokenType> AndMatch<RuleId, TokenType> {
    pub fn new(id: Option<RuleId>, rules: Vec<Combinators<RuleId, TokenType>>) -> Self {
        Self { id, rules }
    }
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for AndMatch<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        let mut _remaining = tokens.clone();
        let mut matched = Vec::new();
        let mut children = Vec::new();
        for rule in &self.rules {
            let ParserResult {
                result,
                ast,
                remaining,
            } = rule.parse(&_remaining);
            if !result {
                return ParserResult::failed(tokens.clone());
            }
            _remaining = remaining.clone();
            matched.extend(&ast.matched);
            children.push(ast);
        }
        return ParserResult::succeeded(AST::new(self.id, matched, children), _remaining);
    }
}

/*
    Returns a match if any of the input rules match, otherwise it fails (and backtracks).
*/
#[derive(Clone)]
pub struct OrMatch<RuleId, TokenType> {
    id: Option<RuleId>,
    rules: Vec<Combinators<RuleId, TokenType>>,
}

impl<RuleId, TokenType> OrMatch<RuleId, TokenType> {
    pub fn new(id: Option<RuleId>, rules: Vec<Combinators<RuleId, TokenType>>) -> Self {
        Self { id, rules }
    }
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for OrMatch<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        for rule in &self.rules {
            let ParserResult {
                result,
                ast,
                remaining,
            } = rule.parse(tokens);
            if result {
                //todo: probably we don't care about one of children or parent's matched, so we can remove a clone here
                return ParserResult::succeeded(
                    AST::new(self.id, ast.matched.clone(), vec![ast]),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

pub fn optional<RuleId, TokenType>(
    id: Option<RuleId>,
    parser: Combinators<RuleId, TokenType>,
) -> Combinators<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + 'static,
{
    Combinators::OrMatch(OrMatch::new(
        id,
        vec![parser, Combinators::MatchNone(MatchNone)],
    ))
}

/*
Matches zero or more occurrences of the provided 'element' rule.
Optionally, matches a delimiter rule between each of the elements.
 */
pub fn many<RuleId, TokenType>(
    id: Option<RuleId>,
    element: Combinators<RuleId, TokenType>,
    delim: Option<Combinators<RuleId, TokenType>>,
) -> Combinators<RuleId, TokenType>
where
    RuleId: Copy + Debug + 'static,
    TokenType: Copy + Debug + Eq + 'static,
{
    Combinators::OrMatch(OrMatch::new(
        id,
        vec![
            Combinators::AtLeastOne(AtLeastOne::new(id, element, delim)),
            Combinators::MatchNone(MatchNone),
        ],
    ))
}

/*
   Matches at least one occurrence of the provided 'element' rule.
   Optionally, matches a delimiter rule between each of the elements.
*/
#[derive(Clone)]
pub struct AtLeastOne<RuleId, TokenType> {
    id: Option<RuleId>,
    element: Box<Combinators<RuleId, TokenType>>,
    delim: Option<Box<Combinators<RuleId, TokenType>>>,
}

impl<RuleId, TokenType> AtLeastOne<RuleId, TokenType> {
    pub fn new(
        id: Option<RuleId>,
        element: Combinators<RuleId, TokenType>,
        delim: Option<Combinators<RuleId, TokenType>>,
    ) -> Self {
        Self {
            id,
            element: Box::new(element),
            delim: delim.map(|x| Box::new(x)),
        }
    }
}

impl<RuleId, TokenType> ParserCombinator<RuleId, TokenType> for AtLeastOne<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream<TokenType>) -> ParserResult<RuleId, TokenType> {
        let result = self.element.parse(tokens);
        if !result.is_ok() {
            return ParserResult::failed(tokens.clone());
        }

        let mut previous_result = ParserResult::succeeded(
            AST::new(self.id, result.ast.matched.clone(), vec![result.ast]),
            result.remaining,
        );
        while previous_result.remaining.not_done() {
            let (delim_result, delim_remaining) = if self.delim.is_some() {
                let result = self
                    .delim
                    .as_ref()
                    .unwrap()
                    .parse(&previous_result.remaining);
                (result.result, result.remaining)
            } else {
                (true, previous_result.remaining.clone())
            };

            if !delim_result {
                return previous_result;
            }

            let element_result = self.element.parse(&delim_remaining);
            if element_result.result {
                if self.delim.is_none() {
                    previous_result = ParserResult::succeeded(
                        previous_result.ast.merge(element_result.ast),
                        element_result.remaining,
                    );
                } else {
                    let mut ast = previous_result.ast;
                    for child in element_result.ast.children {
                        ast = ast.merge(child);
                    }
                    previous_result = ParserResult::succeeded(ast, element_result.remaining);
                }
            } else {
                return previous_result;
            }
        }

        panic!("BUG: Shouldn't happen");
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::parser::{
        ast::AST,
        combinators::{
            lit, many, optional, regex, AndMatch, AtLeastOne, MatchAny, MatchNone, MatchRegex,
            MatchToken, OrMatch, ParserCombinator,
        },
        parser_result::ParserResult,
        token_stream::TokenStream,
    };

    #[test]
    fn test_match_token_success() {
        let tokens = TokenStream::new(Rc::new(vec!["a", "b"]));

        let rule = Some("test");
        let parser = MatchToken::new(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec!["a"], Vec::new()),
                TokenStream::with_offset(Rc::new(vec!["a", "b"]), 1)
            )
        );
    }

    #[test]
    fn test_match_token_failed() {
        let tokens = TokenStream::new(Rc::new(vec!["c", "b"]));

        let rule = Some("test");
        let parser = MatchToken::new(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_success() {
        let tokens = TokenStream::new(Rc::new(vec!["variable", "b"]));

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec!["variable"], Vec::new()),
                TokenStream::with_offset(Rc::new(vec!["variable", "b"]), 1)
            )
        );
    }

    #[test]
    fn test_regex_failure_partial() {
        let tokens = TokenStream::new(Rc::new(vec!["variableA", "b"]));

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_failure() {
        let tokens = TokenStream::new(Rc::new(vec!["variableA", "b"]));

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[0-9]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_match_none_empty_stream() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(Vec::new()));

        let result: ParserResult<&str, &str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_none() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let result: ParserResult<&str, &str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_any_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(lit("b")));

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec!["a"], Vec::new()),
                TokenStream::with_offset(Rc::new(vec!["a"]), 1)
            )
        );
    }

    #[test]
    fn test_match_any_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["b"]));

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(lit("b")));

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_and_match_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "b", "c"]));

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec!["a", "b"],
                    vec![
                        AST::new(None, vec!["a"], Vec::new()),
                        AST::new(None, vec!["b"], Vec::new())
                    ]
                ),
                TokenStream::with_offset(Rc::new(vec!["a", "b", "c"]), 2)
            )
        );
    }

    #[test]
    fn test_and_match_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "c"]));

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_or_match_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "b"]));

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec!["a"], vec![AST::new(None, vec!["a"], Vec::new())]),
                TokenStream::with_offset(Rc::new(vec!["a", "b"]), 1)
            )
        );
    }

    #[test]
    fn test_or_match_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["c"]));

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_optional_some() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let rule = Some("test");
        let parser = optional(rule, lit("a"));

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec!["a"], vec![AST::new(None, vec!["a"], Vec::new())]),
                TokenStream::with_offset(Rc::new(vec!["a"]), 1)
            )
        );
    }

    #[test]
    fn test_optional_none() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let rule = Some("test");
        let parser = optional(rule, lit("b"));

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(AST::new(rule, Vec::new(), vec![AST::empty()]), tokens)
        );
    }

    #[test]
    fn test_at_least_once_no_delim_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "a", "a", "b"]));

        let rule = Some("test");
        let parser = AtLeastOne::new(rule, lit("a"), None);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec!["a", "a", "a"],
                    vec![
                        AST::new(None, vec!["a"], Vec::new()),
                        AST::new(None, vec!["a"], Vec::new()),
                        AST::new(None, vec!["a"], Vec::new())
                    ]
                ),
                TokenStream::with_offset(Rc::new(vec!["a", "a", "a", "b"]), 3),
            )
        );
    }

    #[test]
    fn test_at_least_once_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "a", "a", "b"]));

        let rule = Some("test");
        let parser = AtLeastOne::new(rule, lit("b"), None);

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::failed(TokenStream::with_offset(
                Rc::new(vec!["a", "a", "a", "b"]),
                0
            ),)
        );
    }

    #[test]
    fn test_many_delim_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", ",", "a", ","]));

        let rule = Some("test");
        let parser = many(rule, lit("a"), Some(lit(r",")));

        let result: ParserResult<&str, &str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec!["a", ",", "a"],
                    vec![
                        AST::new(None, vec!["a"], Vec::new()),
                        AST::new(None, vec![","], Vec::new()),
                        AST::new(None, vec!["a"], Vec::new())
                    ]
                ),
                tokens
            )
        );
    }
}
