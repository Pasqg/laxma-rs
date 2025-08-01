use core::panic;
use std::{cell::RefCell, rc::Rc};

use regex::Regex;

use super::{
    ast::AST,
    parser_result::ParserResult,
    token_stream::{Token, TokenInfo, TokenStream},
};

pub trait ParserCombinator<RuleId> {
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId>;
}

#[derive(Clone)]
pub enum Combinators<RuleId> {
    MatchToken(MatchToken<RuleId>),
    MatchRegex(MatchRegex<RuleId>),
    MatchNone(MatchNone),
    MatchAny(MatchAny<RuleId>),
    Exclude(Exclude<RuleId>),
    AndMatch(AndMatch<RuleId>),
    OrMatch(OrMatch<RuleId>),
    MatchMany(MatchMany<RuleId>),
    Reference(Reference<RuleId>),
    AbortOnFailure(Rc<Combinators<RuleId>>, String),
}

impl<RuleId> Combinators<RuleId> {
    pub fn bind(&self, parser: Combinators<RuleId>) {
        match self {
            Combinators::Reference(reference) => reference.bind(parser),
            _ => panic!("Cannot bind a non-reference parser"),
        }
    }
}

impl<RuleId> ParserCombinator<RuleId> for Combinators<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        match self {
            Combinators::MatchToken(parser) => parser.parse(tokens),
            Combinators::MatchNone(parser) => parser.parse(tokens),
            Combinators::MatchAny(parser) => parser.parse(tokens),
            Combinators::Exclude(parser) => parser.parse(tokens),
            Combinators::MatchRegex(parser) => parser.parse(tokens),
            Combinators::AndMatch(parser) => parser.parse(tokens),
            Combinators::OrMatch(parser) => parser.parse(tokens),
            Combinators::MatchMany(parser) => parser.parse(tokens),
            Combinators::Reference(parser) => parser.parse(tokens),
            Combinators::AbortOnFailure(parser, abort_message) => {
                let result = parser.parse(tokens);
                if result.is_abort() {
                    return result;
                }
                if !result.is_ok() {
                    if result.remaining.not_done() {
                        let message = abort_message
                            .replace("{token}", &tokens.peek().unwrap_str())
                            .replace("{line}", &tokens.peek().info().line.to_string())
                            .replace("{col}", &tokens.peek().info().col.to_string());
                        return ParserResult::abort(result.remaining, message);
                    }
                    return ParserResult::abort(
                        result.remaining,
                        abort_message
                            .replace("{token}", "EOF")
                            .replace("{line}", &tokens.last_info().line.to_string())
                            .replace("{col}", &tokens.last_info().col.to_string()),
                    );
                }
                result
            }
        }
    }
}

#[derive(Clone)]
pub struct MatchToken<RuleId> {
    id: Option<RuleId>,
    token: Token,
}

impl<RuleId> MatchToken<RuleId> {
    pub fn new(id: Option<RuleId>, token: Token) -> Self {
        Self { id, token }
    }

    pub fn match_str(id: Option<RuleId>, token: &str) -> Self {
        Self {
            id,
            token: Token::str(token, TokenInfo::new(0, 0)),
        }
    }
}

pub fn lit<RuleId>(token: Token) -> Combinators<RuleId> {
    Combinators::MatchToken(MatchToken::new(None, token))
}

pub fn slit<RuleId>(token: &str) -> Combinators<RuleId> {
    Combinators::MatchToken(MatchToken::match_str(None, token))
}

pub fn aborting<RuleId>(
    combinator: Combinators<RuleId>,
    abort_message: String,
) -> Combinators<RuleId> {
    Combinators::AbortOnFailure(Rc::new(combinator), abort_message)
}

impl<RuleId> ParserCombinator<RuleId> for MatchToken<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            if token.matches(&self.token) {
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

pub fn regex<RuleId>(pattern: &str) -> Combinators<RuleId> {
    Combinators::MatchRegex(MatchRegex::new(None, pattern))
}

impl<RuleId> ParserCombinator<RuleId> for MatchRegex<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            match token {
                Token::StringToken(token_str, token_info) => {
                    let captures = self.regex.captures(&token_str);
                    if captures.is_some() && captures.unwrap()[0] == token_str {
                        return ParserResult::succeeded(
                            AST::new(
                                self.id,
                                vec![Token::StringToken(token_str, token_info)],
                                Vec::new(),
                            ),
                            remaining,
                        );
                    }
                }
                _ => panic!("Regex matcher doesn't support non string tokens"),
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

#[derive(Clone)]
pub struct MatchNone;

impl<RuleId> ParserCombinator<RuleId> for MatchNone
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        ParserResult::succeeded(AST::empty(), tokens.clone())
    }
}

pub fn match_none<RuleId>() -> Combinators<RuleId> {
    Combinators::MatchNone(MatchNone)
}

/*
    Matches anything, as long as the current and following tokens do not match the 'excluded' combinator (if provided).
*/
#[derive(Clone)]
pub struct MatchAny<RuleId> {
    id: Option<RuleId>,
    excluded: Option<Box<Combinators<RuleId>>>,
}

impl<RuleId> MatchAny<RuleId> {
    pub fn new(id: Option<RuleId>, excluded: Option<Combinators<RuleId>>) -> Self {
        Self {
            id,
            excluded: excluded.map(|x| Box::new(x)),
        }
    }
}

impl<'a, RuleId> ParserCombinator<RuleId> for MatchAny<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
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
    Succeds if the parser matches, and the excluded parser doesn't match the same tokens.
    Example:
*/
#[derive(Clone)]
pub struct Exclude<RuleId> {
    include: Box<Combinators<RuleId>>,
    excluded: Box<Combinators<RuleId>>,
}

impl<RuleId> Exclude<RuleId> {
    pub fn new(parser: Combinators<RuleId>, excluded: Combinators<RuleId>) -> Self {
        Self {
            include: Box::new(parser),
            excluded: Box::new(excluded),
        }
    }
}

pub fn exclude<RuleId>(
    include: Combinators<RuleId>,
    exclude: Combinators<RuleId>,
) -> Combinators<RuleId> {
    Combinators::Exclude(Exclude::new(include, exclude))
}

impl<RuleId> ParserCombinator<RuleId> for Exclude<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        let result = self.excluded.parse(tokens);
        if result.is_abort() {
            return result;
        }
        if !result.is_ok() {
            return self.include.parse(tokens);
        }
        return ParserResult::failed(tokens.clone());
    }
}

/*
    Returns a match if all the input rules match, otherwise it fails (and backtracks).
*/
#[derive(Clone)]
pub struct AndMatch<RuleId> {
    id: Option<RuleId>,
    rules: Vec<Combinators<RuleId>>,
}

impl<RuleId> AndMatch<RuleId> {
    pub fn new(id: Option<RuleId>, rules: Vec<Combinators<RuleId>>) -> Self {
        Self { id, rules }
    }
}

pub fn and_match<RuleId>(id: RuleId, rules: Vec<Combinators<RuleId>>) -> Combinators<RuleId> {
    Combinators::AndMatch(AndMatch::new(Some(id), rules))
}

impl<RuleId> ParserCombinator<RuleId> for AndMatch<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        let mut _remaining = tokens.clone();
        let mut matched = Vec::new();
        let mut children = Vec::new();
        for rule in &self.rules {
            let parser_result = rule.parse(&_remaining);
            if parser_result.is_abort() {
                return parser_result;
            }
            if !parser_result.is_ok() {
                return ParserResult::failed(tokens.clone());
            }
            _remaining = parser_result.remaining.clone();
            matched.extend(parser_result.ast.matched.clone());
            children.push(parser_result.ast);
        }
        return ParserResult::succeeded(AST::new(self.id, matched, children), _remaining);
    }
}

/*
    Returns a match if any of the input rules match, otherwise it fails (and backtracks).
*/
#[derive(Clone)]
pub struct OrMatch<RuleId> {
    id: Option<RuleId>,
    rules: Vec<Combinators<RuleId>>,
    flattened: bool,
}

impl<RuleId> OrMatch<RuleId> {
    pub fn new(id: Option<RuleId>, rules: Vec<Combinators<RuleId>>, flattened: bool) -> Self {
        Self {
            id,
            rules,
            flattened,
        }
    }
}

pub fn or_match<RuleId>(id: RuleId, rules: Vec<Combinators<RuleId>>) -> Combinators<RuleId> {
    Combinators::OrMatch(OrMatch::new(Some(id), rules, false))
}

pub fn or_match_flat<RuleId>(rules: Vec<Combinators<RuleId>>) -> Combinators<RuleId> {
    Combinators::OrMatch(OrMatch::new(None, rules, true))
}

impl<RuleId> ParserCombinator<RuleId> for OrMatch<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        for rule in &self.rules {
            let parser_result = rule.parse(tokens);
            if parser_result.is_abort() {
                return parser_result;
            }
            if parser_result.is_ok() {
                //todo: probably we don't care about one of children or parent's matched, so we can remove a clone here
                if self.flattened {
                    return ParserResult::succeeded(parser_result.ast, parser_result.remaining);
                } else {
                    return ParserResult::succeeded(
                        AST::new(
                            self.id,
                            parser_result.ast.matched.clone(),
                            vec![parser_result.ast],
                        ),
                        parser_result.remaining,
                    );
                }
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

pub fn optional<RuleId>(parser: Combinators<RuleId>) -> Combinators<RuleId>
where
    RuleId: Copy,
{
    Combinators::OrMatch(OrMatch::new(
        None,
        vec![parser, Combinators::MatchNone(MatchNone)],
        true,
    ))
}

/*
   Matches at least N occurrences of the provided 'element' rule.
   Optionally, matches a delimiter rule between each of the elements.
*/
#[derive(Clone)]
pub struct MatchMany<RuleId> {
    id: Option<RuleId>,
    element: Box<Combinators<RuleId>>,
    delim: Option<Box<Combinators<RuleId>>>,
    n: u32,
}

impl<RuleId> MatchMany<RuleId> {
    pub fn new(
        id: Option<RuleId>,
        element: Combinators<RuleId>,
        delim: Option<Combinators<RuleId>>,
        n: u32,
    ) -> Self {
        Self {
            id,
            element: Box::new(element),
            delim: delim.map(|x| Box::new(x)),
            n,
        }
    }
}

impl<RuleId> ParserCombinator<RuleId> for MatchMany<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        let mut matches_count = 0;

        let result = self.element.parse(tokens);
        if result.is_abort() {
            return result;
        }
        if !result.is_ok() {
            if matches_count < self.n {
                return ParserResult::failed(tokens.clone());
            } else {
                return ParserResult::succeeded(AST::empty(), tokens.clone());
            }
        }

        matches_count += 1;
        let mut matched = result.ast.matched.clone();
        let mut children = vec![result.ast];
        let mut remaining = result.remaining;
        let mut mismatch: bool = false;
        while remaining.not_done() && !mismatch {
            let (delim_result, delim_ast, delim_remaining) = if self.delim.is_some() {
                let result = self.delim.as_ref().unwrap().parse(&remaining);
                if result.is_abort() {
                    return result;
                }
                (result.is_ok(), result.ast, result.remaining)
            } else {
                (true, AST::empty(), remaining.clone())
            };

            if !delim_result {
                mismatch = true;
            } else {
                let element_result = self.element.parse(&delim_remaining);
                if element_result.is_abort() {
                    return element_result;
                }
                if element_result.is_ok() {
                    matches_count += 1;
                    if self.delim.is_some() {
                        matched.extend(delim_ast.matched.clone());
                        children.push(delim_ast);
                    }
                    matched.extend(element_result.ast.matched.clone());
                    children.push(element_result.ast);
                    remaining = element_result.remaining;
                } else {
                    mismatch = true;
                }
            }
        }

        if matches_count < self.n {
            return ParserResult::failed(tokens.clone());
        } else {
            return ParserResult::succeeded(AST::new(self.id, matched, children), remaining);
        }
    }
}

pub fn many<RuleId>(
    id: Option<RuleId>,
    element: Combinators<RuleId>,
    delim: Option<Combinators<RuleId>>,
) -> Combinators<RuleId> {
    at_least_n(id, element, delim, 0)
}

pub fn at_least_one<RuleId>(
    id: Option<RuleId>,
    element: Combinators<RuleId>,
    delim: Option<Combinators<RuleId>>,
) -> Combinators<RuleId> {
    at_least_n(id, element, delim, 1)
}

pub fn at_least_n<RuleId>(
    id: Option<RuleId>,
    element: Combinators<RuleId>,
    delim: Option<Combinators<RuleId>>,
    n: u32,
) -> Combinators<RuleId> {
    Combinators::MatchMany(MatchMany::new(id, element, delim, n))
}

#[derive(Clone)]
pub struct Reference<RuleId> {
    name: Option<&'static str>,
    reference: Rc<RefCell<Option<Combinators<RuleId>>>>,
}

impl<RuleId> Reference<RuleId> {
    pub fn new() -> Self {
        Self {
            name: None,
            reference: Rc::new(RefCell::new(None)),
        }
    }

    pub fn named(name: &'static str) -> Self {
        Self {
            name: Some(name),
            reference: Rc::new(RefCell::new(None)),
        }
    }

    pub fn bind(&self, parser: Combinators<RuleId>) {
        if self.reference.borrow().as_ref().is_some() {
            panic!("Reference parser '{:?}' was already binded!", self.name);
        }
        self.reference.replace(Some(parser));
    }
}

pub fn parser_ref<RuleId>() -> Combinators<RuleId> {
    Combinators::Reference(Reference::new())
}

impl<RuleId> ParserCombinator<RuleId> for Reference<RuleId>
where
    RuleId: Copy,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        if self.reference.borrow().as_ref().is_none() {
            panic!("Reference parser '{:?}' is unbinded!", self.name);
        }
        self.reference.borrow().as_ref().unwrap().parse(tokens)
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;
    use std::rc::Rc;

    use crate::parser::{
        ast::AST,
        combinators::{
            and_match, at_least_one, exclude, many, match_none, optional, or_match_flat, regex,
            slit, AndMatch, Combinators, MatchAny, MatchMany, MatchNone, MatchRegex, MatchToken,
            OrMatch, ParserCombinator, Reference,
        },
        parser_result::ParserResult,
        token_stream::{Token, TokenInfo, TokenStream},
    };

    #[test]
    fn test_match_token_success() {
        let tokens = test_stream(&["a", "b"]);

        let rule = Some("test");
        let parser = MatchToken::match_str(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![test_token("a")], Vec::new()),
                TokenStream::with_offset(
                    Rc::new(vec![test_token("a"), test_token("b")]),
                    1,
                    TokenInfo::new(0, 0)
                )
            )
        );
    }

    #[test]
    fn test_match_token_failed() {
        let tokens = test_stream(&["c", "b"]);

        let rule = Some("test");
        let parser = MatchToken::match_str(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_success() {
        let tokens = test_stream(&["variable", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![test_token("variable")], Vec::new()),
                TokenStream::with_offset(
                    Rc::new(vec![test_token("variable"), test_token("b")]),
                    1,
                    TokenInfo::new(0, 0)
                )
            )
        );
    }

    #[test]
    fn test_regex_failure_partial() {
        let tokens = test_stream(&["variableA", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_failure() {
        let tokens = test_stream(&["variableA", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[0-9]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_match_none_empty_stream() {
        let tokens: TokenStream = test_stream(&[]);

        let result: ParserResult<&str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_none() {
        let tokens: TokenStream = test_stream(&["a"]);

        let result: ParserResult<&str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_any_success() {
        let tokens: TokenStream = test_stream(&["a"]);

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(slit("b")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![test_token("a")], Vec::new()),
                TokenStream::with_offset(Rc::new(vec![test_token("a")]), 1, TokenInfo::new(0, 0))
            )
        );
    }

    #[test]
    fn test_match_any_failure() {
        let tokens: TokenStream = test_stream(&["b"]);

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(slit("b")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_exclude_success() {
        let tokens: TokenStream = test_stream(&["b"]);

        let parser = exclude(regex(r"[a-z]+"), slit("a"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(None, vec![test_token("b")], vec![]),
                TokenStream::with_offset(Rc::new(vec![test_token("b")]), 1, TokenInfo::new(0, 0))
            )
        );
    }

    #[test]
    fn test_exclude_failure() {
        let tokens: TokenStream = test_stream(&["b"]);

        let parser = exclude(regex(r"[a-z]+"), slit("b"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_and_match_success() {
        let tokens: TokenStream = test_stream(&["a", "b", "c"]);

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![test_token("a"), test_token("b")],
                    vec![
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token("b")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![test_token("a"), test_token("b"), test_token("c")]),
                    2,
                    TokenInfo::new(0, 0)
                )
            )
        );
    }

    #[test]
    fn test_and_match_failure() {
        let tokens: TokenStream = test_stream(&["a", "c"]);

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_or_match_success() {
        let tokens: TokenStream = test_stream(&["a", "b"]);

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![slit("a"), slit("b")], false);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![test_token("a")],
                    vec![AST::new(None, vec![test_token("a")], Vec::new())]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![test_token("a"), test_token("b")]),
                    1,
                    TokenInfo::new(0, 0)
                )
            )
        );
    }

    #[test]
    fn test_or_match_failure() {
        let tokens: TokenStream = test_stream(&["c"]);

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![slit("a"), slit("b")], false);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_optional_some() {
        let tokens: TokenStream = test_stream(&["a"]);

        let parser = optional(slit("a"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(None, vec![test_token("a")], vec![]),
                TokenStream::with_offset(Rc::new(vec![test_token("a")]), 1, TokenInfo::new(0, 0))
            )
        );
    }

    #[test]
    fn test_optional_none() {
        let tokens: TokenStream = test_stream(&["a"]);

        let parser = optional(slit("b"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(AST::new(None, Vec::new(), vec![]), tokens)
        );
    }

    #[test]
    fn test_at_least_once_no_delim_success() {
        let tokens: TokenStream = test_stream(&["a", "a", "a", "b"]);

        let rule = Some("test");
        let parser = at_least_one(rule, slit("a"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![test_token("a"), test_token("a"), test_token("a")],
                    vec![
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![
                        test_token("a"),
                        test_token("a"),
                        test_token("a"),
                        test_token("b")
                    ]),
                    3,
                    TokenInfo::new(0, 0)
                ),
            )
        );
    }

    #[test]
    fn test_at_least_once_failure() {
        let tokens: TokenStream = test_stream(&["a", "a", "a", "b"]);

        let rule = Some("test");
        let parser = at_least_one(rule, slit("b"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens.clone()));
    }

    #[test]
    fn test_at_least_one_empty_stream_failure() {
        let tokens: TokenStream = test_stream(&[]);

        let rule = Some("test");
        let parser = at_least_one(rule, slit("b"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens.clone()));
    }

    #[test]
    fn test_at_least_one_stream_done_success() {
        let tokens: TokenStream = test_stream(&["a", "a"]);

        let rule = Some("test");
        let parser = at_least_one(rule, slit("a"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![test_token("a"), test_token("a")],
                    vec![
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new()),
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![test_token("a"), test_token("a"),]),
                    2,
                    TokenInfo::new(0, 0)
                ),
            )
        );
    }

    #[test]
    fn test_many_none_success() {
        let tokens: TokenStream = test_stream(&["a", "a", "a", "b"]);

        let rule = Some("test");
        let parser = many(rule, slit("b"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(AST::empty(), tokens.clone())
        );
    }

    #[test]
    fn test_at_least_one_delim_success() {
        let tokens: TokenStream = test_stream(&["a", ",", "a", ","]);

        let rule = Some("test");
        let parser = at_least_one(rule, slit("a"), Some(slit(",")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![test_token("a"), test_token(","), test_token("a")],
                    vec![
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token(",")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![
                        test_token("a"),
                        test_token(","),
                        test_token("a"),
                        test_token(",")
                    ]),
                    3,
                    TokenInfo::new(0, 0),
                ),
            )
        );
    }

    #[test]
    fn test_many_delim_success() {
        let tokens: TokenStream = test_stream(&["a", ",", "a", ","]);

        let rule = Some("test");
        let parser = many(rule, slit("b"), Some(slit(",")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(AST::empty(), tokens.clone())
        );
    }

    #[test]
    fn test_match_many_delim_success() {
        let tokens: TokenStream = test_stream(&["a", ",", "a", ",", "a", ","]);

        let rule = Some("test");
        let parser = MatchMany::new(rule, slit("a"), Some(slit(",")), 3);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![
                        test_token("a"),
                        test_token(","),
                        test_token("a"),
                        test_token(","),
                        test_token("a")
                    ],
                    vec![
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token(",")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new()),
                        AST::new(None, vec![test_token(",")], Vec::new()),
                        AST::new(None, vec![test_token("a")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![
                        test_token("a"),
                        test_token(","),
                        test_token("a"),
                        test_token(","),
                        test_token("a"),
                        test_token(",")
                    ]),
                    5,
                    TokenInfo::new(0, 0)
                ),
            )
        );
    }

    #[test]
    fn test_match_many_delim_failure() {
        let tokens: TokenStream = test_stream(&["a", ",", "a", ",", "a", ","]);

        let rule = Some("test");
        let parser = MatchMany::new(rule, slit("a"), Some(slit(",")), 5);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens.clone()));
    }

    #[test]
    fn test_reference_recursion() {
        let tokens: TokenStream = test_stream(&["a", "a", "a", ","]);

        let parser = Reference::new();
        let body = or_match_flat(vec![
            and_match(
                "and",
                vec![slit("a"), Combinators::Reference(parser.clone())],
            ),
            match_none(),
        ]);
        parser.bind(body);

        let result: ParserResult<&str> = parser.parse(&tokens);
        let expected = ParserResult::succeeded(
            AST::new(
                Some("and"),
                vec![test_token("a"), test_token("a"), test_token("a")],
                vec![
                    AST::new(None, vec![test_token("a")], Vec::new()),
                    AST::new(
                        Some("and"),
                        vec![test_token("a"), test_token("a")],
                        vec![
                            AST::new(None, vec![test_token("a")], Vec::new()),
                            AST::new(
                                Some("and"),
                                vec![test_token("a")],
                                vec![
                                    AST::new(None, vec![test_token("a")], Vec::new()),
                                    AST::empty(),
                                ],
                            ),
                        ],
                    ),
                ],
            ),
            TokenStream::with_offset(
                Rc::new(vec![
                    test_token("a"),
                    test_token("a"),
                    test_token("a"),
                    test_token(","),
                ]),
                3,
                TokenInfo::new(0, 0),
            ),
        );
        assert_result(result, expected);
    }

    fn assert_result<A>(result: ParserResult<A>, expected: ParserResult<A>)
    where
        A: Debug + Eq,
    {
        assert_eq!(
            result, expected,
            "\n{}\nbut expected:\n{}",
            result.ast, expected.ast
        );
    }

    fn test_token(str: &str) -> Token {
        Token::str(str, TokenInfo::new(0, 0))
    }

    fn test_stream(tokens: &[&str]) -> TokenStream {
        TokenStream::new(
            Rc::new(
                tokens
                    .into_iter()
                    .map(|token| Token::str(token, TokenInfo::new(0, 0)))
                    .collect(),
            ),
            TokenInfo::new(0, 0),
        )
    }
}
