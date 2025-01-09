use core::panic;
use std::{
    any::{Any, TypeId},
    fmt::Debug,
};

use regex::Regex;

use super::{
    ast::AST,
    parser_result::ParserResult,
    token_stream::{Token, TokenStream},
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
    AndMatch(AndMatch<RuleId>),
    OrMatch(OrMatch<RuleId>),
    AtLeastOne(AtLeastOne<RuleId>),
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
            Combinators::MatchRegex(parser) => parser.parse(tokens),
            Combinators::AndMatch(parser) => parser.parse(tokens),
            Combinators::OrMatch(parser) => parser.parse(tokens),
            Combinators::AtLeastOne(parser) => parser.parse(tokens),
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
            token: Token::str(token),
        }
    }
}

pub fn lit<RuleId>(token: Token) -> Combinators<RuleId> {
    Combinators::MatchToken(MatchToken::new(None, token))
}

pub fn slit<RuleId>(token: &str) -> Combinators<RuleId> {
    Combinators::MatchToken(MatchToken::new(None, Token::str(token)))
}

impl<RuleId> ParserCombinator<RuleId> for MatchToken<RuleId>
where
    RuleId: Copy,
    Token: Clone + Eq,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
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
                Token::StringToken(token_str) => {
                    let captures = self.regex.captures(&token_str);
                    if captures.is_some() && captures.unwrap()[0] == token_str {
                        return ParserResult::succeeded(
                            AST::new(self.id, vec![Token::StringToken(token_str)], Vec::new()),
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
    Token: Clone,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
        ParserResult::succeeded(AST::empty(), tokens.clone())
    }
}

pub fn match_none<RuleId>() -> Combinators<RuleId> {
    Combinators::MatchNone(MatchNone)
}

/*
    Matches anything, as long as the current and following tokens to not match the 'excluded' combinator (if provided).
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
    Token: Clone + Eq + 'static,
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
    Token: Clone + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
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
            matched.extend(ast.matched.clone());
            children.push(ast);
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
}

impl<RuleId> OrMatch<RuleId> {
    pub fn new(id: Option<RuleId>, rules: Vec<Combinators<RuleId>>) -> Self {
        Self { id, rules }
    }
}

pub fn or_match<RuleId>(id: RuleId, rules: Vec<Combinators<RuleId>>) -> Combinators<RuleId> {
    Combinators::OrMatch(OrMatch::new(Some(id), rules))
}

impl<RuleId> ParserCombinator<RuleId> for OrMatch<RuleId>
where
    RuleId: Copy,
    Token: Clone + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
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

pub fn optional<RuleId>(id: Option<RuleId>, parser: Combinators<RuleId>) -> Combinators<RuleId>
where
    RuleId: Copy,
    Token: Clone + Eq + 'static,
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
pub fn many<RuleId>(
    id: Option<RuleId>,
    element: Combinators<RuleId>,
    delim: Option<Combinators<RuleId>>,
) -> Combinators<RuleId>
where
    RuleId: Copy + Debug + 'static,
    Token: Clone + Debug + Eq + 'static,
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
pub struct AtLeastOne<RuleId> {
    id: Option<RuleId>,
    element: Box<Combinators<RuleId>>,
    delim: Option<Box<Combinators<RuleId>>>,
}

impl<RuleId> AtLeastOne<RuleId> {
    pub fn new(
        id: Option<RuleId>,
        element: Combinators<RuleId>,
        delim: Option<Combinators<RuleId>>,
    ) -> Self {
        Self {
            id,
            element: Box::new(element),
            delim: delim.map(|x| Box::new(x)),
        }
    }
}

pub fn at_least_one<RuleId>(
    id: Option<RuleId>,
    element: Combinators<RuleId>,
    delim: Option<Combinators<RuleId>>,
) -> Combinators<RuleId> {
    Combinators::AtLeastOne(AtLeastOne::new(id, element, delim))
}

impl<RuleId> ParserCombinator<RuleId> for AtLeastOne<RuleId>
where
    RuleId: Copy,
    Token: Clone + Eq + 'static,
{
    fn parse(&self, tokens: &TokenStream) -> ParserResult<RuleId> {
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
            many, optional, regex, slit, AndMatch, AtLeastOne, MatchAny, MatchNone,
            MatchRegex, MatchToken, OrMatch, ParserCombinator,
        },
        parser_result::ParserResult,
        token_stream::{Token, TokenStream},
    };

    #[test]
    fn test_match_token_success() {
        let tokens = TokenStream::from_str(vec!["a", "b"]);

        let rule = Some("test");
        let parser = MatchToken::match_str(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![Token::str("a")], Vec::new()),
                TokenStream::with_offset(Rc::new(vec![Token::str("a"), Token::str("b")]), 1)
            )
        );
    }

    #[test]
    fn test_match_token_failed() {
        let tokens = TokenStream::from_str(vec!["c", "b"]);

        let rule = Some("test");
        let parser = MatchToken::match_str(rule, "a");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_success() {
        let tokens = TokenStream::from_str(vec!["variable", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![Token::str("variable")], Vec::new()),
                TokenStream::with_offset(Rc::new(vec![Token::str("variable"), Token::str("b")]), 1)
            )
        );
    }

    #[test]
    fn test_regex_failure_partial() {
        let tokens = TokenStream::from_str(vec!["variableA", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[a-z]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_regex_failure() {
        let tokens = TokenStream::from_str(vec!["variableA", "b"]);

        let rule = Some("test");
        let parser = MatchRegex::new(rule, r"[0-9]+");

        let result = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_match_none_empty_stream() {
        let tokens: TokenStream = TokenStream::from_str(Vec::new());

        let result: ParserResult<&str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_none() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a"]);

        let result: ParserResult<&str> = MatchNone.parse(&tokens);
        assert_eq!(result, ParserResult::succeeded(AST::empty(), tokens));
    }

    #[test]
    fn test_match_any_success() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a"]);

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(slit("b")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(rule, vec![Token::str("a")], Vec::new()),
                TokenStream::with_offset(Rc::new(vec![Token::str("a")]), 1)
            )
        );
    }

    #[test]
    fn test_match_any_failure() {
        let tokens: TokenStream = TokenStream::from_str(vec!["b"]);

        let rule = Some("test");
        let parser = MatchAny::new(rule, Some(slit("b")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_and_match_success() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", "b", "c"]);

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![Token::str("a"), Token::str("b")],
                    vec![
                        AST::new(None, vec![Token::str("a")], Vec::new()),
                        AST::new(None, vec![Token::str("b")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![Token::str("a"), Token::str("b"), Token::str("c")]),
                    2
                )
            )
        );
    }

    #[test]
    fn test_and_match_failure() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", "c"]);

        let rule = Some("test");
        let parser = AndMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_or_match_success() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", "b"]);

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![Token::str("a")],
                    vec![AST::new(None, vec![Token::str("a")], Vec::new())]
                ),
                TokenStream::with_offset(Rc::new(vec![Token::str("a"), Token::str("b")]), 1)
            )
        );
    }

    #[test]
    fn test_or_match_failure() {
        let tokens: TokenStream = TokenStream::from_str(vec!["c"]);

        let rule = Some("test");
        let parser = OrMatch::new(rule, vec![slit("a"), slit("b")]);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens));
    }

    #[test]
    fn test_optional_some() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a"]);

        let rule = Some("test");
        let parser = optional(rule, slit("a"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![Token::str("a")],
                    vec![AST::new(None, vec![Token::str("a")], Vec::new())]
                ),
                TokenStream::with_offset(Rc::new(vec![Token::str("a")]), 1)
            )
        );
    }

    #[test]
    fn test_optional_none() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a"]);

        let rule = Some("test");
        let parser = optional(rule, slit("b"));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(AST::new(rule, Vec::new(), vec![AST::empty()]), tokens)
        );
    }

    #[test]
    fn test_at_least_once_no_delim_success() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", "a", "a", "b"]);

        let rule = Some("test");
        let parser = AtLeastOne::new(rule, slit("a"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![Token::str("a"), Token::str("a"), Token::str("a")],
                    vec![
                        AST::new(None, vec![Token::str("a")], Vec::new()),
                        AST::new(None, vec![Token::str("a")], Vec::new()),
                        AST::new(None, vec![Token::str("a")], Vec::new())
                    ]
                ),
                TokenStream::with_offset(
                    Rc::new(vec![
                        Token::str("a"),
                        Token::str("a"),
                        Token::str("a"),
                        Token::str("b")
                    ]),
                    3
                ),
            )
        );
    }

    #[test]
    fn test_at_least_once_failure() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", "a", "a", "b"]);

        let rule = Some("test");
        let parser = AtLeastOne::new(rule, slit("b"), None);

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(result, ParserResult::failed(tokens.clone()));
    }

    #[test]
    fn test_many_delim_success() {
        let tokens: TokenStream = TokenStream::from_str(vec!["a", ",", "a", ","]);

        let rule = Some("test");
        let parser = many(rule, slit("a"), Some(slit(",")));

        let result: ParserResult<&str> = parser.parse(&tokens);
        assert_eq!(
            result,
            ParserResult::succeeded(
                AST::new(
                    rule,
                    vec![Token::str("a"), Token::str(","), Token::str("a")],
                    vec![
                        AST::new(None, vec![Token::str("a")], Vec::new()),
                        AST::new(None, vec![Token::str(",")], Vec::new()),
                        AST::new(None, vec![Token::str("a")], Vec::new())
                    ]
                ),
                tokens
            )
        );
    }
}
