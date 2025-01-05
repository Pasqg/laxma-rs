use regex::Regex;

use super::{ast::AST, parser_result::ParserResult, token_stream::TokenStream};

pub trait Combinator<RuleId, TokenType>:
    Fn(&TokenStream<TokenType>) -> ParserResult<RuleId, TokenType>
{
}

impl<RuleId, TokenType, T: Fn(&TokenStream<TokenType>) -> ParserResult<RuleId, TokenType>>
    Combinator<RuleId, TokenType> for T
{
}

pub fn match_token<RuleId, TokenType>(
    rule_id: Option<RuleId>,
    matched_token: TokenType,
) -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    move |tokens: &TokenStream<TokenType>| {
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            if token == matched_token {
                return ParserResult::succeeded(
                    AST::new(rule_id, vec![token], Vec::new()),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

pub fn lit<RuleId, TokenType>(token: TokenType) -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    match_token(None, token)
}

pub fn match_regex<RuleId>(rule_id: Option<RuleId>, pattern: &str) -> impl Combinator<RuleId, &str>
where
    RuleId: Copy,
{
    let regex = Regex::new(pattern).unwrap();
    move |tokens: &TokenStream<&str>| {
        if tokens.not_done() {
            let (token, remaining) = tokens.advance();
            let captures = regex.captures(token);
            if captures.is_some() && captures.unwrap()[0] == *token {
                return ParserResult::succeeded(
                    AST::new(rule_id, vec![token], Vec::new()),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

pub fn regex<RuleId>(pattern: &str) -> impl Combinator<RuleId, &str>
where
    RuleId: Copy,
{
    match_regex(None, pattern)
}

/*
    Matches nothing. Used to make it easier to match both empty and non-empty sequences with one rule.
*/
pub fn match_none<RuleId, TokenType>() -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    move |tokens: &TokenStream<TokenType>| ParserResult::succeeded(AST::empty(), tokens.clone())
}

/*
    Matches anything, as long as the current and following tokens to not match the 'excluded' combinator (if provided).
*/
pub fn match_any<RuleId, TokenType>(
    id: Option<RuleId>,
    excluded: Option<impl Combinator<RuleId, TokenType>>,
) -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    move |tokens: &TokenStream<TokenType>| {
        if tokens.not_done() {
            if excluded.is_some() && excluded.as_ref().unwrap()(tokens).is_ok() {
                return ParserResult::failed(tokens.clone());
            }

            let (token, remaining) = tokens.advance();
            return ParserResult::succeeded(AST::new(id, vec![token], Vec::new()), remaining);
        }
        ParserResult::failed(tokens.clone())
    }
}

/*
    Returns a match if all the input rules match, otherwise it fails (and backtracks).
*/
pub fn and_match<RuleId, TokenType>(
    id: Option<RuleId>,
    rules: Vec<impl Combinator<RuleId, TokenType>>,
) -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    move |tokens: &TokenStream<TokenType>| {
        let mut _remaining = tokens.clone();
        let mut matched = Vec::new();
        let mut children = Vec::new();
        for rule in &rules {
            let ParserResult {
                result,
                ast,
                remaining,
            } = rule(&_remaining);
            if !result {
                return ParserResult::failed(tokens.clone());
            }
            _remaining = remaining.clone();
            matched.extend(&ast.matched);
            children.push(ast);
        }
        return ParserResult::succeeded(AST::new(id, matched, children), _remaining);
    }
}

/*
    Returns a match if any of the input rules match, otherwise it fails (and backtracks).
*/
pub fn or_match<RuleId, TokenType>(
    id: Option<RuleId>,
    rules: Vec<impl Combinator<RuleId, TokenType>>,
) -> impl Combinator<RuleId, TokenType>
where
    RuleId: Copy,
    TokenType: Copy + Eq,
{
    move |tokens: &TokenStream<TokenType>| {
        for rule in &rules {
            let ParserResult {
                result,
                ast,
                remaining,
            } = rule(tokens);
            if result {
                //todo: probably we don't care about one of children or parent's matched, so we can remove a clone here
                return ParserResult::succeeded(
                    AST::new(id, ast.matched.clone(), vec![ast]),
                    remaining,
                );
            }
        }
        return ParserResult::failed(tokens.clone());
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::parser::{
        ast::AST,
        combinators::{and_match, lit, match_any, match_none, match_regex, or_match},
        parser_result::ParserResult,
        token_stream::TokenStream,
    };

    use super::match_token;

    #[test]
    fn test_match_token_success() {
        let tokens = TokenStream::new(Rc::new(vec!["a", "b"]));

        let rule = Some("test");
        let parser = match_token(rule, "a");

        let result = parser(&tokens);
        assert_eq!(
            result,
            ParserResult::new(
                true,
                AST::new(rule, vec!["a"], Vec::new()),
                tokens.advance().1
            )
        );
    }

    #[test]
    fn test_match_token_failed() {
        let tokens = TokenStream::new(Rc::new(vec!["c", "b"]));

        let rule = Some("test");
        let parser = match_token(rule, "a");

        let result = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }

    #[test]
    fn test_regex_success() {
        let tokens = TokenStream::new(Rc::new(vec!["variable", "b"]));

        let rule = Some("test");
        let parser = match_regex(rule, r"[a-z]+");

        let result = parser(&tokens);
        assert_eq!(
            result,
            ParserResult::new(
                true,
                AST::new(rule, vec!["variable"], Vec::new()),
                tokens.advance().1
            )
        );
    }

    #[test]
    fn test_regex_failure_partial() {
        let tokens = TokenStream::new(Rc::new(vec!["variableA", "b"]));

        let rule = Some("test");
        let parser = match_regex(rule, r"[a-z]+");

        let result = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }

    #[test]
    fn test_regex_failure() {
        let tokens = TokenStream::new(Rc::new(vec!["variableA", "b"]));

        let rule = Some("test");
        let parser = match_regex(rule, r"[0-9]+");

        let result = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }

    #[test]
    fn test_match_none_empty_stream() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(Vec::new()));

        let parser = match_none();

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(result, ParserResult::new(true, AST::empty(), tokens));
    }

    #[test]
    fn test_match_none() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let parser = match_none();

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(result, ParserResult::new(true, AST::empty(), tokens));
    }

    #[test]
    fn test_match_any_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a"]));

        let rule = Some("test");
        let parser = match_any(rule, Some(lit("b")));

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(
            result,
            ParserResult::new(
                true,
                AST::new(rule, vec!["a"], Vec::new()),
                tokens.advance().1
            )
        );
    }

    #[test]
    fn test_match_any_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["b"]));

        let rule = Some("test");
        let parser = match_any(rule, Some(lit("b")));

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }

    #[test]
    fn test_and_match_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "b", "c"]));

        let rule = Some("test");
        let parser = and_match(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(
            result,
            ParserResult::new(
                true,
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
        let parser = and_match(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }

    #[test]
    fn test_or_match_success() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["a", "b"]));

        let rule = Some("test");
        let parser = or_match(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(
            result,
            ParserResult::new(
                true,
                AST::new(rule, vec!["a"], vec![AST::new(None, vec!["a"], Vec::new())]),
                TokenStream::with_offset(Rc::new(vec!["a", "b"]), 1)
            )
        );
    }

    #[test]
    fn test_or_match_failure() {
        let tokens: TokenStream<&str> = TokenStream::new(Rc::new(vec!["c"]));

        let rule = Some("test");
        let parser = or_match(rule, vec![lit("a"), lit("b")]);

        let result: ParserResult<&str, &str> = parser(&tokens);
        assert_eq!(result, ParserResult::new(false, AST::empty(), tokens));
    }
}
