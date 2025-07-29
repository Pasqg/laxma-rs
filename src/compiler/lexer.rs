use std::rc::Rc;

use regex::Regex;

use crate::{
    compiler::grammar::LEXER_REGEX,
    parser::token_stream::{Token, TokenInfo, TokenStream},
};

pub struct Lexer;

impl Lexer {
    pub fn token_stream(input: &str) -> TokenStream {
        let pattern = LEXER_REGEX.join("|");
        let re = Regex::new(&pattern).unwrap();

        let lines: Vec<&str> = input.split("\n").collect();
        let mut matches = Vec::new();
        for i in 0..lines.len() {
            let line = lines[i].trim();
            if !line.starts_with("#") {
                re.find_iter(line)
                    .map(|m| Token::str(m.as_str().trim(), TokenInfo::new(i + 1, m.start() + 1)))
                    .filter(|token| !token.unwrap_str().is_empty())
                    .for_each(|token| matches.push(token));
            }
        }

        let last_info = if matches.is_empty() {
            TokenInfo::new(1, 1)
        } else {
            matches[matches.len() - 1].info()
        };
        TokenStream::new(Rc::new(matches), last_info)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::parser::token_stream::{Token, TokenInfo, TokenStream};

    use super::Lexer;

    #[test]
    fn test_identifier() {
        let stream = Lexer::token_stream("identifier");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![Token::str("identifier", TokenInfo::new(1, 1))]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_2_identifier() {
        let stream = Lexer::token_stream("one two");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("one", TokenInfo::new(1, 1)),
                    Token::str("two", TokenInfo::new(1, 5))
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_3_identifier() {
        let stream = Lexer::token_stream("one+ two* three");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("one+", TokenInfo::new(1, 1)),
                    Token::str("two*", TokenInfo::new(1, 6)),
                    Token::str("three", TokenInfo::new(1, 11))
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_strings() {
        let stream = Lexer::token_stream(r#""one" "" "one two" " one "two" one two . -> one""#);

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str(r#""one""#, TokenInfo::new(1, 1)),
                    Token::str(r#""""#, TokenInfo::new(1, 7)),
                    Token::str(r#""one two""#, TokenInfo::new(1, 10)),
                    Token::str(r#"" one ""#, TokenInfo::new(1, 20)),
                    Token::str("two", TokenInfo::new(1, 27)),
                    Token::str(r#"" one two . -> one""#, TokenInfo::new(1, 30)),
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_numbers() {
        let stream = Lexer::token_stream("1.2 2 3.303 -8 -1.7 +2.1 +5");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("1.2", TokenInfo::new(1, 1)),
                    Token::str("2", TokenInfo::new(1, 5)),
                    Token::str("3.303", TokenInfo::new(1, 7)),
                    Token::str("-8", TokenInfo::new(1, 13)),
                    Token::str("-1.7", TokenInfo::new(1, 16)),
                    Token::str("+2.1", TokenInfo::new(1, 21)),
                    Token::str("+5", TokenInfo::new(1, 26)),
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_parenthesis() {
        let stream = Lexer::token_stream("()[] () [] ( ) [ ] one() two(three) four(five six)");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("(", TokenInfo::new(1, 1)),
                    Token::str(")", TokenInfo::new(1, 2)),
                    Token::str("[", TokenInfo::new(1, 3)),
                    Token::str("]", TokenInfo::new(1, 4)),
                    Token::str("(", TokenInfo::new(1, 6)),
                    Token::str(")", TokenInfo::new(1, 7)),
                    Token::str("[", TokenInfo::new(1, 9)),
                    Token::str("]", TokenInfo::new(1, 10)),
                    Token::str("(", TokenInfo::new(1, 12)),
                    Token::str(")", TokenInfo::new(1, 14)),
                    Token::str("[", TokenInfo::new(1, 16)),
                    Token::str("]", TokenInfo::new(1, 18)),
                    Token::str("one", TokenInfo::new(1, 20)),
                    Token::str("(", TokenInfo::new(1, 23)),
                    Token::str(")", TokenInfo::new(1, 24)),
                    Token::str("two", TokenInfo::new(1, 26)),
                    Token::str("(", TokenInfo::new(1, 29)),
                    Token::str("three", TokenInfo::new(1, 30)),
                    Token::str(")", TokenInfo::new(1, 35)),
                    Token::str("four", TokenInfo::new(1, 37)),
                    Token::str("(", TokenInfo::new(1, 41)),
                    Token::str("five", TokenInfo::new(1, 42)),
                    Token::str("six", TokenInfo::new(1, 47)),
                    Token::str(")", TokenInfo::new(1, 50)),
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_type() {
        let stream = Lexer::token_stream("fn type X -> Y fn type X ->  Y|Z |W");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("fn", TokenInfo::new(1, 1)),
                    Token::str("type", TokenInfo::new(1, 4)),
                    Token::str("X", TokenInfo::new(1, 9)),
                    Token::str("->", TokenInfo::new(1, 11)),
                    Token::str("Y", TokenInfo::new(1, 14)),
                    Token::str("fn", TokenInfo::new(1, 16)),
                    Token::str("type", TokenInfo::new(1, 19)),
                    Token::str("X", TokenInfo::new(1, 24)),
                    Token::str("->", TokenInfo::new(1, 26)),
                    Token::str("Y", TokenInfo::new(1, 30)),
                    Token::str("|", TokenInfo::new(1, 31)),
                    Token::str("Z", TokenInfo::new(1, 32)),
                    Token::str("|", TokenInfo::new(1, 34)),
                    Token::str("W", TokenInfo::new(1, 35)),
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }

    #[test]
    fn test_type_constructor() {
        let stream =
            Lexer::token_stream("type::variant() type::Variant(data) Type::variant(data data)");

        assert_eq!(
            stream,
            TokenStream::with_offset(
                Rc::new(vec![
                    Token::str("type", TokenInfo::new(1, 1)),
                    Token::str("::", TokenInfo::new(1, 5)),
                    Token::str("variant", TokenInfo::new(1, 7)),
                    Token::str("(", TokenInfo::new(1, 14)),
                    Token::str(")", TokenInfo::new(1, 15)),
                    Token::str("type", TokenInfo::new(1, 17)),
                    Token::str("::", TokenInfo::new(1, 21)),
                    Token::str("Variant", TokenInfo::new(1, 23)),
                    Token::str("(", TokenInfo::new(1, 30)),
                    Token::str("data", TokenInfo::new(1, 31)),
                    Token::str(")", TokenInfo::new(1, 35)),
                    Token::str("Type", TokenInfo::new(1, 37)),
                    Token::str("::", TokenInfo::new(1, 41)),
                    Token::str("variant", TokenInfo::new(1, 43)),
                    Token::str("(", TokenInfo::new(1, 50)),
                    Token::str("data", TokenInfo::new(1, 51)),
                    Token::str("data", TokenInfo::new(1, 56)),
                    Token::str(")", TokenInfo::new(1, 60)),
                ]),
                0,
                TokenInfo::new(0, 0),
            )
        )
    }
}
