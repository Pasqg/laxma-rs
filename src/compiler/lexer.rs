use regex::Regex;

use crate::{compiler::grammar::LEXER_REGEX, parser::token_stream::TokenStream};

pub(super) struct Lexer;

impl Lexer {
    pub(super) fn token_stream(input: &str) -> TokenStream {
        let pattern = LEXER_REGEX.join("|");
        let re = Regex::new(&pattern).unwrap();

        let matches: Vec<&str> = re.find_iter(input).map(|m| m.as_str()).collect();
        TokenStream::from_str(&matches)
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::parser::token_stream::{Token, TokenStream};

    use super::Lexer;

    #[test]
    fn test_identifier() {
        let stream = Lexer::token_stream("identifier");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![Token::str("identifier")]), 0)
        )
    }

    #[test]
    fn test_2_identifier() {
        let stream = Lexer::token_stream("one two");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![Token::str("one"), Token::str("two")]), 0)
        )
    }

    #[test]
    fn test_3_identifier() {
        let stream = Lexer::token_stream("one+ two* three");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![Token::str("one+"), Token::str("two*"), Token::str("three")]), 0)
        )
    }

    #[test]
    fn test_3_parenthesis() {
        let stream = Lexer::token_stream("()[] () [] ( ) [ ] one() two(three) four(five six)");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![
                Token::str("("),
                Token::str(")"),
                Token::str("["),
                Token::str("]"),
                Token::str("("),
                Token::str(")"),
                Token::str("["),
                Token::str("]"),
                Token::str("("),
                Token::str(")"),
                Token::str("["),
                Token::str("]"),
                Token::str("one"),
                Token::str("("),
                Token::str(")"),
                Token::str("two"),
                Token::str("("),
                Token::str("three"),
                Token::str(")"),
                Token::str("four"),
                Token::str("("),
                Token::str("five"),
                Token::str("six"),
                Token::str(")"),
                ]), 0)
        )
    }

    #[test]
    fn test_type() {
        let stream = Lexer::token_stream("fn type X -> Y fn type X -> Y|Z |W");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![
                Token::str("fn"),
                Token::str("type"),
                Token::str("X"),
                Token::str("->"),
                Token::str("Y"),
                Token::str("fn"),
                Token::str("type"),
                Token::str("X"),
                Token::str("->"),
                Token::str("Y"),
                Token::str("|"),
                Token::str("Z"),
                Token::str("|"),
                Token::str("W"),
                ]), 0)
        )
    }

    #[test]
    fn test_type_constructor() {
        let stream = Lexer::token_stream("type::variant() type::Variant(data) Type::variant(data data)");

        assert_eq!(
            stream,
            TokenStream::with_offset(Rc::new(vec![
                Token::str("type"),
                Token::str("::"),
                Token::str("variant"),
                Token::str("("),
                Token::str(")"),
                Token::str("type"),
                Token::str("::"),
                Token::str("Variant"),
                Token::str("("),
                Token::str("data"),
                Token::str(")"),
                Token::str("Type"),
                Token::str("::"),
                Token::str("variant"),
                Token::str("("),
                Token::str("data"),
                Token::str("data"),
                Token::str(")"),
                ]), 0)
        )
    }
}
