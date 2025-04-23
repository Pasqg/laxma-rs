use std::rc::Rc;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token {
    StringToken(String),
}

impl Token {
    pub fn str(str: &str) -> Self {
        Self::StringToken(str.to_string())
    }

    pub fn unwrap_str(&self) -> String {
        match self {
            Self::StringToken(str) => str.to_owned(),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct TokenStream {
    tokens: Rc<Vec<Token>>,
    start: usize,
}

impl TokenStream {
    pub fn new(tokens: Rc<Vec<Token>>) -> Self {
        Self::with_offset(tokens, 0)
    }

    pub fn from_str(tokens: &[&str]) -> Self {
        Self::new(Rc::new(
            tokens.into_iter().map(|str| Token::str(str)).collect(),
        ))
    }

    pub fn with_offset(tokens: Rc<Vec<Token>>, start: usize) -> Self {
        Self { tokens, start }
    }

    pub fn not_done(&self) -> bool {
        self.start < self.tokens.len()
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.start]
    }

    pub fn advance(&self) -> (Token, Self) {
        (
            self.tokens[self.start].clone(),
            Self::with_offset(self.tokens.clone(), self.start + 1),
        )
    }
}
