use std::rc::Rc;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct TokenStream<TokenType> {
    tokens: Rc<Vec<TokenType>>,
    start: usize,
}

// For more flexibility, TokenType could be Clone, however enforcing Copy means we don't end up with low performance by accident
// i.e. using clone types for tokens unnecessarily. Ideally tokens are &str, enums or simple structs.
impl <TokenType> TokenStream<TokenType> where TokenType : Copy {
    pub fn new(tokens: Rc<Vec<TokenType>>) -> Self {
        Self::with_offset(tokens, 0)
    }

    pub fn with_offset(tokens: Rc<Vec<TokenType>>, start: usize) -> Self {
        Self { tokens, start}
    }

    pub fn not_done(&self) -> bool {
        self.start < self.tokens.len()
    }

    pub fn advance(&self) -> (TokenType, Self) {
        (self.tokens[self.start], Self::with_offset(self.tokens.clone(), self.start + 1))
    }
}