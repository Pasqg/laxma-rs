use super::{ast::AST, token_stream::TokenStream};

#[derive(Eq, PartialEq, Debug)]
pub struct ParserResult<RuleId, TokenType> {
    result: bool,
    ast: AST<RuleId, TokenType>,
    remaining: TokenStream<TokenType>,
}

impl <RuleId, TokenType> ParserResult<RuleId, TokenType> where TokenType : Copy {
    pub fn failed(remaining: TokenStream<TokenType>) -> Self {
        Self::new(false, AST::empty(), remaining)
    }

    pub fn succeeded(ast: AST<RuleId, TokenType>, remaining: TokenStream<TokenType>) -> Self {
        Self::new(true, ast, remaining)
    }

    pub fn new(result: bool, ast: AST<RuleId, TokenType>, remaining: TokenStream<TokenType>) -> Self {
        Self { result, ast, remaining }
    }

    pub fn is_ok(&self) -> bool {
        self.result
    }
}