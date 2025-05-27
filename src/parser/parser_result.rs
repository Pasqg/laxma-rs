use super::{ast::AST, token_stream::TokenStream};

#[derive(Eq, PartialEq, Debug)]
pub struct ParserResult<RuleId> {
    result: bool,
    pub ast: AST<RuleId>,
    pub remaining: TokenStream,
}

impl <RuleId> ParserResult<RuleId> {
    pub fn failed(remaining: TokenStream) -> Self {
        Self::new(false, AST::empty(), remaining)
    }

    pub fn succeeded(ast: AST<RuleId>, remaining: TokenStream) -> Self {
        Self::new(true, ast, remaining)
    }

    pub fn new(result: bool, ast: AST<RuleId>, remaining: TokenStream) -> Self {
        Self { result, ast, remaining }
    }

    pub fn is_ok(&self) -> bool {
        self.result
    }
}