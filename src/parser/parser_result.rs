use core::panic;

use super::{ast::AST, token_stream::TokenStream};

#[derive(Eq, PartialEq, Debug)]
enum ParserResultStatus {
    OK,
    MISMATCH,
    ABORT(String),
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParserResult<RuleId> {
    result: ParserResultStatus,
    pub ast: AST<RuleId>,
    pub remaining: TokenStream,
}

impl <RuleId> ParserResult<RuleId> {
    pub fn failed(remaining: TokenStream) -> Self {
        Self::new(ParserResultStatus::MISMATCH, AST::empty(), remaining)
    }

    pub fn abort(remaining: TokenStream, abort_message: String) -> Self {
        Self::new(ParserResultStatus::ABORT(abort_message), AST::empty(), remaining)
    }

    pub fn succeeded(ast: AST<RuleId>, remaining: TokenStream) -> Self {
        Self::new(ParserResultStatus::OK, ast, remaining)
    }

    fn new(result: ParserResultStatus, ast: AST<RuleId>, remaining: TokenStream) -> Self {
        Self { result, ast, remaining }
    }

    pub fn is_ok(&self) -> bool {
        self.result == ParserResultStatus::OK
    }

    pub fn is_abort(&self) -> bool {
        match self.result {
            ParserResultStatus::ABORT(_) => true,
            _ => false,
        }
    }

    pub fn abort_message(&self) -> String {
        match &self.result {
            ParserResultStatus::ABORT(message) => message.clone(),
            _ => panic!("ParserResult is not abort"),
        }
    }
}