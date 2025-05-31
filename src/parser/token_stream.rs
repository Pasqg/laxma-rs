use std::rc::Rc;

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub struct TokenInfo {
    pub line: usize,
    pub col: usize,
}

impl TokenInfo {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token {
    StringToken(String, TokenInfo),
}

impl Token {
    pub fn str(str: &str, info: TokenInfo) -> Self {
        Self::StringToken(str.to_string(), info)
    }

    pub fn unwrap_str(&self) -> String {
        match self {
            Self::StringToken(str, _) => str.to_owned(),
        }
    }

    pub fn info(&self) -> TokenInfo {
        match self {
            Self::StringToken(_, info) => *info,
        }
    }

    pub fn matches(&self, other: &Token) -> bool {
        match (self, other) {
            (Self::StringToken(str1, _), Self::StringToken(str2, _)) => str1 == str2, 
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct TokenStream {
    tokens: Rc<Vec<Token>>,
    start: usize,
    // token info of last token, to be used when stream end is reached
    last_info: TokenInfo, 
}

impl TokenStream {
    pub fn new(tokens: Rc<Vec<Token>>, last_info: TokenInfo) -> Self {
        Self::with_offset(tokens, 0, last_info)
    }

    pub fn with_offset(tokens: Rc<Vec<Token>>, start: usize, last_info: TokenInfo) -> Self {
        Self { tokens, start, last_info }
    }

    pub fn not_done(&self) -> bool {
        self.start < self.tokens.len()
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.start]
    }

    pub fn last_info(&self) -> TokenInfo {
        self.last_info
    }

    pub fn advance(&self) -> (Token, Self) {
        (
            self.tokens[self.start].clone(),
            Self::with_offset(self.tokens.clone(), self.start + 1, self.last_info),
        )
    }
}
