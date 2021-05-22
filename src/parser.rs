#![allow(dead_code)]
use crate::tokenizer::{Token, Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    node_type: String,
    body: Vec<Node>,
}

pub struct Parser {
    tokenizer: Tokenizer,
    lookahead: Option<Token>,
}

impl Parser {
    pub fn new(mut tokenizer: Tokenizer) -> Parser {
        let lookahead = tokenizer.get_next_token();
        Parser {
            tokenizer: tokenizer,
            lookahead: lookahead,
        }
    }

    fn parse(&mut self) -> Program {
        match &self.lookahead {
            None => {
                return Program {
                    node_type: "Program".to_string(),
                    body: Vec::new(),
                }
            }
            Some(_) => {
                return Program {
                    node_type: "Program".to_string(),
                    body: self.parse_statement_list(),
                }
            }
        };
    }

    fn parse_statement_list(&self) -> Vec<Node> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn empty_program() {
        let tokenizer = Tokenizer::new("".to_string());
        let mut parser = Parser::new(tokenizer);
        let actual = parser.parse();
        let expected = Program {
            node_type: "Program".to_string(),
            body: vec![],
        };

        assert_eq!(actual, expected);
    }
}
