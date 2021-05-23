#![allow(dead_code)]
use crate::tokenizer::{Token, Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    node_type: NodeType,
    body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeType {
    Program,
    ExpressionStatement,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Program,
    ExpressionStatement(ExpressionStatement),
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
                    node_type: NodeType::Program,
                    body: Vec::new(),
                }
            }
            Some(_) => {
                return Program {
                    node_type: NodeType::Program,
                    body: self.statement_list(),
                }
            }
        };
    }

    /**
     * StatementList
     *   : Statement
     *   | StatementList Statement
     *   ;
     */
    fn statement_list(&self) -> Vec<Node> {
        let statements = Vec::new();

        statements
    }

    /**
     * Statement
     *   : ExpressionStatement
     *   | EmptyStatement
     *   ;
     */
    fn statement(&self) {
        //
    }

    /**
     * EmptyStatement
     *   : ';'
     *   ;
     */
    fn empty_statement(&self) {
        //
    }

    fn eat(&self, token: Token) {}
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
            node_type: NodeType::Program,
            body: vec![],
        };

        assert_eq!(actual, expected);
    }

    fn numeric_literal() {}
}
