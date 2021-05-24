#![allow(dead_code)]
use crate::tokenizer::{Token, TokenType, Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    node_type: NodeType,
    body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeType {
    Program,
    ExpressionStatement,
    EmptyStatement,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStatement {
    node_type: NodeType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Program,
    ExpressionStatement(ExpressionStatement),
    EmptyStatement(EmptyStatement),
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
    fn statement_list(&mut self) -> Vec<Node> {
        let mut statements = Vec::new();

        while !self.lookahead.is_none() {
            statements.push(self.statement())
        }

        statements
    }

    /**
     * Statement
     *   : ExpressionStatement
     *   | EmptyStatement
     *   ;
     */
    fn statement(&mut self) -> Node {
        let foo = self.lookahead.clone();
        match foo.unwrap().kind {
            TokenType::Semicolon => {
                let node = self.empty_statement();
                return Node::EmptyStatement(node);
            }

            TokenType::IgnoreToken | TokenType::NumberLiteral | TokenType::StringLiteral => {
                panic!("Not implemented");
            }
        }
    }

    /**
     * ExpressionStatement
     *   : Expression ';'
     *   ;
     */
    fn expression_statement(&self) {
        //
    }

    /**
     * Expression
     *   : Literal
     *   ;
     */
    fn expression(&self) {
        //
    }

    /**
     * Literal
     *   : NumericLiteral
     *   | StringLiteral
     *   ;
     */
    fn literal(&self) {
        //
    }

    /**
     * StringLiteral
     *   : STRING
     *   ;
     */
    fn string_literal(&self) {
        //
    }

    /**
     * NumericLiteral
     *   : NUMBER
     *   ;
     */
    fn numeric_literal(&self) {
        //
    }

    /**
     * EmptyStatement
     *   : ';'
     *   ;
     */
    fn empty_statement(&mut self) -> EmptyStatement {
        self.eat(TokenType::Semicolon);

        EmptyStatement {
            node_type: NodeType::EmptyStatement,
        }
    }

    fn eat(&mut self, token_type: TokenType) {
        let token = self.lookahead.as_ref().unwrap();
        if token.kind != token_type {
            panic!("Incorrect token type");
        }

        self.lookahead = self.tokenizer.get_next_token();
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
            node_type: NodeType::Program,
            body: vec![],
        };

        assert_eq!(actual, expected);
    }
    #[test]
    fn empty_statement() {
        let tokenizer = Tokenizer::new(";".to_string());
        let mut parser = Parser::new(tokenizer);
        let actual = parser.parse();
        let expected = Program {
            node_type: NodeType::Program,
            body: vec![Node::EmptyStatement(EmptyStatement {
                node_type: NodeType::EmptyStatement,
            })],
        };

        assert_eq!(actual, expected);
    }
}
