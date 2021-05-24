#![allow(dead_code)]
use crate::tokenizer::{Token, TokenType, Tokenizer};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStatement {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumericLiteral {
    value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    // TODO AssignmentExpression
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {
    expression: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Program(Program),
    EmptyStatement(EmptyStatement),
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

    fn parse(&mut self) -> Node {
        match &self.lookahead {
            None => {
                return Node::Program(Program { body: Vec::new() });
            }
            Some(_) => {
                return Node::Program(Program {
                    body: self.statement_list(),
                });
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
        let lookahead = self.lookahead.clone();
        match lookahead.unwrap().kind {
            TokenType::Semicolon => {
                return Node::EmptyStatement(self.empty_statement());
            }

            _ => {
                return Node::ExpressionStatement(self.expression_statement());
            }
        }
    }

    /**
     * ExpressionStatement
     *   : Expression ';'
     *   ;
     */
    fn expression_statement(&mut self) -> ExpressionStatement {
        ExpressionStatement {
            expression: self.expression(),
        }
    }

    /**
     * Expression
     *   : Literal
     *   ;
     */
    fn expression(&mut self) -> Expression {
        self.assignment_expression()
    }

    /**
     * AssignmentExpression
     *   : LeftHandSideExpression AssignmentOperator AssignmentExpression
     *   ;
     */
    fn assignment_expression(&mut self) -> Expression {
        let left = self.literal();

        self.lookahead = self.tokenizer.get_next_token();

        // TODO: Handle actual assignments
        // if self.lookahead.kind == TokenType::SimpleAssignment || self.lookahead.kind == TokenType::ComplexAssignment {
        //     return left;
        // }

        Expression::Literal(left)
    }

    /**
     * Literal
     *   : NumericLiteral
     *   | StringLiteral
     *   ;
     */
    fn literal(&mut self) -> Literal {
        let left = self.lookahead.as_ref().unwrap();
        match left.kind {
            TokenType::NumberLiteral => {
                let literal = NumericLiteral {
                    value: left.value.as_ref().unwrap().parse::<i64>().unwrap(),
                };

                return Literal::NumericLiteral(literal);
            }
            TokenType::StringLiteral => {
                let literal = StringLiteral {
                    value: left.value.as_ref().unwrap().to_string(),
                };

                return Literal::StringLiteral(literal);
            }
            _ => panic!("Not implemented yet"),
        }
    }

    /**
     * EmptyStatement
     *   : ';'
     *   ;
     */
    fn empty_statement(&mut self) -> EmptyStatement {
        self.eat(TokenType::Semicolon);

        EmptyStatement {}
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
        let expected = Node::Program(Program { body: vec![] });

        assert_eq!(actual, expected);
    }
    #[test]
    fn empty_statement() {
        let tokenizer = Tokenizer::new(";".to_string());
        let mut parser = Parser::new(tokenizer);
        let actual = parser.parse();
        let expected = Node::Program(Program {
            body: vec![Node::EmptyStatement(EmptyStatement {})],
        });

        assert_eq!(actual, expected);
    }

    #[test]
    fn expression_statement_numeric_literal() {
        let tokenizer = Tokenizer::new("42;".to_string());
        let mut parser = Parser::new(tokenizer);
        let actual = parser.parse();
        let expected = Node::Program(Program {
            body: vec![
                Node::ExpressionStatement(ExpressionStatement {
                    expression: Expression::Literal(Literal::NumericLiteral(NumericLiteral {
                        value: 42,
                    })),
                }),
                Node::EmptyStatement(EmptyStatement {}),
            ],
        });

        assert_eq!(actual, expected);
    }
}
