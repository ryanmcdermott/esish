#![allow(dead_code)]
use crate::tokenizer::{Token, TokenType, Tokenizer};
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Program {
    body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EmptyStatement {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct NumericLiteral {
    value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StringLiteral {
    value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Literal {
    NumericLiteral(NumericLiteral),
    StringLiteral(StringLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExpressionStatement {
    expression: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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

    fn _expect_ast(program: String, expected: String, should_pretty_print_actual: bool) {
        let tokenizer = Tokenizer::new(program);
        let mut parser = Parser::new(tokenizer);
        let parse_tree = parser.parse();
        let mut actual_ast = serde_json::to_string_pretty(&parse_tree).unwrap();

        if should_pretty_print_actual {
            println!("Actual AST: \n {} \n\n\n\n", actual_ast);
        }

        actual_ast = str::replace(actual_ast.as_str(), "\n", "");
        actual_ast = str::replace(actual_ast.as_str(), " ", "");
        actual_ast = str::replace(actual_ast.as_str(), "\t", "");

        let mut expected_ast = str::replace(expected.as_str(), "\n", "");
        expected_ast = str::replace(expected_ast.as_str(), " ", "");
        expected_ast = str::replace(expected_ast.as_str(), "\t", "");

        assert_eq!(expected_ast, actual_ast);
    }

    macro_rules! expect_ast {
        ($program:ident, $expected:ident) => {
            _expect_ast($program, $expected, false)
        };
        ($program:ident, $expected:ident, $should_pretty_print_actual:expr) => {
            _expect_ast($program, $expected, true)
        };
    }

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
        let program = "42;".to_string();
        let expected = r#"
            {
                "Program": {
                  "body": [
                    {
                      "ExpressionStatement": {
                        "expression": {
                          "Literal": {
                            "NumericLiteral": {
                              "value": 42
                            }
                          }
                        }
                      }
                    },
                    {
                      "EmptyStatement": {}
                    }
                  ]
                }
              }"#
        .to_string();

        expect_ast!(program, expected, true);
    }
}
