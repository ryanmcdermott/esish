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
pub struct VariableStatement {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BlockStatement {
    body: Vec<Node>,
}

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
    BlockStatement(BlockStatement),
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

    fn get_lookahead_token_str(&self) -> (String, String) {
        let lookahead = self.lookahead.clone().unwrap();
        let token_type = serde_json::to_string(&lookahead.kind).unwrap();
        let mut token_value = String::from("None");

        if !lookahead.value.is_none() {
            token_value = lookahead.value.unwrap();
        }
        (token_type, token_value)
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
            let lookahead = self.lookahead.clone().unwrap();
            if lookahead.kind == TokenType::IgnoreToken {
                self.lookahead = self.tokenizer.get_next_token();
                continue;
            }

            if lookahead.kind == TokenType::CloseBlock {
                break;
            }
            statements.push(self.statement())
        }

        statements
    }

    /**
     * Statement
     *   : ExpressionStatement
     *   | BlockStatement
     *   | EmptyStatement
     *   | VariableStatement
     *   ;
     */
    fn statement(&mut self) -> Node {
        let lookahead = self.lookahead.clone();
        match lookahead.unwrap().kind {
            TokenType::Semicolon => {
                return Node::EmptyStatement(self.empty_statement());
            }

            TokenType::OpenBlock => {
                return Node::BlockStatement(self.block_statement());
            }

            _ => {
                return Node::ExpressionStatement(self.expression_statement());
            }
        }
    }

    /**
     * BlockStatement
     *   : '{' OptStatementList '}'
     *   ;
     */

    fn block_statement(&mut self) -> BlockStatement {
        self.eat(TokenType::OpenBlock);
        let mut body = Vec::new();

        let lookahead = self.lookahead.clone().unwrap();

        if lookahead.kind != TokenType::CloseBlock {
            body = self.statement_list();
        }

        self.eat(TokenType::CloseBlock);

        BlockStatement { body: body }
    }

    /**
     * VariableStatement
     *   : 'let' VariableDeclarationList ';'
     *   ;
     */
    fn variable_statement(&mut self) -> VariableStatement {
        VariableStatement {}
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
            _ => {
                let lookahead = self.lookahead.clone().unwrap();
                let token_type = serde_json::to_string(&lookahead.kind).unwrap();
                let mut token_value = String::from("None");

                if !lookahead.value.is_none() {
                    token_value = lookahead.value.unwrap();
                }

                panic!(
                    "Literal parse not implemented yet for \nToken type: {}\nToken value: {}",
                    token_type, token_value
                )
            }
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
            let (token_type, token_value) = self.get_lookahead_token_str();
            panic!(
                "Incorrect token type, for\nToken type: {}\nToken value: {}",
                token_type, token_value
            );
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
        let program = "".to_string();
        let expected = r#"
            {
                "Program": {
                  "body": []
                }
              }"#
        .to_string();

        expect_ast!(program, expected);
    }
    #[test]
    fn empty_statement() {
        let program = ";".to_string();
        let expected = r#"
            {
                "Program": {
                  "body": [
                    {
                        "EmptyStatement": {}
                    }
                  ]
                }
              }"#
        .to_string();

        expect_ast!(program, expected);
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

        expect_ast!(program, expected);
    }

    #[test]
    fn block_statement() {
        let program = r#"
            {
                42;
            }
        "#
        .to_string();
        let expected = r#"
        {
            "Program": {
              "body": [
                {
                  "BlockStatement": {
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
                }
              ]
            }
          } 
          "#
        .to_string();

        expect_ast!(program, expected);
    }
}
