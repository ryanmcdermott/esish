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
pub struct Identifier {
    name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum LeftAssignment {
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AssignmentExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: TokenType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct VariableDeclaration {
    id: Identifier,
    init: Option<AssignmentExpression>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct VariableStatement {
    declarations: Vec<VariableDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BlockStatement {
    body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BooleanLiteral {
    value: bool,
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
    BooleanLiteral(BooleanLiteral),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: TokenType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LogicalExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: TokenType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnaryExpression {
    argument: Box<Expression>,
    operator: TokenType,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    LogicalExpression(LogicalExpression),
    AssignmentExpression(AssignmentExpression),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum LogicalExpressionBuilder {
    LogicalOrExpression,
    LogicalAndExpression,
    EqualityExpression,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum BinaryExpressionBuilder {
    PrimaryExpression,
    MultiplicativeExpression,
    AdditiveExpression,
    RelationalExpression,
    UnaryExpression,
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
    VariableStatement(VariableStatement),
}

type BinaryExpressionFn = fn(&Parser) -> Expression;

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
        VariableStatement {
            declarations: vec![],
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
     *   : LogicalOrExpression
     *   | LeftHandSideExpression AssignmentOperator AssignmentExpression
     *   ;
     */
    fn assignment_expression(&mut self) -> Expression {
        let left = self.logical_or_expression();

        if !self.is_assignment(&self.lookahead) {
            return left;
        }

        Expression::AssignmentExpression(AssignmentExpression {
            left: Box::new(left),
            right: Box::new(self.assignment_expression()),
            operator: TokenType::SimpleAssignment,
        })
    }

    /**
     * LogicalOrExpression
     *   : LogicalAndExpression
     *   | LogicalAndExpression LOGICAL_OR LogicalOrExpression
     *   ;
     *
     *  &&
     */
    fn logical_or_expression(&mut self) -> Expression {
        self.logical_expression(
            LogicalExpressionBuilder::LogicalAndExpression,
            TokenType::OperatorLogicalOr,
        )
    }

    /**
     * LogicalAndExpression
     *   : EqualityExpression
     *   | EqualityExpression LOGICAL_AND LogicalAndExpression
     *   ;
     *
     *  &&
     */
    fn logical_and_expression(&mut self) -> Expression {
        self.logical_expression(
            LogicalExpressionBuilder::EqualityExpression,
            TokenType::OperatorLogicalAnd,
        )
    }

    /**
     * EqualityExpression
     *   : ReplationalExpression
     *   | ReplationalExpression EQUALITY_OPERATOR EqualityExpression
     *   ;
     *
     *  ==, !=
     */
    fn equality_expression(&mut self) -> Expression {
        self.binary_expression(
            BinaryExpressionBuilder::RelationalExpression,
            TokenType::Equality,
        )
    }

    /**
     * RelationalExpression
     *   : AdditiveExpression
     *   | AdditiveExpression RELATIONAL_OPERATOR RelationalExpression
     *   ;
     *
     *  >, >=, <, <=
     */
    fn relational_expression(&mut self) -> Expression {
        self.binary_expression(
            BinaryExpressionBuilder::AdditiveExpression,
            TokenType::OperatorRelational,
        )
    }

    /**
     * AdditiveExpression
     *   : MultiplicativeExpression
     *   | AdditiveExpression ADDITIVE_OPERATOR MultiplicativeExpression
     *   ;
     */
    fn additive_expression(&mut self) -> Expression {
        self.binary_expression(
            BinaryExpressionBuilder::MultiplicativeExpression,
            TokenType::OperatorAdd,
        )
    }

    /**
     * MultiplicativeExpression
     *   : UnaryExpression
     *   | MultiplicativeExpression MULTIPLICATIVE_OPERATOR UnaryExpression
     *   ;
     */
    fn multiplicative_expression(&mut self) -> Expression {
        self.binary_expression(
            BinaryExpressionBuilder::UnaryExpression,
            TokenType::OperatorMultiply,
        )
    }

    fn left_hand_side_expression(&mut self) -> Expression {
        self.primary_expression()
    }

    fn logical_expression(
        &mut self,
        logical_expression_builder: LogicalExpressionBuilder,
        operator: TokenType,
    ) -> Expression {
        match logical_expression_builder {
            LogicalExpressionBuilder::EqualityExpression => {
                let mut left = self.equality_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::LogicalExpression(LogicalExpression {
                        left: Box::new(left),
                        right: Box::new(self.equality_expression()),
                        operator: operator,
                    })
                }

                return left;
            }

            LogicalExpressionBuilder::LogicalAndExpression => {
                let mut left = self.logical_and_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::LogicalExpression(LogicalExpression {
                        left: Box::new(left),
                        right: Box::new(self.logical_and_expression()),
                        operator: operator,
                    })
                }

                return left;
            }

            LogicalExpressionBuilder::LogicalOrExpression => {
                let mut left = self.logical_or_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::LogicalExpression(LogicalExpression {
                        left: Box::new(left),
                        right: Box::new(self.logical_or_expression()),
                        operator: operator,
                    })
                }

                return left;
            }
        }
    }

    fn binary_expression(
        &mut self,
        expression_builder: BinaryExpressionBuilder,
        operator: TokenType,
    ) -> Expression {
        match expression_builder {
            BinaryExpressionBuilder::AdditiveExpression => {
                let mut left = self.additive_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    let right = self.additive_expression();
                    left = Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: operator,
                    })
                }

                return left;
            }

            BinaryExpressionBuilder::MultiplicativeExpression => {
                let mut left = self.multiplicative_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.multiplicative_expression()),
                        operator: operator,
                    })
                }

                return left;
            }

            BinaryExpressionBuilder::PrimaryExpression => {
                let mut left = self.primary_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.primary_expression()),
                        operator: operator,
                    })
                }

                return left;
            }

            BinaryExpressionBuilder::RelationalExpression => {
                let mut left = self.relational_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.relational_expression()),
                        operator: operator,
                    })
                }

                return left;
            }

            BinaryExpressionBuilder::UnaryExpression => {
                let mut left = self.unary_expression();

                while !self.lookahead.is_none() && self.lookahead.as_ref().unwrap().kind == operator
                {
                    self.eat(operator);
                    left = Expression::BinaryExpression(BinaryExpression {
                        left: Box::new(left),
                        right: Box::new(self.unary_expression()),
                        operator: operator,
                    })
                }

                return left;
            }
        }
    }

    /**
     * UnaryExpression
     *   : LeftHandSideExpression
     *   | ADDITIVE_OPERATOR UnaryExpression
     *   | LOGICAL_NOT UnaryExpression
     */
    fn unary_expression(&mut self) -> Expression {
        let mut operator: Option<TokenType> = None;
        let lookahead = self.lookahead.clone().unwrap();

        if lookahead.kind == TokenType::OperatorAdd {
            self.eat(TokenType::OperatorAdd);
            operator = Some(TokenType::OperatorAdd);
        }

        if lookahead.kind == TokenType::OperatorLogicalNot {
            self.eat(TokenType::OperatorLogicalNot);
            operator = Some(TokenType::OperatorLogicalNot);
        }

        if !operator.is_none() {
            return Expression::UnaryExpression(UnaryExpression {
                operator: operator.unwrap(),
                argument: Box::new(self.unary_expression()),
            });
        }

        return self.left_hand_side_expression();
    }

    /**
     * PrimaryExpression
     *   : Literal
     *   | ParenthesizedExpression
     *   | Identifier
     *   | LeftHandSideExpression
     *   ;
     */
    fn primary_expression(&mut self) -> Expression {
        let lookahead = self.lookahead.clone();
        if self.is_literal(&lookahead) {
            return Expression::Literal(self.literal());
        }

        let lookahead_kind = lookahead.unwrap().kind;

        match lookahead_kind {
            TokenType::OpenParen => {
                return self.parenthesized_expression();
            }

            TokenType::Identifier => {
                return Expression::Identifier(self.identifier());
            }

            _ => {
                return self.left_hand_side_expression();
            }
        }
    }

    /**
     * ParenthesizedExpression
     *   : '(' Expression ')'
     *   ;
     */
    fn parenthesized_expression(&mut self) -> Expression {
        self.eat(TokenType::OpenParen);
        let expression = self.expression();
        self.eat(TokenType::CloseParen);

        return expression;
    }

    /**
     * Identifier
     *   : IDENTIFIER
     *   ;
     */
    fn identifier(&mut self) -> Identifier {
        let lookahead: Token = self.lookahead.clone().unwrap();
        self.eat(TokenType::Identifier);

        let mut name = String::from("");

        if !lookahead.value.is_none() {
            name = lookahead.value.unwrap();
        }

        Identifier { name }
    }

    /**
     * Literal
     *   : NumericLiteral
     *   | StringLiteral
     *   | BooleanLiteral
     *   ;
     */
    fn literal(&mut self) -> Literal {
        let left = self.lookahead.clone().unwrap();
        self.eat(left.kind);

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

            TokenType::KeywordTrue | TokenType::KeywordFalse => {
                let literal = BooleanLiteral {
                    value: left.value.as_ref().unwrap().parse::<bool>().unwrap(),
                };

                return Literal::BooleanLiteral(literal);
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

    fn is_literal(&self, token: &Option<Token>) -> bool {
        if token.is_none() {
            return false;
        }
        let token_type = token.clone().unwrap().kind;

        return token_type == TokenType::NumberLiteral
            || token_type == TokenType::StringLiteral
            || token_type == TokenType::KeywordTrue
            || token_type == TokenType::KeywordFalse
            || token_type == TokenType::KeywordNull;
    }

    fn is_assignment(&self, token: &Option<Token>) -> bool {
        if token.is_none() {
            return false;
        }
        let token_type = token.clone().unwrap().kind;

        return token_type == TokenType::SimpleAssignment
            || token_type == TokenType::ComplexAssignment;
    }

    fn is_operator(&self, token: &Option<Token>) -> bool {
        if token.is_none() {
            return false;
        }
        let token_type = token.clone().unwrap().kind;

        return token_type == TokenType::OperatorAdd
            || token_type == TokenType::OperatorMultiply
            || token_type == TokenType::OperatorRelational
            || token_type == TokenType::OperatorLogicalAnd
            || token_type == TokenType::OperatorLogicalOr
            || token_type == TokenType::OperatorLogicalNot;
    }

    fn eat(&mut self, expected_token_type: TokenType) {
        let token = self.lookahead.as_ref().unwrap();
        let expected_token_type_str = serde_json::to_string(&expected_token_type).unwrap();

        if token.kind != expected_token_type {
            let (token_type, token_value) = self.get_lookahead_token_str();
            panic!(
                "Incorrect token type, for\nToken type: {}\nToken value: {}\n\n Expected token type: {}",
                token_type, token_value, expected_token_type_str
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

    #[test]
    fn unary_expression() {
        let program = " !true;".to_string();
        let expected = r#"
            {
                "Program": {
                  "body": [
                    {
                      "ExpressionStatement": {
                        "expression": {
                          "UnaryExpression": {
                            "argument": {
                              "Literal": {
                                "BooleanLiteral": {
                                  "value": true
                                }
                              }
                            },
                            "operator": "OperatorLogicalNot"
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
    fn binary_expression_additive() {
        let program = "2 + 3".to_string();
        let expected = r#"
        {
            "Program": {
              "body": [
                {
                  "ExpressionStatement": {
                    "expression": {
                      "BinaryExpression": {
                        "left": {
                          "Literal": {
                            "NumericLiteral": {
                              "value": 2
                            }
                          }
                        },
                        "right": {
                          "Literal": {
                            "NumericLiteral": {
                              "value": 3
                            }
                          }
                        },
                        "operator": "OperatorAdd"
                      }
                    }
                  }
                }
              ]
            }
          }"#
        .to_string();

        expect_ast!(program, expected);
    }
}
