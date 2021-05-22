#![allow(dead_code)]
use crate::tokenizer::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralKind {
    Number,
    String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralValue {
    Number(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: LiteralValue,
}

pub struct Expression {}

pub enum NodeType {
    Expression(Expression),
}

pub struct Node {
    node_type: NodeType,
    body: Vec<Node>,
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    fn parse(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn numeric_literal() {}
}
