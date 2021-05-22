#![allow(dead_code)]
use lazy_static::lazy_static;
use regex::Regex;

pub type TokenType = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Number(i64),
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub value: Literal,
}

pub struct Tokenizer {
    text: String,
    cursor: i64,
}

struct TokenRule {
    kind: TokenType,
    rule: &'static Regex,
}

lazy_static! {
    // Token Regular Expressions.
    static ref NUMBER_REGEX: Regex = Regex::new(r"^\d+").unwrap();
    static ref STRING_REGEX: Regex = Regex::new(r"^'[^']*'").unwrap();
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap();

    // Token Rule definitions.
    static ref NUMBER_TOKEN_RULE: TokenRule = TokenRule {
        kind: "NumberLiteral".to_string(),
        rule: &NUMBER_REGEX
    };
    static ref STRING_TOKEN_RULE: TokenRule = TokenRule {
        kind: "StringLiteral".to_string(),
        rule: &STRING_REGEX
    };
    static ref IGNORE_WHITESPACE_TOKEN_RULE: TokenRule = TokenRule {
        kind: "IgnoreToken".to_string(),
        rule: &WHITESPACE_REGEX
    };

    // Token Rule set.
    static ref TOKEN_RULES: Vec<&'static TokenRule> = vec![
        &NUMBER_TOKEN_RULE,
        &STRING_TOKEN_RULE,
        &IGNORE_WHITESPACE_TOKEN_RULE,
    ];
}

impl Tokenizer {
    pub fn new(text: String) -> Tokenizer {
        Tokenizer {
            text: text,
            cursor: 0,
        }
    }

    pub fn has_more_tokens(&self) -> bool {
        self.cursor < (self.text.chars().count() as i64)
    }

    pub fn get_next_token(&mut self) -> Option<Token> {
        if !self.has_more_tokens() {
            return None;
        }

        let token_rules = TOKEN_RULES.clone();
        for token_rule in token_rules.into_iter() {
            if token_rule.rule.is_match(self.text.as_str()) {
                let mat = token_rule.rule.find(self.text.as_str()).unwrap();
                let mat_start = mat.start();
                let mat_end = mat.end();

                if token_rule.kind == "IgnoreToken" {
                    self.text = self.text.as_str()[mat_end..].to_string();
                    self.cursor += mat_end as i64;
                    return Some(Token {
                        kind: token_rule.kind.clone(),
                        value: Literal::Empty,
                    });
                }

                if token_rule.kind == "NumberLiteral" {
                    let value = self.text.as_str()[..mat_end].to_string();
                    let ret = Some(Token {
                        kind: token_rule.kind.clone(),
                        value: Literal::Number(value.parse::<i64>().unwrap()),
                    });

                    self.text = self.text.as_str()[mat_end..].to_string();
                    self.cursor += mat_end as i64;
                    return ret;
                }

                if token_rule.kind == "StringLiteral" {
                    let begin_quote = mat_start + 1;
                    let end_quote = mat_end - 1;
                    let value = self.text.as_str()[begin_quote..end_quote].to_string();

                    let ret = Some(Token {
                        kind: token_rule.kind.clone(),
                        value: Literal::String(value),
                    });

                    self.text = self.text.as_str()[mat_end..].to_string();
                    self.cursor += mat_end as i64;
                    return ret;
                }
            }
        }

        None
    }

    pub fn get_cursor(&self) -> i64 {
        self.cursor
    }

    pub fn get_text(&self) -> &String {
        &self.text
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn numeric_literal() {
        let program = "42";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual = tokenizer.get_next_token().unwrap();
        let expected = Token {
            kind: "NumberLiteral".to_string(),
            value: Literal::Number(42),
        };

        assert_eq!(actual, expected);
        assert_eq!(tokenizer.cursor, program.chars().count() as i64);
    }
    #[test]
    fn string_literal() {
        let program = "'hello'";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual = tokenizer.get_next_token().unwrap();
        let expected = Token {
            kind: "StringLiteral".to_string(),
            value: Literal::String(String::from("hello")),
        };

        assert_eq!(actual, expected);
        assert_eq!(tokenizer.cursor, program.chars().count() as i64);
    }
    #[test]
    fn ignore_whitespace() {
        let program = "   ' hello'";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual1 = tokenizer.get_next_token().unwrap();
        let expected1 = Token {
            kind: "IgnoreToken".to_string(),
            value: Literal::Empty,
        };
        assert_eq!(actual1, expected1);

        let actual2 = tokenizer.get_next_token().unwrap();
        let expected2 = Token {
            kind: "StringLiteral".to_string(),
            value: Literal::String(String::from(" hello")),
        };
        assert_eq!(actual2, expected2);
    }
}
