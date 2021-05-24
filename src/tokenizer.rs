#![allow(dead_code)]
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum TokenType {
    NumberLiteral,
    StringLiteral,
    IgnoreToken,
    Semicolon,
}

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
    static ref SINGLE_LINE_COMMENT_REGEX: Regex = Regex::new(r"^//.*").unwrap();
    static ref MULTI_LINE_COMMENT_REGEX: Regex = Regex::new(r"^/\*[\s\S]*?\*/").unwrap();
    static ref SEMICOLON_REGEX: Regex = Regex::new(r"^;").unwrap();

    // Token Rule definitions.
    static ref NUMBER_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::NumberLiteral,
        rule: &NUMBER_REGEX
    };
    static ref STRING_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::StringLiteral,
        rule: &STRING_REGEX
    };
    static ref IGNORE_WHITESPACE_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::IgnoreToken,
        rule: &WHITESPACE_REGEX
    };
    static ref IGNORE_SINGLE_LINE_COMMENT_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::IgnoreToken,
        rule: &SINGLE_LINE_COMMENT_REGEX
    };
    static ref IGNORE_MULTI_LINE_COMMENT_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::IgnoreToken,
        rule: &MULTI_LINE_COMMENT_REGEX
    };
    static ref SEMICOLON_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::Semicolon,
        rule: &SEMICOLON_REGEX
    };

    // Token Rule set.
    static ref TOKEN_RULES: Vec<&'static TokenRule> = vec![
        &IGNORE_WHITESPACE_TOKEN_RULE,
        &IGNORE_SINGLE_LINE_COMMENT_TOKEN_RULE,
        &IGNORE_MULTI_LINE_COMMENT_TOKEN_RULE,
        &NUMBER_TOKEN_RULE,
        &STRING_TOKEN_RULE,
        &SEMICOLON_TOKEN_RULE
    ];
}

impl Tokenizer {
    pub fn new(text: String) -> Tokenizer {
        Tokenizer { text: text }
    }

    pub fn has_more_tokens(&self) -> bool {
        self.text.chars().count() > 0
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

                match token_rule.kind {
                    TokenType::IgnoreToken | TokenType::Semicolon => {
                        self.text = self.text.as_str()[mat_end..].to_string();
                        return Some(Token {
                            kind: token_rule.kind,
                            value: Literal::Empty,
                        });
                    }

                    TokenType::NumberLiteral => {
                        let value = self.text.as_str()[..mat_end].to_string();
                        let ret = Some(Token {
                            kind: token_rule.kind,
                            value: Literal::Number(value.parse::<i64>().unwrap()),
                        });
                        self.text = self.text.as_str()[mat_end..].to_string();
                        return ret;
                    }

                    TokenType::StringLiteral => {
                        let begin_quote = mat_start + 1;
                        let end_quote = mat_end - 1;
                        let value = self.text.as_str()[begin_quote..end_quote].to_string();

                        let ret = Some(Token {
                            kind: token_rule.kind,
                            value: Literal::String(value),
                        });

                        self.text = self.text.as_str()[mat_end..].to_string();
                        return ret;
                    }
                }
            }
        }

        None
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
            kind: TokenType::NumberLiteral,
            value: Literal::Number(42),
        };

        assert_eq!(actual, expected);
    }
    #[test]
    fn string_literal() {
        let program = "'hello'";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual = tokenizer.get_next_token().unwrap();
        let expected = Token {
            kind: TokenType::StringLiteral,
            value: Literal::String(String::from("hello")),
        };

        assert_eq!(actual, expected);
    }
    #[test]
    fn ignore_whitespace() {
        let program = "   ' hello'";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual1 = tokenizer.get_next_token().unwrap();
        let expected1 = Token {
            kind: TokenType::IgnoreToken,
            value: Literal::Empty,
        };
        assert_eq!(actual1, expected1);

        let actual2 = tokenizer.get_next_token().unwrap();
        let expected2 = Token {
            kind: TokenType::StringLiteral,
            value: Literal::String(String::from(" hello")),
        };
        assert_eq!(actual2, expected2);
    }
    #[test]
    fn ignore_single_line_comment() {
        let program = r#"// Single line comment
        'hello'"#;
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual1 = tokenizer.get_next_token().unwrap();
        let expected1 = Token {
            kind: TokenType::IgnoreToken,
            value: Literal::Empty,
        };
        assert_eq!(actual1, expected1);

        let actual2 = tokenizer.get_next_token().unwrap();
        let expected2 = Token {
            kind: TokenType::IgnoreToken,
            value: Literal::Empty,
        };
        assert_eq!(actual2, expected2);

        let actual3 = tokenizer.get_next_token().unwrap();
        let expected3 = Token {
            kind: TokenType::StringLiteral,
            value: Literal::String(String::from("hello")),
        };
        assert_eq!(actual3, expected3);
    }

    #[test]
    fn ignore_multi_line_comment() {
        let program = r#"/**
         * Multi 
         * line 
         * comment
         */
        'hello'"#;
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual1 = tokenizer.get_next_token().unwrap();
        let expected1 = Token {
            kind: TokenType::IgnoreToken,
            value: Literal::Empty,
        };
        assert_eq!(actual1, expected1);

        let actual2 = tokenizer.get_next_token().unwrap();
        let expected2 = Token {
            kind: TokenType::IgnoreToken,
            value: Literal::Empty,
        };
        assert_eq!(actual2, expected2);

        let actual3 = tokenizer.get_next_token().unwrap();
        let expected3 = Token {
            kind: TokenType::StringLiteral,
            value: Literal::String(String::from("hello")),
        };
        assert_eq!(actual3, expected3);
    }

    #[test]
    fn semicolon() {
        let program = "' hello';";
        let mut tokenizer = Tokenizer::new(program.to_string());
        let actual1 = tokenizer.get_next_token().unwrap();
        let expected1 = Token {
            kind: TokenType::StringLiteral,
            value: Literal::String(String::from(" hello")),
        };
        assert_eq!(actual1, expected1);

        assert_eq!(
            tokenizer.get_next_token().unwrap(),
            Token {
                kind: TokenType::Semicolon,
                value: Literal::Empty,
            }
        );
    }
}
