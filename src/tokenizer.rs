#![allow(dead_code)]
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum TokenType {
    NumberLiteral,
    StringLiteral,
    IgnoreToken,
    Semicolon,
    OpenBlock,
    CloseBlock,
    OpenParen,
    CloseParen,
    Comma,
    Point,
    OpenSquareBracket,
    CloseSquareBracket,
    KeywordLet,
    KeywordIf,
    KeywordElse,
    KeywordTrue,
    KeywordFalse,
    KeywordNull,
    KeywordWhile,
    KeywordDo,
    KeywordFor,
    KeywordDef,
    KeywordReturn,
    KeywordClass,
    KeywordExtends,
    KeywordSuper,
    KeywordNew,
    KeywordThis,
    Identifier,
    Equality,
    SimpleAssignment,
    ComplexAssignment,
    OperatorAdd,
    OperatorMultiply,
    OperatorRelational,
    OperatorLogicalAnd,
    OperatorLogicalOr,
    OperatorLogicalNot,
}

type Literal = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub value: Option<Literal>,
}

pub struct Tokenizer {
    text: String,
}

struct TokenRule {
    kind: TokenType,
    rule: &'static Regex,
}

lazy_static! {
    // --------------------------------------------------------------------------------
    // Token Regular Expressions.
    // --------------------------------------------------------------------------------
    // Literals
    static ref NUMBER_REGEX: Regex = Regex::new(r"^\d+").unwrap();
    static ref STRING_REGEX: Regex = Regex::new(r"^'[^']*'").unwrap();

    // Comments and whitespace.
    static ref MULTI_LINE_COMMENT_REGEX: Regex = Regex::new(r"^/\*[\s\S]*?\*/").unwrap();
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s+").unwrap();
    static ref SINGLE_LINE_COMMENT_REGEX: Regex = Regex::new(r"^//.*").unwrap();

    // Symbols and delimiters
    static ref SEMICOLON_REGEX: Regex = Regex::new(r"^;").unwrap();
    static ref OPEN_BLOCK_REGEX: Regex = Regex::new(r"^{").unwrap();
    static ref CLOSE_BLOCK_REGEX: Regex = Regex::new(r"^}").unwrap();
    static ref OPEN_PAREN_REGEX: Regex = Regex::new(r"^\(").unwrap();
    static ref CLOSE_PAREN_REGEX: Regex = Regex::new(r"^\)").unwrap();
    static ref COMMA_REGEX: Regex = Regex::new(r"^,").unwrap();
    static ref POINT_REGEX: Regex = Regex::new(r"^\.").unwrap();
    static ref OPEN_SQUARE_BRACKET_REGEX: Regex = Regex::new(r"^\[").unwrap();
    static ref CLOSE_SQUARE_BRACKET_REGEX: Regex = Regex::new(r"^\]").unwrap();

    // Keywords
    static ref KEYWORD_LET_REGEX: Regex = Regex::new(r"^\blet\b").unwrap();
    static ref KEYWORD_IF_REGEX: Regex = Regex::new(r"^\bif\b").unwrap();
    static ref KEYWORD_ELSE_REGEX: Regex = Regex::new(r"^\belse\b").unwrap();
    static ref KEYWORD_TRUE_REGEX: Regex = Regex::new(r"^\btrue\b").unwrap();
    static ref KEYWORD_FALSE_REGEX: Regex = Regex::new(r"^\bfalse\b").unwrap();
    static ref KEYWORD_NULL_REGEX: Regex = Regex::new(r"^\bnull\b").unwrap();
    static ref KEYWORD_WHILE_REGEX: Regex = Regex::new(r"^\bwhile\b").unwrap();
    static ref KEYWORD_DO_REGEX: Regex = Regex::new(r"^\bdo\b").unwrap();
    static ref KEYWORD_FOR_REGEX: Regex = Regex::new(r"^\bfor\b").unwrap();
    static ref KEYWORD_DEF_REGEX: Regex = Regex::new(r"^\bdef\b").unwrap();
    static ref KEYWORD_RETURN_REGEX: Regex = Regex::new(r"^\breturn\b").unwrap();
    static ref KEYWORD_CLASS_REGEX: Regex = Regex::new(r"^\bclass\b").unwrap();
    static ref KEYWORD_EXTENDS_REGEX: Regex = Regex::new(r"^\bextends\b").unwrap();
    static ref KEYWORD_SUPER_REGEX: Regex = Regex::new(r"^\bsuper\b").unwrap();
    static ref KEYWORD_NEW_REGEX: Regex = Regex::new(r"^\bnew\b").unwrap();
    static ref KEYWORD_THIS_REGEX: Regex = Regex::new(r"^\bthis\b").unwrap();

    // Identifiers
    static ref IDENTIFIER_REGEX: Regex = Regex::new(r"^\w+").unwrap();

    // Equality operators: ==, !=
    static ref EQUALITY_REGEX: Regex = Regex::new(r"^[=!]=").unwrap();

    // Assignment operators: =, *=, /=, +=, -=
    static ref SIMPLE_ASSIGN_REGEX: Regex = Regex::new(r"^=").unwrap();
    static ref COMPLEX_ASSIGN_REGEX: Regex = Regex::new(r"^[-+*/]=").unwrap();

    // Math operators: +, -
    static ref ADD_REGEX: Regex = Regex::new(r"^[+-]").unwrap();
    static ref MULTIPLY_REGEX: Regex = Regex::new(r"^[*/]").unwrap();

    // Relational operators: >, >=, <, <=
    static ref RELATIONAL_REGEX: Regex = Regex::new(r"^[<>]=?").unwrap();

    // Logical operators: &&, ||
    static ref LOGICAL_AND_REGEX: Regex = Regex::new(r"^&&").unwrap();
    static ref LOGICAL_OR_REGEX: Regex = Regex::new(r"^\|\|").unwrap();
    static ref LOGICAL_NOT_REGEX: Regex = Regex::new(r"^!").unwrap();

    // --------------------------------------------------------------------------------
    // Token Rule definitions.
    // --------------------------------------------------------------------------------
    // Literals
    static ref NUMBER_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::NumberLiteral,
        rule: &NUMBER_REGEX
    };
    static ref STRING_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::StringLiteral,
        rule: &STRING_REGEX
    };

    // Comments and whitespace.
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

    // Symbols and delimiters
    static ref SEMICOLON_TOKEN_RULE: TokenRule = TokenRule {
        kind: TokenType::Semicolon,
        rule: &SEMICOLON_REGEX
    };


    // --------------------------------------------------------------------------------
    // Token Rule Set.
    // --------------------------------------------------------------------------------
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
                    TokenType::StringLiteral => {
                        let begin_quote = mat_start + 1;
                        let end_quote = mat_end - 1;
                        let value = self.text.as_str()[begin_quote..end_quote].to_string();

                        let ret = Some(Token {
                            kind: token_rule.kind,
                            value: Some(value),
                        });

                        self.text = self.text.as_str()[mat_end..].to_string();
                        return ret;
                    }
                    TokenType::IgnoreToken => {
                        let ret = Some(Token {
                            kind: token_rule.kind,
                            value: None,
                        });
                        self.text = self.text.as_str()[mat_end..].to_string();
                        return ret;
                    }
                    _ => {
                        let value = self.text.as_str()[..mat_end].to_string();
                        let ret = Some(Token {
                            kind: token_rule.kind,
                            value: Some(value),
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

    fn expect_tokens(program: String, tokens: Vec<Token>) {
        let mut tokenizer = Tokenizer::new(program);

        for token in tokens.iter() {
            assert_eq!(tokenizer.get_next_token().unwrap(), *token);
        }
    }
    #[test]
    fn numeric_literal() {
        expect_tokens(
            String::from("42"),
            vec![Token {
                kind: TokenType::NumberLiteral,
                value: Some(String::from("42")),
            }],
        );
    }
    #[test]
    fn string_literal() {
        expect_tokens(
            String::from("'hello'"),
            vec![Token {
                kind: TokenType::StringLiteral,
                value: Some(String::from("hello")),
            }],
        );
    }
    #[test]
    fn ignore_whitespace() {
        expect_tokens(
            String::from("    ' hello'"),
            vec![
                Token {
                    kind: TokenType::IgnoreToken,
                    value: None,
                },
                Token {
                    kind: TokenType::StringLiteral,
                    value: Some(String::from(" hello")),
                },
            ],
        );
    }
    #[test]
    fn ignore_single_line_comment() {
        let program = r#"// Single line comment
        'hello'"#;
        expect_tokens(
            String::from(program),
            vec![
                Token {
                    kind: TokenType::IgnoreToken,
                    value: None,
                },
                Token {
                    kind: TokenType::IgnoreToken,
                    value: None,
                },
                Token {
                    kind: TokenType::StringLiteral,
                    value: Some(String::from("hello")),
                },
            ],
        );
    }

    #[test]
    fn ignore_multi_line_comment() {
        let program = r#"/**
         * Multi 
         * line 
         * comment
         */
        'hello'"#;
        expect_tokens(
            String::from(program),
            vec![
                Token {
                    kind: TokenType::IgnoreToken,
                    value: None,
                },
                Token {
                    kind: TokenType::IgnoreToken,
                    value: None,
                },
                Token {
                    kind: TokenType::StringLiteral,
                    value: Some(String::from("hello")),
                },
            ],
        );
    }

    #[test]
    fn semicolon() {
        expect_tokens(
            String::from("' hello';"),
            vec![
                Token {
                    kind: TokenType::StringLiteral,
                    value: Some(String::from(" hello")),
                },
                Token {
                    kind: TokenType::Semicolon,
                    value: Some(String::from(";")),
                },
            ],
        );
    }
}
