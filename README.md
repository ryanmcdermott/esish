# ESish

Recursive descent parser written in Rust for an ECMAScript inspired language.

## Example


### Crate usage
```rust
use esish::{Parser, Tokenizer};

let program = r#"
    class Fib {
    function calc(num) {
        if (num <= 1) {
        return 1;
        }

        return this.calc(num - 1) + this.calc(num - 2);
    }
    }

    let fib = new Fib();
    fib.calc(42);
"#
.to_string();

let tokenizer = Tokenizer::new(program);
let mut parser = Parser::new(tokenizer);
let parse_tree = parser.parse();
let mut actual_ast = serde_json::to_string_pretty(&parse_tree).unwrap();

println!("AST:\n {}", actual_ast);
```

### Output
```json
{
    "Program": {
    "body": [
        {
        "ClassDeclaration": {
            "id": {
            "name": "Fib"
            },
            "body": {
            "body": [
                {
                "FunctionDeclaration": {
                    "name": {
                    "name": "calc"
                    },
                    "params": [
                    {
                        "name": "num"
                    }
                    ],
                    "body": {
                    "body": [
                        {
                        "IfStatement": {
                            "test": {
                            "BinaryExpression": {
                                "left": {
                                "Identifier": {
                                    "name": "num"
                                }
                                },
                                "right": {
                                "Literal": {
                                    "NumericLiteral": {
                                    "value": 1
                                    }
                                }
                                },
                                "operator": "OperatorRelational"
                            }
                            },
                            "consequent": {
                            "BlockStatement": {
                                "body": [
                                {
                                    "ReturnStatement": {
                                    "argument": {
                                        "Literal": {
                                        "NumericLiteral": {
                                            "value": 1
                                        }
                                        }
                                    }
                                    }
                                }
                                ]
                            }
                            },
                            "alternate": null
                        }
                        },
                        {
                        "ReturnStatement": {
                            "argument": {
                            "BinaryExpression": {
                                "left": {
                                "CallExpression": {
                                    "callee": {
                                    "MemberExpression": {
                                        "object": {
                                        "ThisExpression": {}
                                        },
                                        "computed": false,
                                        "property": {
                                        "Identifier": {
                                            "name": "calc"
                                        }
                                        }
                                    }
                                    },
                                    "arguments": [
                                    {
                                        "BinaryExpression": {
                                        "left": {
                                            "Identifier": {
                                            "name": "num"
                                            }
                                        },
                                        "right": {
                                            "Literal": {
                                            "NumericLiteral": {
                                                "value": 1
                                            }
                                            }
                                        },
                                        "operator": "OperatorAdd"
                                        }
                                    }
                                    ]
                                }
                                },
                                "right": {
                                "CallExpression": {
                                    "callee": {
                                    "MemberExpression": {
                                        "object": {
                                        "ThisExpression": {}
                                        },
                                        "computed": false,
                                        "property": {
                                        "Identifier": {
                                            "name": "calc"
                                        }
                                        }
                                    }
                                    },
                                    "arguments": [
                                    {
                                        "BinaryExpression": {
                                        "left": {
                                            "Identifier": {
                                            "name": "num"
                                            }
                                        },
                                        "right": {
                                            "Literal": {
                                            "NumericLiteral": {
                                                "value": 2
                                            }
                                            }
                                        },
                                        "operator": "OperatorAdd"
                                        }
                                    }
                                    ]
                                }
                                },
                                "operator": "OperatorAdd"
                            }
                            }
                        }
                        }
                    ]
                    }
                }
                }
            ]
            },
            "super_class": null
        }
        },
        {
        "VariableStatement": {
            "declarations": [
            {
                "id": {
                "name": "fib"
                },
                "init": {
                "NewExpression": {
                    "callee": {
                    "Identifier": {
                        "name": "Fib"
                    }
                    },
                    "arguments": []
                }
                }
            }
            ]
        }
        },
        {
        "ExpressionStatement": {
            "expression": {
            "CallExpression": {
                "callee": {
                "MemberExpression": {
                    "object": {
                    "Identifier": {
                        "name": "fib"
                    }
                    },
                    "computed": false,
                    "property": {
                    "Identifier": {
                        "name": "calc"
                    }
                    }
                }
                },
                "arguments": [
                {
                    "Literal": {
                    "NumericLiteral": {
                        "value": 42
                    }
                    }
                }
                ]
            }
            }
        }
        }
    ]
    }
}
```