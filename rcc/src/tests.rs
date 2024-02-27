#[cfg(test)]
mod tests {
    use crate::*;

    macro_rules! token_new {
        // One can use this pattern to create a token where col and line don't matter.
        ($type: expr) => {
            Token {
                col: 0,
                line: 0,
                type_: $type,
            }
        };
        ($type: expr, $line: literal, $col: literal) => {
            Token {
                col: $col,
                line: $line,
                type_: $type,
            }
        };
    }

    #[test]
    fn parse_function_definition_without_parameters_and_empty_body() {
        let tokens = vec![
            token_new!(TokenType::KeywordFn),
            token_new!(TokenType::Identifier("main")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::DelimiterBraceOpen { level: 0 }),
            token_new!(TokenType::DelimiterBraceClose { level: 0 }),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![],
                body: vec![]
            }]
        );
    }
    #[test]
    fn parse_function_definition_without_parameters_and_one_literal_as_return_value() {
        let tokens = vec![
            token_new!(TokenType::KeywordFn),
            token_new!(TokenType::Identifier("main")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::OperatorArrow),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterBraceOpen { level: 0 }),
            token_new!(TokenType::LiteralInteger(5)),
            token_new!(TokenType::DelimiterBraceClose { level: 0 }),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![],
                body: vec![AstNode::ReturnStatement(Box::new(AstNode::LiteralInteger(
                    5
                )))]
            }]
        );
    }
    #[test]
    fn parse_function_definition_with_parameters_and_one_expression_as_return_value() {
        let tokens = vec![
            token_new!(TokenType::KeywordFn),
            token_new!(TokenType::Identifier("main")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterComma),
            token_new!(TokenType::Identifier("y")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::OperatorArrow),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterBraceOpen { level: 0 }),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::OperatorAddition),
            token_new!(TokenType::Identifier("y")),
            token_new!(TokenType::DelimiterBraceClose { level: 0 }),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![
                    AstNodeFunctionParameter { name: "x" },
                    AstNodeFunctionParameter { name: "y" }
                ],
                body: vec![AstNode::ReturnStatement(Box::new(AstNode::Addition(
                    Box::new(AstNode::Identifier("x")),
                    Box::new(AstNode::Identifier("y"))
                )))]
            }]
        );
    }
    #[test]
    fn parse_function_definition_with_one_parameter_and_empty_body() {
        let tokens = vec![
            token_new!(TokenType::KeywordFn),
            token_new!(TokenType::Identifier("main")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::DelimiterBraceOpen { level: 0 }),
            token_new!(TokenType::DelimiterBraceClose { level: 0 }),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![AstNodeFunctionParameter { name: "x" }],
                body: vec![]
            }]
        );
    }
    #[test]
    fn parse_function_definition_with_many_parameters_and_empty_body() {
        let tokens = vec![
            token_new!(TokenType::KeywordFn),
            token_new!(TokenType::Identifier("main")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterComma),
            token_new!(TokenType::Identifier("y")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::DelimiterBraceOpen { level: 0 }),
            token_new!(TokenType::DelimiterBraceClose { level: 0 }),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![
                    AstNodeFunctionParameter { name: "x" },
                    AstNodeFunctionParameter { name: "y" }
                ],
                body: vec![]
            }]
        );
    }
    #[test]
    fn parse_variable_definition_with_callable_attribute() {
        let tokens = vec![
            token_new!(TokenType::KeywordLet),
            token_new!(TokenType::Attribute("virtual_mem_addr")),
            token_new!(TokenType::DelimiterParenthesisOpen),
            token_new!(TokenType::LiteralInteger(0x80010000)),
            token_new!(TokenType::DelimiterParenthesisClose),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::OperatorAssignment),
            token_new!(TokenType::LiteralInteger(5)),
            token_new!(TokenType::OperatorStatementEnd),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::VariableDefinition(
                AstNodeVariableIdentifier::WithType {
                    attributes: vec![AstNodeAttribute::Callable {
                        name: "virtual_mem_addr",
                        arguments: vec![AstNode::LiteralInteger(0x80010000)]
                    }],
                    identifier_name: "x",
                    type_name: "I32"
                },
                Box::new(AstNode::LiteralInteger(5))
            )]
        );
    }
    #[test]
    fn parse_variable_definition_with_value_attribute() {
        let tokens = vec![
            token_new!(TokenType::KeywordLet),
            token_new!(TokenType::Attribute("static")),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::OperatorAssignment),
            token_new!(TokenType::LiteralInteger(5)),
            token_new!(TokenType::OperatorStatementEnd),
        ];
        let ast_nodes = parse(&tokens, Scope::Global).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::VariableDefinition(
                AstNodeVariableIdentifier::WithType {
                    attributes: vec![AstNodeAttribute::Value("static")],
                    identifier_name: "x",
                    type_name: "I32"
                },
                Box::new(AstNode::LiteralInteger(5))
            )]
        );
    }
    #[test]
    fn parse_variable_definition_with_automatic_type_deduction_and_with_integer_literal() {
        let tokens = vec![
            token_new!(TokenType::KeywordLet),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::OperatorAssignment),
            token_new!(TokenType::LiteralInteger(5)),
            token_new!(TokenType::OperatorStatementEnd),
        ];
        let ast_nodes = parse(&tokens, Scope::Function).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::VariableDefinition(
                AstNodeVariableIdentifier::WithoutType {
                    attributes: vec![],
                    identifier_name: "x"
                },
                Box::new(AstNode::LiteralInteger(5))
            )]
        );
    }
    #[test]
    fn parse_variable_definition_with_type_and_with_integer_literal() {
        let tokens = vec![
            token_new!(TokenType::KeywordLet),
            token_new!(TokenType::Identifier("x")),
            token_new!(TokenType::DelimiterColon),
            token_new!(TokenType::Identifier("I32")),
            token_new!(TokenType::OperatorAssignment),
            token_new!(TokenType::LiteralInteger(5)),
            token_new!(TokenType::OperatorStatementEnd),
        ];
        let ast_nodes = parse(&tokens, Scope::Function).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::VariableDefinition(
                AstNodeVariableIdentifier::WithType {
                    attributes: vec![],
                    identifier_name: "x",
                    type_name: "I32"
                },
                Box::new(AstNode::LiteralInteger(5))
            )]
        );
    }

    #[test]
    fn tokenize_attribute() {
        let code = "#attribute";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![token_new!(TokenType::Attribute("attribute"), 1, 0)]
        );
    }
    #[test]
    fn tokenize_attributes() {
        let code = "#attribute1 #attribute2";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::Attribute("attribute1"), 1, 0),
                token_new!(TokenType::Attribute("attribute2"), 1, 12),
            ]
        );
    }
    #[test]
    fn tokenize_expression_arithmetic_addition() {
        let code = "5 + 10";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::LiteralInteger(5), 1, 0),
                token_new!(TokenType::OperatorAddition, 1, 2),
                token_new!(TokenType::LiteralInteger(10), 1, 4),
            ]
        );
    }
    #[test]
    fn tokenize_function_without_parameters_and_with_empty_body() {
        let code = "fn main(){}";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::KeywordFn, 1, 0),
                token_new!(TokenType::Identifier("main"), 1, 3),
                token_new!(TokenType::DelimiterParenthesisOpen, 1, 7),
                token_new!(TokenType::DelimiterParenthesisClose, 1, 8),
                token_new!(TokenType::DelimiterBraceOpen { level: 0 }, 1, 9),
                token_new!(TokenType::DelimiterBraceClose { level: 0 }, 1, 10),
            ]
        );
    }
    #[test]
    fn tokenize_literals_boolean() {
        let code = "true false";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::LiteralBooleanTrue, 1, 0),
                token_new!(TokenType::LiteralBooleanFalse, 1, 5),
            ]
        );
    }
    #[test]
    fn tokenize_literals_integers_as_decimal() {
        let code = "5 10 200 1000";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::LiteralInteger(5), 1, 0),
                token_new!(TokenType::LiteralInteger(10), 1, 2),
                token_new!(TokenType::LiteralInteger(200), 1, 5),
                token_new!(TokenType::LiteralInteger(1000), 1, 9),
            ]
        );
    }
    #[test]
    fn tokenize_literals_integers_as_hexadecimal() {
        let code = "0x5 0xA 0xC8 0x3E8"; // 5 10 200 1000 in decimal.
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::LiteralInteger(5), 1, 0),
                token_new!(TokenType::LiteralInteger(10), 1, 4),
                token_new!(TokenType::LiteralInteger(200), 1, 8),
                token_new!(TokenType::LiteralInteger(1000), 1, 13),
            ]
        );
    }
    #[test]
    fn tokenize_operators_arithmetic() {
        // All together
        let code = "+-*/";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::OperatorAddition, 1, 0),
                token_new!(TokenType::OperatorSubtraction, 1, 1),
                token_new!(TokenType::OperatorMultiplication, 1, 2),
                token_new!(TokenType::OperatorDivision, 1, 3),
            ]
        );

        // Seperated by spaces
        let code = "+ - * /";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::OperatorAddition, 1, 0),
                token_new!(TokenType::OperatorSubtraction, 1, 2),
                token_new!(TokenType::OperatorMultiplication, 1, 4),
                token_new!(TokenType::OperatorDivision, 1, 6),
            ]
        );
    }
    #[test]
    fn tokenize_variable_definition_with_automatic_type_deduction() {
        let code = "let x = 5;";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                token_new!(TokenType::KeywordLet, 1, 0),
                token_new!(TokenType::Identifier("x"), 1, 4),
                token_new!(TokenType::OperatorAssignment, 1, 6),
                token_new!(TokenType::LiteralInteger(5), 1, 8),
                token_new!(TokenType::OperatorStatementEnd, 1, 9),
            ]
        );
    }
}
