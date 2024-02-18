#[derive(Debug, PartialEq)]
pub enum AstNode<'a> {
    FunctionDefinition {
        name: &'a str,
        parameters: Vec<AstNodeFunctionParameter<'a>>,
    },
}
#[derive(Debug, PartialEq)]
pub struct AstNodeFunctionParameter<'a> {
    name: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    DelimiterBraceClose,
    DelimiterBraceOpen,
    DelimiterColon,
    DelimiterComma,
    DelimiterParenthesisClose,
    DelimiterParenthesisOpen,
    Identifier(&'a str),
    LiteralBooleanFalse,
    LiteralBooleanTrue,
    LiteralInteger(i64),
    KeywordFn,
    KeywordLet,
    OperatorAssignment,
    OperatorAddition,
    OperatorDivision,
    OperatorMultiplication,
    OperatorSubtraction,
    OperatorStatementEnd,
}

pub fn parse<'a>(tokens: &Vec<Token<'a>>) -> Result<Vec<AstNode<'a>>, String> {
    let mut tokens = tokens.iter().peekable();

    let mut ast_nodes = vec![];

    while let Some(token) = tokens.next() {
        match token {
            Token::KeywordFn => {
                if let Some(Token::Identifier(name)) = tokens.peek() {
                    tokens.next();

                    if let Some(Token::DelimiterParenthesisOpen) = tokens.peek() {
                        tokens.next();

                        let mut parameters = vec![];

                        let mut expect_parameter_type = false;
                        let mut parameter_name_token = None;

                        'parameter_parsing: while let Some(next_token) = tokens.peek() {
                            match next_token {
                                Token::DelimiterColon => {
                                    if parameter_name_token.is_some() {
                                        expect_parameter_type = true;
                                        tokens.next();
                                    } else {
                                        return Err(format!(
                                            "Expected parameter name before comma. Instead, found: {:?}",
                                            next_token
                                        ));
                                    }
                                }
                                Token::DelimiterComma => {
                                    tokens.next();
                                }
                                Token::DelimiterParenthesisClose => {
                                    tokens.next();
                                    break 'parameter_parsing;
                                }
                                Token::Identifier(parameter_name) => {
                                    match parameter_name_token {
                                        Some(parameter_name) => {
                                            if expect_parameter_type {
                                                expect_parameter_type = false;
                                                parameters.push(AstNodeFunctionParameter {
                                                    name: parameter_name,
                                                });
                                                parameter_name_token = None;
                                            } else {
                                                return Err(format!(
                                                    "Expected comma after parameter name. Instead, found: {:?}",
                                                    next_token
                                                )
                                                .into());
                                            }
                                        }
                                        None => {
                                            parameter_name_token = Some(parameter_name);
                                        }
                                    }

                                    tokens.next();
                                }
                                _ => {
                                    return Err(format!(
                                        "Expected parameter name or closing parenthesis after opening parenthesis. Instead, found: {:?}",
                                        next_token
                                    ));
                                }
                            }
                        }

                        ast_nodes.push(AstNode::FunctionDefinition { name, parameters });
                    } else {
                        return Err("Expected opening parenthesis after function name. Instead, no opening parenthesis found.".into());
                    }
                } else {
                    return Err("Expected function name after 'fn' keyword. Instead, no identifier for function name found.".into());
                }
            }
            _ => {}
        }
    }

    Ok(ast_nodes)
}
/// Parses code in form of a string into a sequence of tokens.
pub fn tokenize(code: &str) -> Result<Vec<Token>, String> {
    // Add an extra space character to the end of the input string iterator
    // to simplify the tokenization logic. The logic below relies on peeking
    // at the next character. If there are no characters left, there is nothing
    // to peek at. By adding an extra space character, we can always peek at
    // the next character without having to check if there are any characters left.
    // Also, the extra space character does not result in any extra tokens.
    // This simplifies the logic in certain places.
    let chars = code.chars().chain(std::iter::once(' '));
    let mut chars = chars.enumerate().peekable();

    let mut tokens = vec![];

    'main_token_loop: while let Some((i, c)) = chars.next() {
        match c {
            '}' => {
                tokens.push(Token::DelimiterBraceClose);
            }
            '{' => {
                tokens.push(Token::DelimiterBraceOpen);
            }
            ':' => {
                tokens.push(Token::DelimiterColon);
            }
            ',' => {
                tokens.push(Token::DelimiterComma);
            }
            ')' => {
                tokens.push(Token::DelimiterParenthesisClose);
            }
            '(' => {
                tokens.push(Token::DelimiterParenthesisOpen);
            }
            '=' => {
                tokens.push(Token::OperatorAssignment);
            }
            '+' => {
                tokens.push(Token::OperatorAddition);
            }
            '/' => {
                tokens.push(Token::OperatorDivision);
            }
            '*' => {
                tokens.push(Token::OperatorMultiplication);
            }
            '-' => {
                tokens.push(Token::OperatorSubtraction);
            }
            ';' => {
                tokens.push(Token::OperatorStatementEnd);
            }
            ' ' => {}
            _ => {
                if c.is_digit(10) {
                    let mut value_index_end = i;

                    while let Some((_, next_c)) = chars.peek() {
                        value_index_end += 1;
                        if next_c.is_digit(10) {
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    let value = if i == value_index_end {
                        &code[i..=value_index_end]
                    } else {
                        &code[i..value_index_end]
                    };

                    let value = value.parse::<i64>().map_err(|e| e.to_string())?;
                    tokens.push(Token::LiteralInteger(value));
                } else if c.is_alphabetic() {
                    // In this state, we expect an identifier or a keyword.
                    let mut identifier_index_end = i;

                    while let Some((_, next_c)) = chars.peek() {
                        identifier_index_end += 1;
                        if next_c.is_alphanumeric() {
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    match &code[i..identifier_index_end] {
                        "fn" => tokens.push(Token::KeywordFn),
                        "let" => tokens.push(Token::KeywordLet),
                        "true" => tokens.push(Token::LiteralBooleanTrue),
                        "false" => tokens.push(Token::LiteralBooleanFalse),
                        _ => {
                            let identifier = &code[i..identifier_index_end];
                            tokens.push(Token::Identifier(identifier));
                        }
                    }
                }
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_function_definition_without_parameters_and_empty_body() {
        let tokens = vec![
            Token::KeywordFn,
            Token::Identifier("main"),
            Token::DelimiterParenthesisOpen,
            Token::DelimiterParenthesisClose,
            Token::DelimiterBraceOpen,
            Token::DelimiterBraceClose,
        ];
        let ast_nodes = parse(&tokens).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![]
            }]
        );
    }
    #[test]
    fn parse_function_definition_with_one_parameter_and_empty_body() {
        let tokens = vec![
            Token::KeywordFn,
            Token::Identifier("main"),
            Token::DelimiterParenthesisOpen,
            Token::Identifier("x"),
            Token::DelimiterColon,
            Token::Identifier("I32"),
            Token::DelimiterParenthesisClose,
            Token::DelimiterBraceOpen,
            Token::DelimiterBraceClose,
        ];
        let ast_nodes = parse(&tokens).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![AstNodeFunctionParameter { name: "x" }]
            }]
        );
    }
    #[test]
    fn parse_function_definition_with_many_parameters_and_empty_body() {
        let tokens = vec![
            Token::KeywordFn,
            Token::Identifier("main"),
            Token::DelimiterParenthesisOpen,
            Token::Identifier("x"),
            Token::DelimiterColon,
            Token::Identifier("I32"),
            Token::DelimiterComma,
            Token::Identifier("y"),
            Token::DelimiterColon,
            Token::Identifier("I32"),
            Token::DelimiterParenthesisClose,
            Token::DelimiterBraceOpen,
            Token::DelimiterBraceClose,
        ];
        let ast_nodes = parse(&tokens).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::FunctionDefinition {
                name: "main",
                parameters: vec![
                    AstNodeFunctionParameter { name: "x" },
                    AstNodeFunctionParameter { name: "y" }
                ]
            }]
        );
    }

    #[test]
    fn tokenize_function_without_parameters_and_with_empty_body() {
        let code = "fn main(){}";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::KeywordFn,
                Token::Identifier("main"),
                Token::DelimiterParenthesisOpen,
                Token::DelimiterParenthesisClose,
                Token::DelimiterBraceOpen,
                Token::DelimiterBraceClose
            ]
        );
    }
    #[test]
    fn tokenize_literals_boolean() {
        let code = "true false";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![Token::LiteralBooleanTrue, Token::LiteralBooleanFalse]
        );
    }
    #[test]
    fn tokenize_literal_integers() {
        let code = "5 10 200 1000";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LiteralInteger(5),
                Token::LiteralInteger(10),
                Token::LiteralInteger(200),
                Token::LiteralInteger(1000)
            ]
        );
    }
    #[test]
    fn tokenize_operator_arithmetic() {
        // All together
        let code = "+-*/";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::OperatorAddition,
                Token::OperatorSubtraction,
                Token::OperatorMultiplication,
                Token::OperatorDivision
            ]
        );

        // Seperated by spaces
        let code = "+ - * /";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::OperatorAddition,
                Token::OperatorSubtraction,
                Token::OperatorMultiplication,
                Token::OperatorDivision
            ]
        );
    }
    #[test]
    fn tokenize_variable_definition() {
        let code = "let x = 5;";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::KeywordLet,
                Token::Identifier("x"),
                Token::OperatorAssignment,
                Token::LiteralInteger(5),
                Token::OperatorStatementEnd
            ]
        );
    }
}
