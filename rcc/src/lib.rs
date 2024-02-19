#[derive(Debug, PartialEq)]
pub enum AstNode<'a> {
    Addition(Box<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionDefinition {
        name: &'a str,
        parameters: Vec<AstNodeFunctionParameter<'a>>,
        body: Vec<AstNode<'a>>,
    },
    Identifier(&'a str),
    LiteralInteger(i64),
    ReturnStatement(Box<AstNode<'a>>),
    VariableDefinition(&'a str, Box<AstNode<'a>>),
}
#[derive(Debug, PartialEq)]
pub struct AstNodeFunctionParameter<'a> {
    name: &'a str,
}

const KEYWORD_FN: &str = "fn";
const KEYWORD_LET: &str = "let";
const KEYWORD_FALSE: &str = "false";
const KEYWORD_TRUE: &str = "true";

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    DelimiterBraceClose { level: usize },
    DelimiterBraceOpen { level: usize },
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
    OperatorArrow,
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

    'main_parse_loop: while let Some(token) = tokens.next() {
        match token {
            Token::KeywordFn => {
                if let Some(Token::Identifier(name)) = tokens.peek() {
                    tokens.next();

                    if let Some(Token::DelimiterParenthesisOpen) = tokens.peek() {
                        tokens.next();

                        let mut parameters = vec![];
                        let mut body = vec![];

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

                        // If there is a return type for the function after the parameters
                        if let Some(Token::OperatorArrow) = tokens.peek() {
                            tokens.next();
                            tokens.next(); // For now, ignore the return type of the function.
                        }

                        // Expect a function body after the parameters and possible return type
                        if let Some(Token::DelimiterBraceOpen { level }) = tokens.peek() {
                            tokens.next();

                            let function_body_opening_level = *level;
                            let mut prev_value_token = None;

                            while let Some(next_token) = tokens.next() {
                                match next_token {
                                    Token::DelimiterBraceClose { level }
                                        if *level == function_body_opening_level =>
                                    {
                                        // If encounted an integer literal right before the closing brace,
                                        // then it is the return value of the function as in Rust.
                                        // In Rust, the last expression in a function (no semicolon after the value) is the return value.
                                        if let Some(prev_value_token) = prev_value_token {
                                            body.push(AstNode::ReturnStatement(Box::new(
                                                prev_value_token,
                                            )));
                                        }

                                        ast_nodes.push(AstNode::FunctionDefinition {
                                            name,
                                            parameters,
                                            body,
                                        });
                                        continue 'main_parse_loop;
                                    }
                                    Token::Identifier(name) => {
                                        // Expect an addition to follow this identifier token.
                                        if let Some(Token::OperatorAddition) = tokens.peek() {
                                            tokens.next();

                                            if let Some(Token::Identifier(name2)) = tokens.peek() {
                                                tokens.next();

                                                prev_value_token = Some(AstNode::Addition(
                                                    Box::new(AstNode::Identifier(name)),
                                                    Box::new(AstNode::Identifier(name2)),
                                                ));
                                            } else {
                                                return Err(format!(
                                                    "Expected identifier after addition (+). Instead, found: {:?}",
                                                    tokens.peek()
                                                ));
                                            }
                                        } else {
                                            return Err(format!(
                                                "Expected addition (+) after identifier. Instead, found: {:?}",
                                                tokens.peek()
                                            ));
                                        }
                                    }
                                    Token::LiteralInteger(value) => {
                                        prev_value_token = Some(AstNode::LiteralInteger(*value));
                                    }
                                    _ => {
                                        prev_value_token = None;
                                    }
                                }
                            }
                        } else {
                            return Err("Expected opening brace after function parameters.".into());
                        }
                    } else {
                        return Err("Expected opening parenthesis after function name. Instead, no opening parenthesis found.".into());
                    }
                } else {
                    return Err(format!(
                        "Expected function name after '{KEYWORD_FN}' keyword. Instead, no identifier for function name found."
                    ));
                }
            }
            Token::KeywordLet => {
                if let Some(Token::Identifier(name)) = tokens.peek() {
                    tokens.next();

                    if let Some(Token::OperatorAssignment) = tokens.peek() {
                        tokens.next();

                        if let Some(Token::LiteralInteger(value)) = tokens.peek() {
                            tokens.next();
                            if let Some(Token::OperatorStatementEnd) = tokens.peek() {
                                tokens.next();
                                ast_nodes.push(AstNode::VariableDefinition(
                                    name,
                                    Box::new(AstNode::LiteralInteger(*value)),
                                ));
                            } else {
                                return Err(format!(
                                    "Expected statement end after variable definition. Instead, found: {:?}",
                                    tokens.peek()
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected integer literal after assignment operator. Instead, found: {:?}",
                                tokens.peek()
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Expected assignment operator after variable name. Instead, found: {:?}",
                            tokens.peek()
                        ));
                    }
                } else {
                    return Err(format!(
                        "Expected variable name after '{KEYWORD_LET}' keyword. Instead, no identifier for variable name found."
                    ));
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
    let mut brace_level = 0usize;
    let mut tokens = vec![];

    while let Some((i, c)) = chars.next() {
        match c {
            '}' => {
                if brace_level == 0 {
                    return Err("Unexpected closing brace. There are too many closing braces compared to opening braces.".into());
                }
                brace_level -= 1;
                tokens.push(Token::DelimiterBraceClose { level: brace_level });
            }
            '{' => {
                tokens.push(Token::DelimiterBraceOpen { level: brace_level });
                brace_level += 1;
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
                if let Some((_, '>')) = chars.peek() {
                    chars.next();
                    tokens.push(Token::OperatorArrow);
                } else {
                    tokens.push(Token::OperatorSubtraction);
                }
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
                    let mut identifier_or_keyword_index_end = i;

                    while let Some((_, next_c)) = chars.peek() {
                        identifier_or_keyword_index_end += 1;
                        if next_c.is_alphanumeric() {
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    match &code[i..identifier_or_keyword_index_end] {
                        KEYWORD_FN => tokens.push(Token::KeywordFn),
                        KEYWORD_LET => tokens.push(Token::KeywordLet),
                        KEYWORD_TRUE => tokens.push(Token::LiteralBooleanTrue),
                        KEYWORD_FALSE => tokens.push(Token::LiteralBooleanFalse),
                        _ => {
                            let identifier = &code[i..identifier_or_keyword_index_end];
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
            Token::DelimiterBraceOpen { level: 0 },
            Token::DelimiterBraceClose { level: 0 },
        ];
        let ast_nodes = parse(&tokens).unwrap();
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
            Token::KeywordFn,
            Token::Identifier("main"),
            Token::DelimiterParenthesisOpen,
            Token::DelimiterParenthesisClose,
            Token::OperatorArrow,
            Token::Identifier("I32"),
            Token::DelimiterBraceOpen { level: 0 },
            Token::LiteralInteger(5),
            Token::DelimiterBraceClose { level: 0 },
        ];
        let ast_nodes = parse(&tokens).unwrap();
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
            Token::OperatorArrow,
            Token::Identifier("I32"),
            Token::DelimiterBraceOpen { level: 0 },
            Token::Identifier("x"),
            Token::OperatorAddition,
            Token::Identifier("y"),
            Token::DelimiterBraceClose { level: 0 },
        ];
        let ast_nodes = parse(&tokens).unwrap();
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
            Token::KeywordFn,
            Token::Identifier("main"),
            Token::DelimiterParenthesisOpen,
            Token::Identifier("x"),
            Token::DelimiterColon,
            Token::Identifier("I32"),
            Token::DelimiterParenthesisClose,
            Token::DelimiterBraceOpen { level: 0 },
            Token::DelimiterBraceClose { level: 0 },
        ];
        let ast_nodes = parse(&tokens).unwrap();
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
            Token::DelimiterBraceOpen { level: 0 },
            Token::DelimiterBraceClose { level: 0 },
        ];
        let ast_nodes = parse(&tokens).unwrap();
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
    fn parse_variable_definition_with_integer_literal() {
        let tokens = vec![
            Token::KeywordLet,
            Token::Identifier("x"),
            Token::OperatorAssignment,
            Token::LiteralInteger(5),
            Token::OperatorStatementEnd,
        ];
        let ast_nodes = parse(&tokens).unwrap();
        assert_eq!(
            ast_nodes,
            vec![AstNode::VariableDefinition(
                "x",
                Box::new(AstNode::LiteralInteger(5))
            )]
        );
    }

    #[test]
    fn tokenize_expression_arithmetic_addition() {
        let code = "5 + 10";
        let tokens = tokenize(code).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LiteralInteger(5),
                Token::OperatorAddition,
                Token::LiteralInteger(10)
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
                Token::KeywordFn,
                Token::Identifier("main"),
                Token::DelimiterParenthesisOpen,
                Token::DelimiterParenthesisClose,
                Token::DelimiterBraceOpen { level: 0 },
                Token::DelimiterBraceClose { level: 0 }
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
    fn tokenize_literals_integers() {
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
    fn tokenize_operators_arithmetic() {
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
    fn tokenize_variable_definition_with_automatic_type_deduction() {
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
