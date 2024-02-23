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
    VariableDefinition(AstNodeVariableIdentifier<'a>, Box<AstNode<'a>>),
}
#[derive(Debug, PartialEq)]
pub struct AstNodeFunctionParameter<'a> {
    name: &'a str,
}
#[derive(Debug, PartialEq)]
pub enum AstNodeVariableIdentifier<'a> {
    WithType {
        attributes: Vec<&'a str>,
        identifier_name: &'a str,
        type_name: &'a str,
    },
    WithoutType {
        attributes: Vec<&'a str>,
        identifier_name: &'a str,
    },
}

#[derive(Debug)]
pub struct ParseError {
    line: usize,
    col: usize,
    pub message: String,
}
impl ParseError {
    #[inline]
    pub fn position(&self) -> String {
        format!("{}:{}", self.line, self.col)
    }
}

#[derive(Debug, PartialEq)]
pub enum Scope {
    Global,
    Function,
}

const KEYWORD_FN: &str = "fn";
const KEYWORD_LET: &str = "let";
const KEYWORD_FALSE: &str = "false";
const KEYWORD_TRUE: &str = "true";

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    col: usize,
    line: usize,
    type_: TokenType<'a>,
}
#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
    Attribute(&'a str),
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

type PeekableTokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Token<'a>>>;

/// Creates a parse error with the given token and message.
/// It also includes source code file and line information for debugging purposes.
macro_rules! create_parse_error {
    ($token: expr, $message: expr) => {
        ParseError {
            line: $token.line,
            col: $token.col,
            message: $message
                + &format!(
                    "\nError in source code occured in \"{}:{}\"",
                    file!(),
                    line!()
                ),
        }
    };
}

pub fn parse<'a>(tokens: &'a Vec<Token<'a>>, scope: Scope) -> Result<Vec<AstNode<'a>>, ParseError> {
    let mut tokens: PeekableTokenIter<'a> = tokens.iter().peekable();

    let mut ast_nodes = vec![];

    'main_parse_loop: while let Some(token) = tokens.next() {
        match token.type_ {
            TokenType::KeywordFn => {
                if let Some(Token {
                    type_: TokenType::Identifier(name),
                    ..
                }) = tokens.peek()
                {
                    // Safety: Peeking for the identifier was successful before. Therefore, getting it here is safe.
                    let identifier_token = tokens.next().unwrap();

                    if let Some(Token {
                        type_: TokenType::DelimiterParenthesisOpen,
                        ..
                    }) = tokens.peek()
                    {
                        tokens.next();

                        let mut parameters = vec![];
                        let mut body = vec![];

                        let mut expect_parameter_type = false;
                        let mut parameter_name_token = None;

                        'parameter_parsing: while let Some(next_token) = tokens.peek() {
                            match next_token.type_ {
                                TokenType::DelimiterColon => {
                                    if parameter_name_token.is_some() {
                                        expect_parameter_type = true;
                                        tokens.next();
                                    } else {
                                        return Err(create_parse_error!(
                                            next_token,
                                            format!(
                                                "Expected parameter name before colon. Instead, found: {:?}",
                                                next_token
                                            )
                                        ));
                                    }
                                }
                                TokenType::DelimiterComma => {
                                    tokens.next();
                                }
                                TokenType::DelimiterParenthesisClose => {
                                    tokens.next();
                                    break 'parameter_parsing;
                                }
                                TokenType::Identifier(parameter_name) => {
                                    match parameter_name_token {
                                        Some(parameter_name) => {
                                            if expect_parameter_type {
                                                expect_parameter_type = false;
                                                parameters.push(AstNodeFunctionParameter {
                                                    name: parameter_name,
                                                });
                                                parameter_name_token = None;
                                            } else {
                                                return Err(create_parse_error!(
                                                    next_token,
                                                    format!(
                                                        "Expected colon after parameter name. Instead, found: {:?}",
                                                        next_token
                                                    )
                                                ));
                                            }
                                        }
                                        None => {
                                            parameter_name_token = Some(parameter_name);
                                        }
                                    }

                                    tokens.next();
                                }
                                _ => {
                                    return Err(create_parse_error!(
                                        next_token,
                                        format!(
                                            "Expected parameter name or closing parenthesis after opening parenthesis. Instead, found: {:?}",
                                            next_token
                                        )
                                    ));
                                }
                            }
                        }

                        // If there is a return type for the function after the parameters
                        if let Some(Token {
                            type_: TokenType::OperatorArrow,
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();
                            tokens.next(); // For now, ignore the return type of the function.
                        }

                        // Expect a function body after the parameters and possible return type
                        if let Some(Token {
                            type_: TokenType::DelimiterBraceOpen { level },
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();

                            let function_body_opening_level = *level;
                            let mut prev_value_token = None;

                            while let Some(next_token) = tokens.next() {
                                match next_token.type_ {
                                    TokenType::DelimiterBraceClose { level }
                                        if level == function_body_opening_level =>
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
                                    TokenType::Identifier(name) => {
                                        // Expect an addition to follow this identifier token.
                                        if let Some(Token {
                                            type_: TokenType::OperatorAddition,
                                            ..
                                        }) = tokens.peek()
                                        {
                                            tokens.next();

                                            if let Some(Token {
                                                type_: TokenType::Identifier(name2),
                                                ..
                                            }) = tokens.peek()
                                            {
                                                tokens.next();

                                                prev_value_token = Some(AstNode::Addition(
                                                    Box::new(AstNode::Identifier(name)),
                                                    Box::new(AstNode::Identifier(name2)),
                                                ));
                                            } else {
                                                return Err(create_parse_error!(
                                                    next_token,
                                                    format!(
                                                        "Expected identifier after addition (+). Instead, found: {:?}",
                                                        tokens.peek()
                                                    )
                                                ));
                                            }
                                        } else {
                                            return Err(create_parse_error!(
                                                next_token,
                                                format!(
                                                    "Expected addition (+) after identifier. Instead, found: {:?}",
                                                    tokens.peek()
                                                )
                                            ));
                                        }
                                    }
                                    TokenType::LiteralInteger(value) => {
                                        prev_value_token = Some(AstNode::LiteralInteger(value));
                                    }
                                    _ => {
                                        prev_value_token = None;
                                    }
                                }
                            }
                        } else {
                            return Err(create_parse_error!(
                                identifier_token,
                                format!(
                                    "Expected opening brace after function parameters. Instead, found: {:?}",
                                    tokens.peek()
                                )
                            ));
                        }
                    } else {
                        return Err(create_parse_error!(
                            identifier_token,
                            String::from(
                                "Expected opening parenthesis after function name. Instead, no opening parenthesis found."
                            )
                        ));
                    }
                } else {
                    return Err(create_parse_error!(
                        token,
                        format!(
                            "Expected function name after '{KEYWORD_FN}' keyword. Instead, no identifier for function name found."
                        )
                    ));
                }
            }
            TokenType::KeywordLet => {
                match parse_variable_definition(tokens.clone(), token, &scope) {
                    Ok((new_ast_nodes, new_tokens)) => {
                        ast_nodes.extend(new_ast_nodes);
                        tokens = new_tokens;
                    }
                    Err(parse_error) => {
                        return Err(parse_error);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(ast_nodes)
}
fn parse_variable_definition<'a>(
    mut tokens: PeekableTokenIter<'a>,
    token: &Token<'_>,
    scope: &Scope,
) -> Result<(Vec<AstNode<'a>>, PeekableTokenIter<'a>), ParseError> {
    // Explicit type for vec here, because otherwise attributes would have type &&str automatically deduced instead of &str.
    let mut attributes = Vec::<&str>::new();
    let mut ast_nodes = vec![];

    'variable_defition_parsing_loop: loop {
        match tokens.peek() {
            Some(Token {
                type_: TokenType::Attribute(name),
                ..
            }) => {
                tokens.next();
                attributes.push(name);

                while let Some(Token {
                    type_: TokenType::Attribute(name),
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                    attributes.push(name);
                }

                // Ensure that static variables are only allowed in the global scope.
                if *scope != Scope::Global && attributes.contains(&"static") {
                    return Err(create_parse_error!(
                        token,
                        format!(
                            "Static variables are only allowed in the global scope, not in this scope."
                        )
                    ));
                }

                continue 'variable_defition_parsing_loop;
            }
            Some(Token {
                type_: TokenType::Identifier(name),
                ..
            }) => {
                tokens.next();

                match tokens.peek() {
                    Some(Token {
                        type_: TokenType::DelimiterColon,
                        ..
                    }) => {
                        tokens.next();

                        let variable_type_name = if let Some(Token {
                            type_: TokenType::Identifier(type_name),
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();
                            type_name
                        } else {
                            return Err(create_parse_error!(
                                token,
                                format!("Expected type after colon.",)
                            ));
                        };

                        if let Some(Token {
                            type_: TokenType::OperatorAssignment,
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();

                            if let Some(Token {
                                type_: TokenType::LiteralInteger(value),
                                ..
                            }) = tokens.peek()
                            {
                                tokens.next();

                                if let Some(Token {
                                    type_: TokenType::OperatorStatementEnd,
                                    ..
                                }) = tokens.peek()
                                {
                                    tokens.next();
                                    ast_nodes.push(AstNode::VariableDefinition(
                                        AstNodeVariableIdentifier::WithType {
                                            attributes,
                                            identifier_name: name,
                                            type_name: variable_type_name,
                                        },
                                        Box::new(AstNode::LiteralInteger(*value)),
                                    ));
                                } else {
                                    return Err(create_parse_error!(
                                        token,
                                        format!(
                                            "Expected statement end after variable definition.",
                                        )
                                    ));
                                }
                            } else {
                                return Err(create_parse_error!(
                                    token,
                                    format!("Expected integer literal after assignment operator.",)
                                ));
                            }
                        } else {
                            return Err(create_parse_error!(
                                token,
                                format!("Expected assignment operator after variable name.",)
                            ));
                        }
                    }
                    Some(Token {
                        type_: TokenType::OperatorAssignment,
                        ..
                    }) => {
                        tokens.next();

                        if let Some(Token {
                            type_: TokenType::LiteralInteger(value),
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();

                            if let Some(Token {
                                type_: TokenType::OperatorStatementEnd,
                                ..
                            }) = tokens.peek()
                            {
                                tokens.next();
                                ast_nodes.push(AstNode::VariableDefinition(
                                    AstNodeVariableIdentifier::WithoutType {
                                        attributes: vec![],
                                        identifier_name: name,
                                    },
                                    Box::new(AstNode::LiteralInteger(*value)),
                                ));
                            } else {
                                return Err(create_parse_error!(
                                    token,
                                    format!("Expected statement end after variable definition.",)
                                ));
                            }
                        } else {
                            return Err(create_parse_error!(
                                token,
                                format!("Expected integer literal after assignment operator.",)
                            ));
                        }
                    }
                    _ => {
                        return Err(create_parse_error!(
                            token,
                            format!(
                                "Expected type or assignment operator after variable name. Instead, found: {:?}",
                                tokens.peek()
                            )
                        ));
                    }
                }

                break;
            }
            _ => {
                return Err(create_parse_error!(
                    token,
                    format!(
                        "Expected variable name after '{KEYWORD_LET}' keyword. Instead, no identifier for variable name found."
                    )
                ));
            }
        }
    }

    Ok((ast_nodes, tokens))
}

/// Parses code in form of a string into a sequence of tokens.
pub fn tokenize<'a>(code: &str) -> Result<Vec<Token>, String> {
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
    let mut current_col = 0usize;
    let mut current_line = 1usize;
    let mut tokens = vec![];

    while let Some((i, c)) = chars.next() {
        match c {
            '}' => {
                if brace_level == 0 {
                    return Err("Unexpected closing brace. There are too many closing braces compared to opening braces.".into());
                }
                brace_level -= 1;
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterBraceClose { level: brace_level },
                });
                current_col += 1;
            }
            '{' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterBraceOpen { level: brace_level },
                });
                brace_level += 1;
                current_col += 1;
            }
            ':' => {
                current_col += 1;
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterColon,
                });
            }
            ',' => {
                current_col += 1;
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterComma,
                });
            }
            ')' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterParenthesisClose,
                });
                current_col += 1;
            }
            '(' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::DelimiterParenthesisOpen,
                });
                current_col += 1;
            }
            '=' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::OperatorAssignment,
                });
                current_col += 1;
            }
            '+' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::OperatorAddition,
                });
                current_col += 1;
            }
            '/' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::OperatorDivision,
                });
                current_col += 1;
            }
            '*' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::OperatorMultiplication,
                });
                current_col += 1;
            }
            '-' => {
                if let Some((_, '>')) = chars.peek() {
                    chars.next();
                    tokens.push(Token {
                        col: current_col,
                        line: current_line,
                        type_: TokenType::OperatorArrow,
                    });
                    current_col += 2;
                } else {
                    tokens.push(Token {
                        col: current_col,
                        line: current_line,
                        type_: TokenType::OperatorSubtraction,
                    });
                    current_col += 1;
                }
            }
            ';' => {
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::OperatorStatementEnd,
                });
                current_col += 1;
            }
            ' ' => {
                current_col += 1;
            }
            '\n' => {
                current_col = 0;
                current_line += 1;
            }
            '#' => {
                // Skip this character, because it is not stored at part of the attribute token.
                chars.next();

                let attribute_index_begin = i + 1;
                let mut attribute_index_end = attribute_index_begin;

                while let Some((j, next_c)) = chars.peek() {
                    attribute_index_end += 1;
                    // If the first character of the attribute is a letter or an underscore
                    if *j == attribute_index_begin && next_c.is_alphabetic() || *next_c == '_' {
                        chars.next();
                    }
                    // If any character following the first character is a letter, a digit or an underscore
                    else if *j > attribute_index_begin && next_c.is_alphanumeric()
                        || *next_c == '_'
                    {
                        chars.next();
                    }
                    // If no valid character found for an attribute name
                    else {
                        break;
                    }
                }

                let attribute = &code[attribute_index_begin..attribute_index_end];
                tokens.push(Token {
                    col: current_col,
                    line: current_line,
                    type_: TokenType::Attribute(attribute),
                });
                current_col += attribute_index_end - attribute_index_begin + 1; // + 1 for the '#' character.
            }
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
                    tokens.push(Token {
                        col: current_col,
                        line: current_line,
                        type_: TokenType::LiteralInteger(value),
                    });
                    current_col += value_index_end - i;
                } else if c.is_alphabetic() || c == '_' {
                    // In this state, we expect an identifier or a keyword.
                    let mut identifier_or_keyword_index_end = i;

                    while let Some((_, next_c)) = chars.peek() {
                        identifier_or_keyword_index_end += 1;
                        if next_c.is_alphanumeric() || *next_c == '_' {
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    match &code[i..identifier_or_keyword_index_end] {
                        KEYWORD_FN => {
                            tokens.push(Token {
                                col: current_col,
                                line: current_line,
                                type_: TokenType::KeywordFn,
                            });
                            current_col += KEYWORD_FN.len();
                        }
                        KEYWORD_LET => {
                            tokens.push(Token {
                                col: current_col,
                                line: current_line,
                                type_: TokenType::KeywordLet,
                            });
                            current_col += KEYWORD_LET.len();
                        }
                        KEYWORD_TRUE => {
                            tokens.push(Token {
                                col: current_col,
                                line: current_line,
                                type_: TokenType::LiteralBooleanTrue,
                            });
                            current_col += KEYWORD_TRUE.len();
                        }
                        KEYWORD_FALSE => {
                            tokens.push(Token {
                                col: current_col,
                                line: current_line,
                                type_: TokenType::LiteralBooleanFalse,
                            });
                            current_col += KEYWORD_FALSE.len();
                        }
                        _ => {
                            let identifier = &code[i..identifier_or_keyword_index_end];
                            tokens.push(Token {
                                col: current_col,
                                line: current_line,
                                type_: TokenType::Identifier(identifier),
                            });
                            current_col += identifier_or_keyword_index_end - i;
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
    fn parse_variable_definition_with_attributes() {
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
                    attributes: vec!["static"],
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
    fn tokenize_literals_integers() {
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
