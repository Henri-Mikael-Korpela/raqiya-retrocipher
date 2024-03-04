#[derive(Clone, Debug, PartialEq)]
pub enum AstNode<'a> {
    Addition(Box<AstNode<'a>>, Box<AstNode<'a>>),
    BinaryAnd(Box<AstNode<'a>>, Box<AstNode<'a>>),
    Division(Box<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionDefinition {
        attributes: Vec<AstNodeAttribute<'a>>,
        name: &'a str,
        parameters: Vec<AstNodeFunctionParameter<'a>>,
        body: Vec<AstNode<'a>>,
    },
    Identifier(&'a str),
    LiteralInteger(i64),
    LiteralString(&'a str),
    Multiplication(Box<AstNode<'a>>, Box<AstNode<'a>>),
    ReturnStatement(Box<AstNode<'a>>),
    Substraction(Box<AstNode<'a>>, Box<AstNode<'a>>),
    VariableDefinition(AstNodeVariableIdentifier<'a>, Box<AstNode<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstNodeAttribute<'a> {
    Callable {
        name: &'a str,
        arguments: Vec<AstNode<'a>>,
    },
    Value(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstNodeFunctionParameter<'a> {
    name: &'a str,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstNodeVariableIdentifier<'a> {
    WithType {
        attributes: Vec<AstNodeAttribute<'a>>,
        identifier_name: &'a str,
        type_name: &'a str,
    },
    WithoutType {
        attributes: Vec<AstNodeAttribute<'a>>,
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
#[derive(Clone, Debug, PartialEq)]
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
    LiteralString(&'a str),
    KeywordFn,
    KeywordLet,
    OperatorArrow,
    OperatorAssignment,
    OperatorAddition,
    OperatorBinaryAnd,
    OperatorDivision,
    OperatorMultiplication,
    OperatorSubstraction,
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
                let mut attributes = vec![];

                while let Some(token) = tokens.peek() {
                    match token.type_ {
                        TokenType::Attribute(name) => {
                            tokens.next();

                            (tokens, attributes) = match parse_attributes(tokens, name) {
                                Ok((attributes, new_tokens)) => (new_tokens, attributes),
                                Err(parse_error) => {
                                    return Err(parse_error);
                                }
                            };
                        }
                        TokenType::Identifier(name) => {
                            let identifier_token = tokens.next().unwrap();

                            if let Some(Token {
                                type_: TokenType::DelimiterParenthesisOpen,
                                ..
                            }) = tokens.peek()
                            {
                                tokens.next();

                                // Parse function parameters
                                let parameters: Vec<AstNodeFunctionParameter<'_>>;
                                (parameters, tokens) = match parse_function_parameters(tokens) {
                                    Ok(parameters) => parameters,
                                    Err(parse_error) => {
                                        return Err(parse_error);
                                    }
                                };

                                // If there is a return type for the function after the parameters
                                if let Some(Token {
                                    type_: TokenType::OperatorArrow,
                                    ..
                                }) = tokens.peek()
                                {
                                    tokens.next();
                                    tokens.next(); // For now, ignore the return type of the function.
                                }

                                let mut body: Vec<AstNode<'_>> = vec![];

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
                                                    attributes,
                                                    name,
                                                    parameters,
                                                    body,
                                                });
                                                continue 'main_parse_loop;
                                            }
                                            TokenType::Identifier(name) => {
                                                // Expect an addition to follow this identifier token.
                                                if !matches!(
                                                    tokens.next(),
                                                    Some(Token {
                                                        type_: TokenType::OperatorAddition,
                                                        ..
                                                    })
                                                ) {
                                                    return Err(create_parse_error!(
                                                        next_token,
                                                        format!(
                                                            "Expected addition (+) after identifier. Instead, found: {:?}",
                                                            tokens.peek()
                                                        )
                                                    ));
                                                }

                                                prev_value_token = if let Some(Token {
                                                    type_: TokenType::Identifier(name2),
                                                    ..
                                                }) = tokens.peek()
                                                {
                                                    tokens.next();

                                                    Some(AstNode::Addition(
                                                        Box::new(AstNode::Identifier(name)),
                                                        Box::new(AstNode::Identifier(name2)),
                                                    ))
                                                } else {
                                                    return Err(create_parse_error!(
                                                        next_token,
                                                        format!(
                                                            "Expected identifier after addition (+). Instead, found: {:?}",
                                                            tokens.peek()
                                                        )
                                                    ));
                                                }
                                            }
                                            TokenType::LiteralInteger(value) => {
                                                prev_value_token =
                                                    Some(AstNode::LiteralInteger(value));
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
                        }
                        _ => {
                            return Err(create_parse_error!(
                                token,
                                format!(
                                    "Expected function attributes or name after '{KEYWORD_FN}' keyword. Instead, got {:?}",
                                    tokens.peek()
                                )
                            ));
                        }
                    }
                }
            }
            TokenType::KeywordLet => match parse_variable_definition(tokens, token, &scope) {
                Ok((new_ast_nodes, new_tokens)) => {
                    ast_nodes.extend(new_ast_nodes);
                    tokens = new_tokens;
                }
                Err(parse_error) => {
                    return Err(parse_error);
                }
            },
            _ => {}
        }
    }

    Ok(ast_nodes)
}
/// Parses attributes from the given token iterator from both functions and variable definitions.
/// The given iterator is expected to have consumed the first attribute name token.
fn parse_attributes<'a>(
    mut tokens: PeekableTokenIter<'a>,
    first_attribute_name: &'a str,
) -> Result<(Vec<AstNodeAttribute<'a>>, PeekableTokenIter<'a>), ParseError> {
    let mut attributes: Vec<AstNodeAttribute<'_>> = vec![];
    let mut callable_attribute_args: Vec<AstNode<'_>> = vec![];
    let mut current_attribute_name: &str = first_attribute_name;
    let mut parsing_callable_attribute_args = false;

    while let Some(token) = tokens.peek() {
        match token.type_ {
            // If encountered another attribute
            TokenType::Attribute(name) => {
                // If still parsing callable attribute arguments, then it is an error.
                if parsing_callable_attribute_args {
                    return Err(create_parse_error!(
                        token,
                        format!("Unexpected attribute after a parenthesis to begin callable attribute arguments.")
                    ));
                }

                tokens.next();

                if callable_attribute_args.iter().count() > 0 {
                    attributes.push(AstNodeAttribute::Callable {
                        name: current_attribute_name,
                        arguments: callable_attribute_args,
                    });
                    callable_attribute_args = vec![];
                } else {
                    attributes.push(AstNodeAttribute::Value(current_attribute_name));
                }

                current_attribute_name = name;
            }
            // If encountered an end token for callable attribute arguments
            TokenType::DelimiterParenthesisClose => {
                if parsing_callable_attribute_args {
                    tokens.next();

                    attributes.push(AstNodeAttribute::Callable {
                        name: current_attribute_name,
                        arguments: callable_attribute_args.clone(),
                    });
                } else {
                    return Err(create_parse_error!(
                        token,
                        format!("Unepected closing parenthesis")
                    ));
                }
            }
            // If encountered a begin token for callable attribute arguments
            TokenType::DelimiterParenthesisOpen => {
                tokens.next();

                parsing_callable_attribute_args = true;
            }
            TokenType::LiteralInteger(value) => {
                if parsing_callable_attribute_args {
                    tokens.next();

                    callable_attribute_args.push(AstNode::LiteralInteger(value));
                } else {
                    return Err(create_parse_error!(
                        token,
                        format!("Unexpected integer literal outside callable attribute arguments.")
                    ));
                }
            }
            _ => {
                break;
            }
        }
    }

    // This special case is for variable definitions with only one value attribute.
    if *current_attribute_name == *first_attribute_name && callable_attribute_args.len() == 0 {
        attributes.push(AstNodeAttribute::Value(current_attribute_name));
    }

    Ok((attributes, tokens))
}

fn parse_expression_until_token_type<'a>(
    mut tokens: PeekableTokenIter<'a>,
    prev_token: &Token<'_>,
    end_token_type: TokenType<'_>,
) -> Result<(AstNode<'a>, PeekableTokenIter<'a>), ParseError> {
    /// TODO: Think of a better name for this type.
    #[derive(Clone, Debug)]
    enum Unit<'a> {
        Node(Box<AstNode<'a>>),
        Operator(TokenType<'a>),
        Value(AstNode<'a>),
    }

    let mut units = vec![];

    while let Some(token) = tokens.next() {
        match token.type_ {
            TokenType::DelimiterParenthesisOpen => {
                let (expression, new_tokens) = parse_expression_until_token_type(
                    tokens,
                    token,
                    TokenType::DelimiterParenthesisClose,
                )?;
                tokens = new_tokens;
                units.push(Unit::Node(Box::new(expression)));
            }
            TokenType::Identifier(name) => {
                units.push(Unit::Value(AstNode::Identifier(name)));
            }
            TokenType::LiteralInteger(value) => {
                units.push(Unit::Value(AstNode::LiteralInteger(value)));
            }
            TokenType::LiteralString(value) => {
                units.push(Unit::Value(AstNode::LiteralString(value)));
            }
            TokenType::OperatorAddition => {
                units.push(Unit::Operator(TokenType::OperatorAddition));
            }
            TokenType::OperatorBinaryAnd => {
                units.push(Unit::Operator(TokenType::OperatorBinaryAnd));
            }
            TokenType::OperatorDivision => {
                units.push(Unit::Operator(TokenType::OperatorDivision));
            }
            TokenType::OperatorMultiplication => {
                units.push(Unit::Operator(TokenType::OperatorMultiplication));
            }
            TokenType::OperatorSubstraction => {
                units.push(Unit::Operator(TokenType::OperatorSubstraction));
            }
            _ => {
                if token.type_ == end_token_type {
                    break;
                } else {
                    return Err(create_parse_error!(
                        token,
                        format!("Unexpected token {:?}.", token)
                    ));
                }
            }
        }
    }

    // Precdence starts off with the highest precedence level and goes down from there in the loop below.
    let mut current_precedence = 3;

    while current_precedence > 0 {
        let mut i = 0;
        while i < units.len() {
            /// Parses a binary expression from the given units into AST nodes on the left and right side of the operator.
            fn parse_binary_expression<'a>(
                token: &Token<'_>,
                units: &[Unit<'a>],
            ) -> Result<(Box<AstNode<'a>>, Box<AstNode<'a>>), ParseError> {
                let left = match units.get(0) {
                    Some(Unit::Node(node)) => node.clone(),
                    Some(Unit::Value(value)) => Box::new(value.clone()),
                    _ => {
                        return Err(create_parse_error!(
                            // TODO: Change token to the actual token associated with the error.
                            token,
                            format!("Expected value on the left side of addition operator.")
                        ));
                    }
                };
                let right = match units.get(2) {
                    Some(Unit::Node(node)) => node.clone(),
                    Some(Unit::Value(value)) => Box::new(value.clone()),
                    _ => {
                        return Err(create_parse_error!(
                            // TODO: Change token to the actual token associated with the error.
                            token,
                            format!("Expected value on the right side of addition operator.")
                        ));
                    }
                };
                Ok((left, right))
            }

            /// Reduces repetitive code for parsing binary expression for an operator.
            /// This macro also ensures no additional function calls with all the parameter
            /// passing associated with that is not needed.
            macro_rules! parse_binary_expression_for_operator {
                ($operator: ident) => {{
                    let binary_expr_begin_index = i - 1;
                    let binary_expr_end_index = i + 1;
                    let binary_expr_range = binary_expr_begin_index..=binary_expr_end_index;

                    let (left, right) =
                        parse_binary_expression(prev_token, &units[binary_expr_range.clone()])?;

                    // Remove the existing units and replace them with a single node unit.
                    units.drain(binary_expr_range);
                    units.insert(
                        binary_expr_begin_index,
                        Unit::Node(Box::new(AstNode::$operator(left, right))),
                    );

                    i += 2;
                }};
            }

            // Match cases for each supported operator ordered by their precedence level in descending order.
            match (current_precedence, &units[i]) {
                (3, Unit::Operator(TokenType::OperatorDivision)) => {
                    parse_binary_expression_for_operator!(Division)
                }
                (3, Unit::Operator(TokenType::OperatorMultiplication)) => {
                    parse_binary_expression_for_operator!(Multiplication)
                }

                (2, Unit::Operator(TokenType::OperatorAddition)) => {
                    parse_binary_expression_for_operator!(Addition)
                }
                (2, Unit::Operator(TokenType::OperatorSubstraction)) => {
                    parse_binary_expression_for_operator!(Substraction)
                }

                (1, Unit::Operator(TokenType::OperatorBinaryAnd)) => {
                    parse_binary_expression_for_operator!(BinaryAnd)
                }

                _ => {
                    i += 1;
                }
            }
        }
        current_precedence -= 1;
    }

    // There should be only one unit left, which is the root node of the expression.
    assert_eq!(units.len(), 1);

    let ast_node = match &units[0] {
        Unit::Node(node) => Ok(*node.clone()),
        Unit::Value(value) => Ok(value.clone()),
        _ => {
            // Should never happen, but it is here to satisfy the compiler and ensure all cases are handled.
            panic!(
                "Expected node or value unit. Instead, found: {:?}",
                units[0]
            );
        }
    }?;

    Ok((ast_node, tokens))
}

fn parse_function_parameters<'a>(
    mut tokens: PeekableTokenIter<'a>,
) -> Result<(Vec<AstNodeFunctionParameter<'a>>, PeekableTokenIter<'a>), ParseError> {
    let mut resulting_parameters: Vec<AstNodeFunctionParameter<'_>> = vec![];

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
                            resulting_parameters.push(AstNodeFunctionParameter {
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

    Ok((resulting_parameters, tokens))
}

fn parse_variable_definition<'a>(
    mut tokens: PeekableTokenIter<'a>,
    token: &Token<'_>,
    scope: &Scope,
) -> Result<(Vec<AstNode<'a>>, PeekableTokenIter<'a>), ParseError> {
    let mut attributes: Vec<AstNodeAttribute<'_>> = vec![];
    let mut ast_nodes: Vec<AstNode<'_>> = vec![];

    'variable_defition_parsing_loop: loop {
        match tokens.peek() {
            Some(Token {
                type_: TokenType::Attribute(name),
                ..
            }) => {
                // In this state, expect more than one attribute to follow.
                tokens.next();

                (tokens, attributes) = match parse_attributes(tokens, name) {
                    Ok((attributes, new_tokens)) => (new_tokens, attributes),
                    Err(parse_error) => {
                        return Err(parse_error);
                    }
                };

                // Ensure that static variables are only allowed in the global scope.
                if attributes.contains(&AstNodeAttribute::Value("static"))
                    && *scope != Scope::Global
                {
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

                        let assignment_token = match tokens.next() {
                            Some(token) => {
                                if matches!(token.type_, TokenType::OperatorAssignment) {
                                    token
                                } else {
                                    return Err(create_parse_error!(
                                        token,
                                        format!(
                                            "Expected assignment operator after variable name."
                                        )
                                    ));
                                }
                            }
                            _ => {
                                return Err(create_parse_error!(
                                    token,
                                    format!("Expected a token after the variable type name.")
                                ));
                            }
                        };

                        let value = match parse_expression_until_token_type(
                            tokens.clone(),
                            &assignment_token,
                            TokenType::OperatorStatementEnd,
                        ) {
                            Ok((value, new_tokens)) => {
                                tokens = new_tokens;
                                value
                            }
                            Err(parse_error) => {
                                return Err(parse_error);
                            }
                        };

                        ast_nodes.push(AstNode::VariableDefinition(
                            AstNodeVariableIdentifier::WithType {
                                attributes,
                                identifier_name: name,
                                type_name: variable_type_name,
                            },
                            Box::new(value),
                        ));
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

    macro_rules! push_token_with_pos {
        ($type: expr) => {{
            tokens.push(Token {
                col: current_col,
                line: current_line,
                type_: $type,
            });
        }};
    }

    while let Some((i, c)) = chars.next() {
        match c {
            '}' => {
                if brace_level == 0 {
                    return Err("Unexpected closing brace. There are too many closing braces compared to opening braces.".into());
                }
                brace_level -= 1;
                push_token_with_pos!(TokenType::DelimiterBraceClose { level: brace_level });
                current_col += 1;
            }
            '{' => {
                push_token_with_pos!(TokenType::DelimiterBraceOpen { level: brace_level });
                brace_level += 1;
                current_col += 1;
            }
            ':' => {
                current_col += 1;
                push_token_with_pos!(TokenType::DelimiterColon);
            }
            ',' => {
                current_col += 1;
                push_token_with_pos!(TokenType::DelimiterComma);
            }
            ')' => {
                push_token_with_pos!(TokenType::DelimiterParenthesisClose);
                current_col += 1;
            }
            '(' => {
                push_token_with_pos!(TokenType::DelimiterParenthesisOpen);
                current_col += 1;
            }
            '=' => {
                push_token_with_pos!(TokenType::OperatorAssignment);
                current_col += 1;
            }
            '+' => {
                push_token_with_pos!(TokenType::OperatorAddition);
                current_col += 1;
            }
            '/' => {
                push_token_with_pos!(TokenType::OperatorDivision);
                current_col += 1;
            }
            '*' => {
                push_token_with_pos!(TokenType::OperatorMultiplication);
                current_col += 1;
            }
            '-' => {
                if let Some((_, '>')) = chars.peek() {
                    chars.next();
                    push_token_with_pos!(TokenType::OperatorArrow);
                    current_col += 2;
                } else {
                    push_token_with_pos!(TokenType::OperatorSubstraction);
                    current_col += 1;
                }
            }
            '&' => {
                push_token_with_pos!(TokenType::OperatorBinaryAnd);
                current_col += 1;
            }
            ';' => {
                push_token_with_pos!(TokenType::OperatorStatementEnd);
                current_col += 1;
            }
            ' ' => {
                // Whitespace characters are not stored as tokens.
                current_col += 1;
            }
            '\n' => {
                current_col = 0;
                current_line += 1;
            }
            '#' => {
                // Skip this character, because it is not stored as part of the attribute token.
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
                push_token_with_pos!(TokenType::Attribute(attribute));
                current_col += attribute_index_end - attribute_index_begin + 1; // + 1 for the '#' character.
            }
            '"' => {
                // Skip this character, because it is not stored as part of the string literal token.
                chars.next();

                let mut string_literal_index_end = i;

                while let Some((j, next_c)) = chars.next() {
                    if next_c == '"' {
                        string_literal_index_end = j;
                        break;
                    }
                }

                let value = &code[i + 1..string_literal_index_end];
                push_token_with_pos!(TokenType::LiteralString(value));
                current_col += string_literal_index_end - i + 1; // + 1 for the '"' character.
            }
            _ => {
                if c.is_digit(10) {
                    // In this state, we expect an integer literal in decimal or hexadecimal format.
                    let mut is_hexadecimal = false;
                    let mut value_index_end = i;

                    while let Some((j, next_c)) = chars.peek() {
                        if next_c.is_digit(10) {
                            chars.next();
                        } else if is_hexadecimal && next_c.is_ascii_hexdigit() {
                            chars.next();
                        } else if *next_c == 'x' {
                            // If the character preceeding the hexadecimal delimiter is a 0 digit
                            if c == '0' {
                                chars.next();
                                is_hexadecimal = true;
                            }
                            // If there is no 0 digit preceeding the hexadecimal delimiter, it is an error.
                            else {
                                return Err(
                                    format!("Invalid character '{}' found in integer literal. Expected digit 0 to preceed the {}character for a hexadecimal literal.", c, next_c),
                                );
                            }
                        } else {
                            value_index_end = *j;
                            break;
                        }
                    }

                    let value = {
                        if is_hexadecimal {
                            // + 2 to skip the 0x prefix in the hexadecimal literal.
                            let value_str_without_0x = &code[i + 2..value_index_end];
                            let hex_value = i64::from_str_radix(value_str_without_0x, 16)
                                .map_err(|e| e.to_string())?;
                            hex_value
                        } else {
                            let value_str = if i == value_index_end {
                                &code[i..=value_index_end]
                            } else {
                                &code[i..value_index_end]
                            };

                            let dec_value = value_str.parse::<i64>().map_err(|e| e.to_string())?;
                            dec_value
                        }
                    };

                    push_token_with_pos!(TokenType::LiteralInteger(value));
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

                    macro_rules! push_keyword_token_with_pos {
                        ($keyword: ident, $type: expr) => {{
                            push_token_with_pos!($type);
                            current_col += $keyword.len();
                        }};
                    }

                    match &code[i..identifier_or_keyword_index_end] {
                        KEYWORD_FN => {
                            push_keyword_token_with_pos!(KEYWORD_FN, TokenType::KeywordFn)
                        }
                        KEYWORD_LET => {
                            push_keyword_token_with_pos!(KEYWORD_LET, TokenType::KeywordLet)
                        }
                        KEYWORD_TRUE => {
                            push_keyword_token_with_pos!(
                                KEYWORD_TRUE,
                                TokenType::LiteralBooleanTrue
                            )
                        }
                        KEYWORD_FALSE => {
                            push_keyword_token_with_pos!(
                                KEYWORD_FALSE,
                                TokenType::LiteralBooleanFalse
                            )
                        }
                        _ => {
                            let identifier = &code[i..identifier_or_keyword_index_end];
                            push_token_with_pos!(TokenType::Identifier(identifier));
                            current_col += identifier_or_keyword_index_end - i;
                        }
                    }
                }
            }
        }
    }

    Ok(tokens)
}

mod tests;
