use proc_macro::{TokenStream, TokenTree};

#[proc_macro]
pub fn html(input: TokenStream) -> TokenStream {
    let mut result = String::new();

    let mut subsequent_ident_count = 0;
    let mut prev_is_str_literal = false;

    for token in input {
        match token {
            TokenTree::Ident(ident) => {
                if prev_is_str_literal {
                    result.push(' ');
                }
                if subsequent_ident_count > 0 {
                    result.push_str(" ");
                }
                result.push_str(&ident.to_string());
                subsequent_ident_count += 1;
            }
            TokenTree::Literal(literal) => {
                let literal_as_string = literal.to_string();
                if literal_as_string.starts_with("\"") {
                    // Escape double quotes
                    let literal_as_string = literal_as_string.replace("\"", "\\\"");
                    result.push_str(&literal_as_string);
                }
                prev_is_str_literal = true;
                subsequent_ident_count = 0;
            }
            TokenTree::Punct(punct) => {
                match punct.as_char() {
                    '/' => result.push('/'),
                    '<' => result.push('<'),
                    '>' => {
                        if prev_is_str_literal {
                            prev_is_str_literal = false;
                        }

                        result.push('>');
                    }
                    '=' => result.push('='),
                    '.' => result.push('.'),
                    _ => {}
                }
                subsequent_ident_count = 0;
            }
            _ => println!("Unexpected token: {:?}", token),
        }
    }

    let result = format!("\"{}\"", result).parse().unwrap();
    result
}
