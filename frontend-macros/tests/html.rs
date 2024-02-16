#[cfg(test)]
mod tests {
    use frontend_macros::html;

    #[test]
    fn generate_markup_for_element_with_separate_close_tag_but_no_children() {
        let result = html!(<div></div>);
        assert_eq!(result, "<div></div>");

        let result = html!(<span></span>);
        assert_eq!(result, "<span></span>");
    }
    #[test]
    fn generate_markup_for_element_with_separate_close_tag_and_string_attributes_but_no_children() {
        let result = html!(<div class="container"></div>);
        assert_eq!(result, "<div class=\"container\"></div>");

        let result = html!(<span class="container"></span>);
        assert_eq!(result, "<span class=\"container\"></span>");
    }
    #[test]
    fn generate_markup_for_element_content() {
        let result = html!(<div>Hello, world!</div>);
        assert_eq!(result, "<div>Hello, world!</div>");
    }
}
