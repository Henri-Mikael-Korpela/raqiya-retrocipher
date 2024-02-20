use rcc::AstNode;

#[derive(Debug)]
pub enum AssemblyNode<'a> {
    Label {
        content: &'a str,
        indent_level: usize,
    },
}
impl<'a> ToString for AssemblyNode<'a> {
    fn to_string(&self) -> String {
        match self {
            AssemblyNode::Label {
                content,
                indent_level,
            } => format!("{}{}:", "\t".repeat(*indent_level), content),
        }
    }
}

/// Assemble the given MIPS R3000 assembly nodes to binary data.
/// Each instruction is 32 bits long, hence they can be stored into a vec of 32-bit integers.
pub fn assemble(_assembly_nodes: Vec<AssemblyNode>) -> Vec<i32> {
    vec![]
}
/// Compile the given AST nodes to MIPS R3000 assembly nodes.
pub fn compile_to_assembly<'a>(ast_nodes: &'a [AstNode]) -> Vec<AssemblyNode<'a>> {
    let mut assembly_nodes = vec![];

    for node in ast_nodes {
        match node {
            AstNode::FunctionDefinition { name, .. } => {
                assembly_nodes.push(AssemblyNode::Label {
                    content: name,
                    indent_level: 0,
                });
            }
            _ => {}
        }
    }

    assembly_nodes
}
