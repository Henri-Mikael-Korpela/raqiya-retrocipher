use rcc::AstNode;

#[derive(Debug)]
pub struct AssemblyNode;

/// Assemble the given MIPS R3000 assembly nodes to binary data.
/// Each instruction is 32 bits long, hence they can be stored into a vec of 32-bit integers.
pub fn assemble(_assembly_nodes: Vec<AssemblyNode>) -> Vec<i32> {
    vec![]
}
/// Compile the given AST nodes to MIPS R3000 assembly nodes.
pub fn compile_to_assembly(_ast_nodes: Vec<AstNode>) -> Vec<AssemblyNode> {
    vec![]
}
