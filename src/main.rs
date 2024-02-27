use std::fs;

fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}
fn run() -> Result<(), String> {
    let mut args = std::env::args();
    args.next(); // Skip the program name

    // Parse command line arguments
    let mut file_path = None;
    let mut output_assembly_file_path = None;

    while let Some(arg) = args.next() {
        let arg = arg.as_str();
        match arg {
            "--in" => {
                if let Some(next_arg) = args.next() {
                    file_path = Some(next_arg);
                } else {
                    return Err(format!("No input file path given after option \"{arg}\"."));
                }
            }
            "--out-asm" => {
                if let Some(next_arg) = args.next() {
                    output_assembly_file_path = Some(next_arg);
                } else {
                    return Err(format!(
                        "No output assembly file path given after option \"{arg}\"."
                    ));
                }
            }
            _ => {}
        }
    }

    // Ensure that an input file path is given in the command line arguments
    let Some(file_path) = &file_path else {
        return Err(String::from(
            "No file path provided in command line arguments.",
        ));
    };

    let file_content = std::fs::read_to_string(file_path)
        .map_err(|err| format!("Failed to read the given file: {}", err))?;

    let tokens = rcc::tokenize(&file_content)?;

    let ast_nodes = rcc::parse(&tokens, rcc::Scope::Global)
        .map_err(|err| format!("Error in {}:{}: {}", file_path, err.position(), err.message))?;

    if let Some(output_assembly_file_path) = &output_assembly_file_path {
        let assembly_nodes = rc2mips_r3000::compile_to_assembly(&ast_nodes);

        // Convert each assembly node to a string and join them together
        // so that each node is on its own line.
        let assembly_content = assembly_nodes
            .iter()
            .map(|node| node.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        if let Err(err) = fs::write(output_assembly_file_path, assembly_content) {
            return Err(format!(
                "Failed to write to the output assembly file: {}",
                err
            ));
        }
    } else {
        println!("{:#?}", ast_nodes);
    }

    Ok(())
}
