fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args();
    args.next(); // Skip the program name

    let Some(file_path) = args.next() else {
        return Err("No file path provided".into());
    };

    let file_content = std::fs::read_to_string(file_path)
        .map_err(|err| format!("Failed to read the given file: {}", err))?;

    let tokens = rcc::tokenize(&file_content)?;

    let ast_nodes = rcc::parse(&tokens, rcc::Scope::Global)?;
    println!("{:?}", ast_nodes);

    Ok(())
}
