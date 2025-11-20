use clap::{Parser as ClapParser, Subcommand};
use std::fs;
use std::path::PathBuf;
use tjlb_rust::lexer::{tokenize};
use tjlb_rust::parser::Parser;
use tjlb_rust::codegen::CodeGenerator;
use inkwell::context::Context;

#[derive(ClapParser)]
#[command(name = "tjlb-parser")]
#[command(about = "Parser for the TJLB programming language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenize the input file and print tokens
    Lex {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Parse the input file and print the AST
    Parse {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Compile the input file to LLVM IR
    Compile {
        /// Input file path
        #[arg(value_name = "FILE")]
        file: PathBuf,

        /// Output file path (defaults to stdout)
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,

        /// Print IR to stdout even when writing to file
        #[arg(short, long)]
        print: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Lex { file } => {
            if let Err(e) = lex_file(&file) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
        Commands::Parse { file } => {
            if let Err(e) = parse_file(&file) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
        Commands::Compile { file, output, print } => {
            if let Err(e) = compile_file(&file, output.as_deref(), print) {
                eprintln!("Error: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn lex_file(path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(path)?;

    let tokens = tokenize(input)?;

    println!("Tokens:");
    println!("-------");
    for token in &tokens {
        println!("{}:{}:{} {:?} {}",
            path.display(),
            token.pos.line,
            token.pos.column,
            token.kind,
            token.lexeme
        );
    }

    println!("\nTotal tokens: {}", tokens.len());

    Ok(())
}

fn parse_file(path: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(path)?;

    // Tokenize
    let tokens = tokenize(input)?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    // Print AST
    println!("Program AST:");
    println!("============\n");

    if !program.global_vars.is_empty() {
        println!("Global Variables:");
        for global in &program.global_vars {
            println!("  {} {} = {:?}", global.var_type, global.name, global.initializer);
        }
        println!();
    }

    if !program.string_literals.is_empty() {
        println!("String Literals:");
        for (i, s) in program.string_literals.iter().enumerate() {
            println!("  [{i}]: \"{s}\"");
        }
        println!();
    }

    println!("Functions:");
    for func in &program.functions {
        print!("  fn {}(", func.proto.name);
        for (i, (param_type, param_name)) in func.proto.params.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            print!("{param_type} {param_name}");
        }
        println!(") -> {}", func.proto.return_type);

        if func.proto.is_extern {
            println!("    [extern]");
        } else {
            println!("    body: {} statements", func.body.len());
            if !func.body.is_empty() {
                println!("    statements:");
                for (i, stmt) in func.body.iter().enumerate() {
                    println!("      [{i}]: {stmt:#?}");
                }
            }
        }
    }

    println!("\nParsing completed successfully!");

    Ok(())
}

fn compile_file(path: &PathBuf, output: Option<&std::path::Path>, print: bool) -> Result<(), Box<dyn std::error::Error>> {
    let input = fs::read_to_string(path)?;

    // Tokenize
    let tokens = tokenize(input)?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    // Generate LLVM IR
    let context = Context::create();
    let module_name = path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    let mut codegen = CodeGenerator::new(&context, module_name);
    codegen.generate(&program)?;

    // Output
    let ir = codegen.print_to_string();

    if let Some(output_path) = output {
        codegen.write_to_file(output_path)?;
        if print {
            println!("{ir}");
        } else {
            println!("LLVM IR written to: {}", output_path.display());
        }
    } else {
        println!("{ir}");
    }

    Ok(())
}
