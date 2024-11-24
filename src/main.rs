use kast_ast::{lex, parse, read_syntax};
use kast_util::SourceFile;

use std::path::PathBuf;

pub mod eval;
pub mod node;
pub mod node2;

#[derive(Clone, clap::Subcommand)]
enum CliCommands {
    Parse { file: PathBuf },
    Eval { file: PathBuf },
}

#[derive(clap::Parser)]
struct Cli {
    #[command(subcommand)]
    command: CliCommands,
}

fn main() {
    let args = <Cli as clap::Parser>::parse();
    tracing_subscriber::fmt::init();

    // Parse the syntax-file with the kast syntax parser
    let syntax_def = read_syntax(SourceFile {
        contents: std::fs::read_to_string(
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("syntax.ks"),
        )
        .unwrap(),
        filename: "std/syntax.ks".into(),
    })
    .unwrap();

    let tokens = lex(SourceFile {
        contents: std::fs::read_to_string(match &args.command {
            CliCommands::Parse { file } => file,
            CliCommands::Eval { file } => file,
        })
        .unwrap(),
        filename: "<stdin>".into(),
    })
    .unwrap();

    // let parser = Parser

    //Parse the provided source-file with the kast parser
    let ast = parse(
        &syntax_def,
        SourceFile {
            contents: std::fs::read_to_string(match &args.command {
                CliCommands::Parse { file } => file,
                CliCommands::Eval { file } => file,
            })
            .unwrap(),
            filename: "<stdin>".into(),
        },
    )
    .unwrap()
    .unwrap();

    // let mut parser = Parser::recursive(tokens);
    // let ast = parser
    //     .read_all(&syntax_def)
    //     .map_err(|msg| msg.at(parser.reader.peek().unwrap().span.clone()))
    //     .unwrap()
    //     .unwrap();

    // Print the debug representation of the parsed source ast
    // println!("{:#}", ast);

    let node: node::Node = (&ast).try_into().unwrap();
    let statements = node.statements();
    let _ = statements
        .map(|statement| println!("{}", statement.pretty_printer()))
        .collect::<()>();

    // Evaluate the parsed source ast
    if let CliCommands::Eval { .. } = args.command {
        todo!();
    }
}
