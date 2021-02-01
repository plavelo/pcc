mod ast;
mod combinator;
mod generator;
mod parser;

use clap::Clap;
use generator::*;
use parser::*;

#[derive(Clap)]
struct Opts {
    input: String,
}

fn main() {
    let input = Opts::parse().input;
    let result = parse_program(&input);
    match result {
        Ok(success) => println!("{}", generate(success.value)),
        Err(failure) => panic!("{}", error(&input, failure.position, failure.expected)),
    };
}

fn error(source: &str, position: usize, expected: Vec<String>) -> String {
    vec![
        "Failed to compile:".to_string(),
        source.to_string(),
        " ".repeat(position) + "^",
        " ".repeat(position) + "expected: " + expected.join(", ").as_str(),
    ]
    .join("\n")
}
