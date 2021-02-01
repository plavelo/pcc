pub mod ast;
pub mod combinator;
pub mod generator;
pub mod parser;

use ast::*;
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

fn generate(asts: Vec<AST>) -> String {
    let mut env = Environment::default();
    let mut acc = vec![];
    for ast in asts {
        let (gen, next_env) = gen(ast, env);
        env = next_env;
        acc.push(gen);
    }
    vec![
        ".intel_syntax noprefix",
        ".globl main",
        "main:",
        // prologue, allocate memory for variables
        "  push rbp",
        "  mov rbp, rsp",
        format!("  sub rsp, {}", env.offset()).as_str(),
        acc.join("\n").as_str(),
        // epilogue
        "  mov rsp, rbp",
        "  pop rbp",
        "  ret",
    ]
    .join("\n")
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
