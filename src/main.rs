mod parser;

use clap::Clap;

#[derive(Clap)]
struct Opts {
    input: String,
}

fn main() {
    let opts: Opts = Opts::parse();

    let output = format!(
        r#".intel_syntax noprefix
.globl main
main:
    mov rax, {}
    ret"#,
        opts.input
    );

    println!("{}", output);
}
