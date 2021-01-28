mod parser;

use clap::Clap;
use parser::*;

#[derive(Clap)]
struct Opts {
    input: String,
}

fn main() {
    let input = Opts::parse().input;
    let parser = then(whitespace(), expr());
    let result = parse(parser, &input);
    let output = match result {
        Ok(success) => convert(success.value),
        Err(failure) => error(&input, failure.position, failure.expected),
    };
    println!("{}", output);
}

fn convert(ast: AST) -> String {
    vec![
        ".intel_syntax noprefix",
        ".globl main",
        "main:",
        &gen(ast),
        "    pop rax",
        "    ret",
    ]
    .join("\n")
}

fn error(source: &str, position: usize, expected: Vec<String>) -> String {
    vec![
        "Failed to compile:".to_string(),
        source.to_string(),
        " ".repeat(position) + "^",
        " ".repeat(position) + "expected: " + &expected.join(", "),
    ]
    .join("\n")
}

fn gen(tree: AST) -> String {
    match tree {
        AST::Literal { value } => format!("    push {}", value),
        AST::Operator { kind, lhs, rhs } => [
            vec![
                gen(*lhs).as_str(),
                gen(*rhs).as_str(),
                "    pop rdi",
                "    pop rax",
            ],
            match kind {
                OpKind::Add => vec!["    add rax, rdi"],
                OpKind::Sub => vec!["    sub rax, rdi"],
                OpKind::Mul => vec!["    imul rax, rdi"],
                OpKind::Div => vec!["    cqo", "    idiv rdi"],
                OpKind::Eq => vec!["  cmp rax, rdi", "  sete al", "  movzb rax, al"],
                OpKind::Ne => vec!["  cmp rax, rdi", "  setne al", "  movzb rax, al"],
                OpKind::Lt => vec!["  cmp rax, rdi", "  setl al", "  movzb rax, al"],
                OpKind::Le => vec!["  cmp rax, rdi", "  setle al", "  movzb rax, al"],
            },
            vec!["    push rax"],
        ]
        .concat()
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .join("\n"),
    }
}

#[derive(PartialEq, Debug, Clone)]
enum OpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Eq,  // ==
    Ne,  // !=
    Lt,  // <
    Le,  // <=
}

#[derive(PartialEq, Debug, Clone)]
enum AST {
    Operator {
        kind: OpKind,
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    Literal {
        value: usize,
    },
}

fn whitespace<'a>() -> impl Parser<'a, String> {
    regex(r"\s*", 0)
}

fn token<'a, P, Output>(parser: P) -> impl Parser<'a, Output>
where
    P: Parser<'a, Output>,
    Output: Clone,
{
    skip(parser, whitespace())
}

/// expr       = equality
#[derive(Clone)]
struct Expr;
impl<'a> Parser<'a, AST> for Expr {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<AST>, Failure> {
        equality().parse(input, position)
    }
}
fn expr<'a>() -> impl Parser<'a, AST> {
    Expr
}

/// equality   = relational ("==" relational | "!=" relational)*
fn equality<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            relational(),
            many(or(
                and(token(string("==")), relational()),
                and(token(string("!=")), relational()),
            )),
        ),
        |(init, rest)| {
            rest.iter().fold(init, |node, (kind, next)| AST::Operator {
                kind: if kind == "==" { OpKind::Eq } else { OpKind::Ne },
                lhs: Box::new(node),
                rhs: Box::new(next.clone()),
            })
        },
    )
}

/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            add(),
            many(or(
                and(token(string(">")), add()),
                or(
                    and(token(string("<")), add()),
                    or(
                        and(token(string(">=")), add()),
                        and(token(string("<=")), add()),
                    ),
                ),
            )),
        ),
        |(init, rest)| {
            rest.iter()
                .fold(init, |node, (kind, next)| match kind.as_str() {
                    l @ "<" | l @ "<=" => AST::Operator {
                        kind: if l == "<" { OpKind::Lt } else { OpKind::Le },
                        lhs: Box::new(node),
                        rhs: Box::new(next.clone()),
                    },
                    g => AST::Operator {
                        kind: if g == ">" { OpKind::Lt } else { OpKind::Le },
                        lhs: Box::new(next.clone()),
                        rhs: Box::new(node),
                    },
                })
        },
    )
}

/// add        = mul ("+" mul | "-" mul)*
fn add<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            mul(),
            many(or(
                and(token(string("+")), mul()),
                and(token(string("-")), mul()),
            )),
        ),
        |(init, rest)| {
            rest.iter().fold(init, |node, (kind, next)| AST::Operator {
                kind: if kind == "+" {
                    OpKind::Add
                } else {
                    OpKind::Sub
                },
                lhs: Box::new(node),
                rhs: Box::new(next.clone()),
            })
        },
    )
}

/// mul     = unary ("*" unary | "/" unary)*
fn mul<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            unary(),
            many(or(
                and(token(string("*")), unary()),
                and(token(string("/")), unary()),
            )),
        ),
        |(init, rest)| {
            rest.iter().fold(init, |node, (kind, next)| AST::Operator {
                kind: if kind == "*" {
                    OpKind::Mul
                } else {
                    OpKind::Div
                },
                lhs: Box::new(node),
                rhs: Box::new(next.clone()),
            })
        },
    )
}

/// unary      = ("+" | "-")? primary
fn unary<'a>() -> impl Parser<'a, AST> {
    map(
        and(at_most(or(string("+"), string("-")), 1), primary()),
        |(ops, primary)| {
            if ops.is_empty() {
                primary
            } else {
                AST::Operator {
                    kind: if ops.first().unwrap() == "+" {
                        OpKind::Add
                    } else {
                        OpKind::Sub
                    },
                    lhs: Box::new(AST::Literal { value: 0 }),
                    rhs: Box::new(primary),
                }
            }
        },
    )
}

/// primary = num | "(" expr ")"
fn primary<'a>() -> impl Parser<'a, AST> {
    or(
        num(),
        then(token(string("(")), skip(expr(), token(string(")")))),
    )
}

fn num<'a>() -> impl Parser<'a, AST> {
    map(token(regex("(0|[1-9][0-9]*)", 0)), |input| AST::Literal {
        value: input.parse::<usize>().unwrap(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_ok() {
        let parser = then(whitespace(), expr());
        let result = parse(parser, " 1 + 2 * 3 / 2 ");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            AST::Operator {
                kind: OpKind::Add,
                lhs: Box::new(AST::Literal { value: 1 }),
                rhs: Box::new(AST::Operator {
                    kind: OpKind::Div,
                    lhs: Box::new(AST::Operator {
                        kind: OpKind::Mul,
                        lhs: Box::new(AST::Literal { value: 2 }),
                        rhs: Box::new(AST::Literal { value: 3 }),
                    }),
                    rhs: Box::new(AST::Literal { value: 2 }),
                }),
            },
        );
    }

    #[test]
    fn num_ok() {
        let parser = then(whitespace(), num());
        let result = parse(parser, "   123   ");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), AST::Literal { value: 123 });
    }
}
