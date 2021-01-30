mod parser;

use clap::Clap;
use parser::*;
use std::collections::HashMap;

#[derive(Clap)]
struct Opts {
    input: String,
}

fn main() {
    let input = Opts::parse().input;
    let parser = then(whitespace(), program());
    let result = parse(parser, &input);
    match result {
        Ok(success) => println!("{}", convert(success.value)),
        Err(failure) => panic!("{}", error(&input, failure.position, failure.expected)),
    };
}

fn convert(asts: Vec<AST>) -> String {
    let mut vars = HashMap::new();
    let mut acc = vec![];
    for ast in asts {
        let (gen, new_vars) = gen(ast, vars);
        vars = new_vars;
        acc.push(gen + "\n  pop rax");
    }
    vec![
        ".intel_syntax noprefix",
        ".globl main",
        "main:",
        // prologue, allocate memory for 26 variables
        "  push rbp",
        "  mov rbp, rsp",
        "  sub rsp, 208",
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
    Variable {
        name: String,
    },
    Assign {
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    Return {
        lhs: Box<AST>,
    },
}

fn gen_lvar(tree: AST, vars: HashMap<String, usize>) -> (String, HashMap<String, usize>) {
    match tree {
        AST::Variable { name } => {
            let (offset, new_vars) = match vars.get(&name) {
                Some(offset) => (*offset, vars),
                None => {
                    let next_offset = match vars.values().max() {
                        Some(offset) => offset + 8,
                        None => 8,
                    };
                    let next_vars = vars
                        .into_iter()
                        .chain(vec![(name.clone(), next_offset)])
                        .collect();
                    (next_offset, next_vars)
                }
            };
            (
                vec![
                    "  mov rax, rbp",
                    format!("  sub rax, {}", offset).as_str(),
                    "  push rax",
                ]
                .join("\n"),
                new_vars,
            )
        }
        _ => panic!("The left side value of the assignment is not a variable."),
    }
}

fn gen(tree: AST, vars: HashMap<String, usize>) -> (String, HashMap<String, usize>) {
    match tree {
        AST::Literal { value } => (format!("  push {}", value), vars),
        AST::Variable { name: _ } => {
            let (gen, new_vars) = gen_lvar(tree, vars);
            (
                vec![gen.as_str(), "  pop rax", "  mov rax, [rax]", "  push rax"].join("\n"),
                new_vars,
            )
        }
        AST::Assign { lhs, rhs } => {
            let (lhs_gen, lhs_vars) = gen_lvar(*lhs, vars);
            let (rhs_gen, rhs_vars) = gen(*rhs, lhs_vars);
            (
                vec![
                    lhs_gen.as_str(),
                    rhs_gen.as_str(),
                    "  pop rdi",
                    "  pop rax",
                    "  mov [rax], rdi",
                    "  push rdi",
                ]
                .join("\n"),
                rhs_vars,
            )
        }
        AST::Return { lhs } => {
            let (lhs_gen, lhs_vars) = gen(*lhs, vars);
            (
                vec![
                    lhs_gen.as_str(),
                    "  pop rax",
                    "  mov rsp, rbp",
                    "  pop rbp",
                    "  ret",
                ]
                .join("\n"),
                lhs_vars,
            )
        }
        AST::Operator { kind, lhs, rhs } => {
            let (lhs_gen, lhs_vars) = gen(*lhs, vars);
            let (rhs_gen, rhs_vars) = gen(*rhs, lhs_vars);
            (
                [
                    vec![lhs_gen.as_str(), rhs_gen.as_str(), "  pop rdi", "  pop rax"],
                    match kind {
                        OpKind::Add => vec!["  add rax, rdi"],
                        OpKind::Sub => vec!["  sub rax, rdi"],
                        OpKind::Mul => vec!["  imul rax, rdi"],
                        OpKind::Div => vec!["  cqo", "  idiv rdi"],
                        OpKind::Eq => vec!["  cmp rax, rdi", "  sete al", "  movzb rax, al"],
                        OpKind::Ne => vec!["  cmp rax, rdi", "  setne al", "  movzb rax, al"],
                        OpKind::Lt => vec!["  cmp rax, rdi", "  setl al", "  movzb rax, al"],
                        OpKind::Le => vec!["  cmp rax, rdi", "  setle al", "  movzb rax, al"],
                    },
                    vec!["  push rax"],
                ]
                .concat()
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
                rhs_vars,
            )
        }
    }
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

/// program    = stmt*
fn program<'a>() -> impl Parser<'a, Vec<AST>> {
    many(stmt())
}

/// stmt       = expr ";"
///            | "return" expr ";"
fn stmt<'a>() -> impl Parser<'a, AST> {
    or(
        skip(expr(), token(string(";"))),
        map(
            skip(
                then(
                    token(not_followed_by(string("return"), regex("[a-zA-Z0-9_]", 0))),
                    expr(),
                ),
                token(string(";")),
            ),
            move |input| AST::Return {
                lhs: Box::new(input),
            },
        ),
    )
}

/// expr       = assign
fn expr<'a>() -> impl Parser<'a, AST> {
    assign()
}

/// assign     = equality ("=" assign)?
#[derive(Clone)]
struct Assign;
impl<'a> Parser<'a, AST> for Assign {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<AST>, Failure> {
        map(
            and(equality(), at_most(then(token(string("=")), assign()), 1)),
            |(equality, assign)| {
                if assign.is_empty() {
                    equality
                } else {
                    AST::Assign {
                        lhs: Box::new(equality),
                        rhs: Box::new(assign.first().unwrap().clone()),
                    }
                }
            },
        )
        .parse(input, position)
    }
}
fn assign<'a>() -> impl Parser<'a, AST> {
    Assign
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

/// primary = num | ident | "(" expr ")"
fn primary<'a>() -> impl Parser<'a, AST> {
    or(
        or(num(), ident()),
        then(token(string("(")), skip(expr(), token(string(")")))),
    )
}

fn num<'a>() -> impl Parser<'a, AST> {
    map(token(regex("(0|[1-9][0-9]*)", 0)), |input| AST::Literal {
        value: input.parse::<usize>().unwrap(),
    })
}

fn ident<'a>() -> impl Parser<'a, AST> {
    map(token(regex("[a-zA-Z_][a-zA-Z0-9_]*", 0)), |input| {
        AST::Variable { name: input }
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
