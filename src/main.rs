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
        Ok(success) => vec![
            ".intel_syntax noprefix\n",
            ".globl main\n",
            "main:\n",
            &gen(success.value),
            "    pop rax\n",
            "    ret\n",
        ]
        .concat(),
        Err(failure) => error(&input, failure.position, failure.expected),
    };
    println!("{}", output);
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
    Gt,  // >
    Ge,  // >=
}

#[derive(PartialEq, Debug, Clone)]
enum AST {
    Operator {
        kind: OpKind,
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    Literal {
        value: i32,
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
                map(then(token(string("==")), relational()), |input| {
                    (OpKind::Eq, input)
                }),
                map(then(token(string("!=")), relational()), |input| {
                    (OpKind::Ne, input)
                }),
            )),
        ),
        |input| {
            let (head, tail) = input;
            let mut node = head;
            for (kind, next_node) in tail.iter() {
                node = AST::Operator {
                    kind: kind.clone(),
                    lhs: Box::new(node),
                    rhs: Box::new(next_node.clone()),
                }
            }
            node
        },
    )
}

/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            add(),
            many(or(
                map(then(token(string(">")), add()), |input| (OpKind::Gt, input)),
                or(
                    map(then(token(string(">=")), add()), |input| {
                        (OpKind::Ge, input)
                    }),
                    or(
                        map(then(token(string("<=")), add()), |input| {
                            (OpKind::Le, input)
                        }),
                        map(then(token(string("<")), add()), |input| (OpKind::Lt, input)),
                    ),
                ),
            )),
        ),
        |input| {
            let (head, tail) = input;
            let mut node = head;
            for (kind, next_node) in tail.iter() {
                node = match kind {
                    OpKind::Lt => AST::Operator {
                        kind: OpKind::Lt,
                        lhs: Box::new(node),
                        rhs: Box::new(next_node.clone()),
                    },
                    OpKind::Le => AST::Operator {
                        kind: OpKind::Le,
                        lhs: Box::new(node),
                        rhs: Box::new(next_node.clone()),
                    },
                    OpKind::Gt => AST::Operator {
                        kind: OpKind::Lt,
                        lhs: Box::new(next_node.clone()),
                        rhs: Box::new(node),
                    },
                    // must be OpKind::Ge
                    _ => AST::Operator {
                        kind: OpKind::Le,
                        lhs: Box::new(next_node.clone()),
                        rhs: Box::new(node),
                    },
                };
            }
            node
        },
    )
}

/// add        = mul ("+" mul | "-" mul)*
fn add<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            mul(),
            many(or(
                map(then(token(string("+")), mul()), |input| {
                    (OpKind::Add, input)
                }),
                map(then(token(string("-")), mul()), |input| {
                    (OpKind::Sub, input)
                }),
            )),
        ),
        |input| {
            let (head, tail) = input;
            let mut node = head;
            for (kind, next_node) in tail.iter() {
                node = AST::Operator {
                    kind: kind.clone(),
                    lhs: Box::new(node),
                    rhs: Box::new(next_node.clone()),
                }
            }
            node
        },
    )
}

/// mul     = unary ("*" unary | "/" unary)*
fn mul<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            unary(),
            many(or(
                map(then(token(string("*")), unary()), |input| {
                    (OpKind::Mul, input)
                }),
                map(then(token(string("/")), unary()), |input| {
                    (OpKind::Div, input)
                }),
            )),
        ),
        |input| {
            let (head, tail) = input;
            let mut node = head;
            for (kind, next_node) in tail.iter() {
                node = AST::Operator {
                    kind: kind.clone(),
                    lhs: Box::new(node),
                    rhs: Box::new(next_node.clone()),
                }
            }
            node
        },
    )
}

/// unary      = ("+" | "-")? primary
fn unary<'a>() -> impl Parser<'a, AST> {
    map(
        and(at_most(or(string("+"), string("-")), 1), primary()),
        |input| {
            let (ops, primary) = input;
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
        value: input.parse::<i32>().unwrap(),
    })
}

fn gen(tree: AST) -> String {
    match tree {
        AST::Literal { value } => format!("    push {}\n", value),
        AST::Operator { kind, lhs, rhs } => [
            vec![
                gen(*lhs).as_str(),
                &gen(*rhs).as_str(),
                "    pop rdi\n",
                "    pop rax\n",
            ],
            match kind {
                OpKind::Add => vec!["    add rax, rdi\n"],
                OpKind::Sub => vec!["    sub rax, rdi\n"],
                OpKind::Mul => vec!["    imul rax, rdi\n"],
                OpKind::Div => vec!["    cqo\n", "    idiv rdi\n"],
                OpKind::Eq => vec!["  cmp rax, rdi\n", "  sete al\n", "  movzb rax, al\n"],
                OpKind::Ne => vec!["  cmp rax, rdi\n", "  setne al\n", "  movzb rax, al\n"],
                OpKind::Lt => vec!["  cmp rax, rdi\n", "  setl al\n", "  movzb rax, al\n"],
                // must be OpKind::Le
                _ => vec!["  cmp rax, rdi\n", "  setle al\n", "  movzb rax, al\n"],
            },
            vec!["    push rax\n"],
        ]
        .concat()
        .into_iter()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .join(""),
    }
}

fn error(source: &str, position: usize, expected: Vec<String>) -> String {
    vec![
        "Failed to compile:\n".to_string(),
        source.to_string(),
        "\n".to_string(),
        " ".repeat(position),
        "^\n".to_string(),
        " ".repeat(position),
        "expected: ".to_string(),
        expected.join(", "),
    ]
    .join("")
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
