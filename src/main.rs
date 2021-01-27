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
    Add,
    Sub,
    Mul,
    Div,
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

/// expr    = mul ("+" mul | "-" mul)*
#[derive(Clone)]
struct Expr;
impl<'a> Parser<'a, AST> for Expr {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<AST>, Failure> {
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
        .parse(input, position)
    }
}
fn expr<'a>() -> impl Parser<'a, AST> {
    Expr
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
                gen(*lhs),
                gen(*rhs),
                "    pop rdi\n".to_string(),
                "    pop rax\n".to_string(),
            ],
            match kind {
                OpKind::Add => vec!["    add rax, rdi\n".to_string()],
                OpKind::Sub => vec!["    sub rax, rdi\n".to_string()],
                OpKind::Mul => vec!["    imul rax, rdi\n".to_string()],
                OpKind::Div => vec!["    cqo\n".to_string(), "    idiv rdi\n".to_string()],
            },
            vec!["    push rax\n".to_string()],
        ]
        .concat()
        .join(""),
    }
}

fn error(source: &str, position: usize, expected: Vec<String>) -> String {
    vec![
        source.to_string(),
        "\n".to_string(),
        " ".repeat(position),
        "^\n".to_string(),
        " ".repeat(position),
        expected.join(""),
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
