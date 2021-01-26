mod parser;

use clap::Clap;
use parser::*;

#[derive(Clap)]
struct Opts {
    input: String,
}

fn main() {
    let opts: Opts = Opts::parse();
    let input = opts.input;
    let parser = then(whitespace(), expr());
    let result = parse(parser, &input);
    let output = match result {
        Ok(success) => vec![
            ".intel_syntax noprefix\n",
            ".globl main\n",
            "main:\n",
            &gen(Box::new(success.value)),
            "    pop rax\n",
            "    ret\n",
        ]
        .concat(),
        Err(failure) => error(&input, failure.position, failure.expected),
    };
    println!("{}", output);
}

#[derive(PartialEq, Debug, Clone)]
enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(PartialEq, Debug, Clone)]
enum SyntaxTree {
    Node {
        kind: NodeKind,
        lhs: Box<SyntaxTree>,
        rhs: Box<SyntaxTree>,
    },
    Leaf {
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
struct Expr;
impl<'a> Parser<'a, SyntaxTree> for Expr {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<SyntaxTree>, Failure> {
        map(
            and(
                mul(),
                many(or(
                    map(then(token(string("+")), mul()), |input| {
                        (NodeKind::Add, input)
                    }),
                    map(then(token(string("-")), mul()), |input| {
                        (NodeKind::Sub, input)
                    }),
                )),
            ),
            |input| {
                let (head, tail) = input;
                let mut node = head;
                for (kind, next_node) in tail.iter() {
                    node = SyntaxTree::Node {
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
fn expr<'a>() -> impl Parser<'a, SyntaxTree> {
    Expr
}

/// mul     = primary ("*" primary | "/" primary)*
fn mul<'a>() -> impl Parser<'a, SyntaxTree> {
    map(
        and(
            primary(),
            many(or(
                map(then(token(string("*")), primary()), |input| {
                    (NodeKind::Mul, input)
                }),
                map(then(token(string("/")), primary()), |input| {
                    (NodeKind::Div, input)
                }),
            )),
        ),
        |input| {
            let (head, tail) = input;
            let mut node = head;
            for (kind, next_node) in tail.iter() {
                node = SyntaxTree::Node {
                    kind: kind.clone(),
                    lhs: Box::new(node),
                    rhs: Box::new(next_node.clone()),
                }
            }
            node
        },
    )
}

/// primary = num | "(" expr ")"
fn primary<'a>() -> impl Parser<'a, SyntaxTree> {
    or(
        num(),
        then(token(string("(")), skip(expr(), token(string(")")))),
    )
}

fn num<'a>() -> impl Parser<'a, SyntaxTree> {
    map(token(regex("(0|[1-9][0-9]*)", 0)), |input| {
        SyntaxTree::Leaf {
            value: input.parse::<i32>().unwrap(),
        }
    })
}

#[allow(clippy::boxed_local)]
fn gen(tree: Box<SyntaxTree>) -> String {
    match *tree {
        SyntaxTree::Leaf { value } => format!("    push {}\n", value),
        SyntaxTree::Node { kind, lhs, rhs } => [
            vec![
                gen(lhs),
                gen(rhs),
                "    pop rdi\n".to_string(),
                "    pop rax\n".to_string(),
            ],
            match kind {
                NodeKind::Add => vec!["    add rax, rdi\n".to_string()],
                NodeKind::Sub => vec!["    sub rax, rdi\n".to_string()],
                NodeKind::Mul => vec!["    imul rax, rdi\n".to_string()],
                NodeKind::Div => vec!["    cqo\n".to_string(), "    idiv rdi\n".to_string()],
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
            SyntaxTree::Node {
                kind: NodeKind::Add,
                lhs: Box::new(SyntaxTree::Leaf { value: 1 }),
                rhs: Box::new(SyntaxTree::Node {
                    kind: NodeKind::Div,
                    lhs: Box::new(SyntaxTree::Node {
                        kind: NodeKind::Mul,
                        lhs: Box::new(SyntaxTree::Leaf { value: 2 }),
                        rhs: Box::new(SyntaxTree::Leaf { value: 3 }),
                    }),
                    rhs: Box::new(SyntaxTree::Leaf { value: 2 }),
                }),
            },
        );
    }

    #[test]
    fn num_ok() {
        let parser = then(whitespace(), num());
        let result = parse(parser, "   123   ");
        assert_eq!(result.is_ok(), true);
        assert_eq!(result.value(), SyntaxTree::Leaf { value: 123 });
    }
}
