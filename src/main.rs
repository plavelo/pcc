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
        Ok(success) => println!("{}", generate(success.value)),
        Err(failure) => panic!("{}", error(&input, failure.position, failure.expected)),
    };
}

fn generate(asts: Vec<AST>) -> String {
    let mut vars = HashMap::new();
    let mut acc = vec![];
    let mut counter = 0;
    for ast in asts {
        let (gen, next_vars, next_counter) = gen(ast, vars, counter);
        vars = next_vars;
        counter = next_counter;
        acc.push(gen);
    }
    let allocation = vars.values().max().unwrap_or(&0);
    vec![
        ".intel_syntax noprefix",
        ".globl main",
        "main:",
        // prologue, allocate memory for variables
        "  push rbp",
        "  mov rbp, rsp",
        format!("  sub rsp, {}", allocation).as_str(),
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
    If {
        cond: Box<AST>,
        then: Box<AST>,
        els: Box<Option<AST>>,
    },
    While {
        cond: Box<AST>,
        then: Box<AST>,
    },
    For {
        init: Box<Option<AST>>,
        cond: Box<Option<AST>>,
        inc: Box<Option<AST>>,
        then: Box<AST>,
    },
    Return {
        lhs: Box<AST>,
    },
}

fn gen_lvar(tree: AST, vars: HashMap<String, usize>) -> (String, HashMap<String, usize>) {
    match tree {
        AST::Variable { name } => {
            let (next_offset, next_vars) = match vars.get(&name) {
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
                    format!("  sub rax, {}", next_offset).as_str(),
                    "  push rax",
                ]
                .join("\n"),
                next_vars,
            )
        }
        _ => panic!("The left side value of the assignment is not a variable."),
    }
}

fn gen(
    tree: AST,
    vars: HashMap<String, usize>,
    counter: usize,
) -> (String, HashMap<String, usize>, usize) {
    match tree {
        AST::Literal { value } => (format!("  push {}", value), vars, counter),
        AST::Variable { name: _ } => {
            let (gen, next_vars) = gen_lvar(tree, vars);
            (
                vec![gen.as_str(), "  pop rax", "  mov rax, [rax]", "  push rax"].join("\n"),
                next_vars,
                counter,
            )
        }
        AST::Assign { lhs, rhs } => {
            let (lhs_gen, lhs_vars) = gen_lvar(*lhs, vars);
            let (rhs_gen, rhs_vars, rhs_counter) = gen(*rhs, lhs_vars, counter);
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
                rhs_counter,
            )
        }
        AST::If { cond, then, els } => {
            let els_label = format!(".Lelse{}", counter);
            let end_label = format!(".Lend{}", counter + 1);
            let next_counter = counter + 2;
            let (cond_gen, cond_vars, cond_counter) = gen(*cond, vars, next_counter);
            let (then_gen, then_vars, then_counter) = gen(*then, cond_vars, cond_counter);
            let (els_gen, els_vars, els_counter) = if let Some(ast) = *els {
                let (g, v, c) = gen(ast, then_vars, then_counter);
                (
                    vec![
                        format!("  je {}", els_label),
                        then_gen,
                        format!("  jmp {}", end_label),
                        format!("{}:", els_label),
                        g,
                    ]
                    .join("\n"),
                    v,
                    c,
                )
            } else {
                (
                    vec![format!("  je {}", end_label), then_gen].join("\n"),
                    then_vars,
                    then_counter,
                )
            };
            (
                vec![
                    cond_gen,
                    "  pop rax".to_string(),
                    "  cmp rax, 0".to_string(),
                    els_gen,
                    format!("{}:", end_label),
                ]
                .join("\n"),
                els_vars,
                els_counter,
            )
        }
        AST::While { cond, then } => {
            let begin_label = format!(".Lbegin{}", counter + 1);
            let end_label = format!(".Lend{}", counter + 2);
            let next_counter = counter + 3;
            let (cond_gen, cond_vars, cond_counter) = gen(*cond, vars, next_counter);
            let (then_gen, then_vars, then_counter) = gen(*then, cond_vars, cond_counter);
            (
                vec![
                    format!("{}:", begin_label),
                    cond_gen,
                    "  pop rax".to_string(),
                    "  cmp rax, 0".to_string(),
                    format!("  je {}", end_label),
                    then_gen,
                    format!("  jmp {}", begin_label),
                    format!("{}:", end_label),
                ]
                .join("\n"),
                then_vars,
                then_counter,
            )
        }
        AST::For {
            init,
            cond,
            inc,
            then,
        } => {
            let begin_label = format!(".Lbegin{}", counter + 1);
            let end_label = format!(".Lend{}", counter + 2);
            let next_counter = counter + 3;
            let (init_gen, init_vars, init_counter) = if let Some(ast) = *init {
                gen(ast, vars, next_counter)
            } else {
                ("".to_string(), vars, next_counter)
            };
            let (cond_gen, cond_vars, cond_counter) = if let Some(ast) = *cond {
                let (g, v, c) = gen(ast, init_vars, init_counter);
                (
                    vec![
                        g.as_str(),
                        "  pop rax",
                        "  cmp rax, 0",
                        format!("  je {}", end_label).as_str(),
                    ]
                    .join("\n"),
                    v,
                    c,
                )
            } else {
                ("".to_string(), init_vars, init_counter)
            };
            let (then_gen, then_vars, then_counter) = gen(*then, cond_vars, cond_counter);
            let (inc_gen, inc_vars, inc_counter) = if let Some(ast) = *inc {
                gen(ast, then_vars, then_counter)
            } else {
                ("".to_string(), then_vars, then_counter)
            };
            (
                vec![
                    if init_gen.is_empty() {
                        vec![]
                    } else {
                        vec![init_gen]
                    },
                    vec![format!("{}:", begin_label)],
                    if cond_gen.is_empty() {
                        vec![]
                    } else {
                        vec![cond_gen]
                    },
                    vec![then_gen],
                    if inc_gen.is_empty() {
                        vec![]
                    } else {
                        vec![inc_gen]
                    },
                    vec![format!("  jmp {}", begin_label), format!("{}:", end_label)],
                ]
                .concat()
                .join("\n"),
                inc_vars,
                inc_counter,
            )
        }
        AST::Return { lhs } => {
            let (lhs_gen, lhs_vars, lhs_counter) = gen(*lhs, vars, counter);
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
                lhs_counter,
            )
        }
        AST::Operator { kind, lhs, rhs } => {
            let (lhs_gen, lhs_vars, lhs_counter) = gen(*lhs, vars, counter);
            let (rhs_gen, rhs_vars, rhs_counter) = gen(*rhs, lhs_vars, lhs_counter);
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
                .join("\n"),
                rhs_vars,
                rhs_counter,
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

/// stmt_expr  = expr ";"
fn stmt_expr<'a>() -> impl Parser<'a, AST> {
    skip(expr(), token(string(";")))
}

/// stmt_if    = "if" "(" expr ")" stmt ("else" stmt)?
fn stmt_if<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            and(
                skip(
                    then(and(token(string("if")), token(string("("))), expr()),
                    token(string(")")),
                ),
                stmt(),
            ),
            at_most(then(token(string("else")), stmt()), 1),
        ),
        |((cond, then), els)| AST::If {
            cond: Box::new(cond),
            then: Box::new(then),
            els: Box::new(match els.first() {
                Some(ast) => Some(ast.clone()),
                None => None,
            }),
        },
    )
}

/// stmt_while = "while" "(" expr ")" stmt
fn stmt_while<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            skip(
                then(and(token(string("while")), token(string("("))), expr()),
                token(string(")")),
            ),
            stmt(),
        ),
        |(cond, then)| AST::While {
            cond: Box::new(cond),
            then: Box::new(then),
        },
    )
}

/// stmt_for = "for" "(" expr? ";" expr? ";" expr? ")" stmt
fn stmt_for<'a>() -> impl Parser<'a, AST> {
    map(
        and(
            and(
                and(
                    skip(
                        then(
                            and(token(string("for")), token(string("("))),
                            at_most(expr(), 1),
                        ),
                        token(string(";")),
                    ),
                    skip(at_most(expr(), 1), token(string(";"))),
                ),
                skip(at_most(expr(), 1), token(string(")"))),
            ),
            stmt(),
        ),
        |(((init, cond), inc), then)| AST::For {
            init: Box::new(match init.first() {
                Some(ast) => Some(ast.clone()),
                None => None,
            }),
            cond: Box::new(match cond.first() {
                Some(ast) => Some(ast.clone()),
                None => None,
            }),
            inc: Box::new(match inc.first() {
                Some(ast) => Some(ast.clone()),
                None => None,
            }),
            then: Box::new(then),
        },
    )
}

/// stmt_return = "return" expr ";"
fn stmt_return<'a>() -> impl Parser<'a, AST> {
    map(
        skip(
            then(
                token(not_followed_by(string("return"), regex("[a-zA-Z0-9_]", 0))),
                expr(),
            ),
            token(string(";")),
        ),
        |input| AST::Return {
            lhs: Box::new(input),
        },
    )
}

/// stmt       = stmt_expr
///            | stmt_if
///            | stmt_while
///            | stmt_for
///            | stmt_return
#[derive(Clone)]
struct Stmt;
impl<'a> Parser<'a, AST> for Stmt {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<AST>, Failure> {
        or(
            or(or(or(stmt_expr(), stmt_if()), stmt_while()), stmt_for()),
            stmt_return(),
        )
        .parse(input, position)
    }
}
fn stmt<'a>() -> impl Parser<'a, AST> {
    Stmt
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
    fn stmt_if_ok() {
        let parser = stmt_if();
        let result = parse(parser, "if (first < 3) second = first / 2;");
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            AST::If {
                cond: Box::new(AST::Operator {
                    kind: OpKind::Lt,
                    lhs: Box::new(AST::Variable {
                        name: "first".to_string()
                    }),
                    rhs: Box::new(AST::Literal { value: 3 }),
                }),
                then: Box::new(AST::Assign {
                    lhs: Box::new(AST::Variable {
                        name: "second".to_string()
                    }),
                    rhs: Box::new(AST::Operator {
                        kind: OpKind::Div,
                        lhs: Box::new(AST::Variable {
                            name: "first".to_string()
                        }),
                        rhs: Box::new(AST::Literal { value: 2 }),
                    }),
                }),
                els: Box::new(None),
            }
        );

        let parser = stmt_if();
        let result = parse(
            parser,
            "if (first < 3) second = first / 2; else second = first * 2;",
        );
        assert_eq!(result.is_ok(), true);
        assert_eq!(
            result.value(),
            AST::If {
                cond: Box::new(AST::Operator {
                    kind: OpKind::Lt,
                    lhs: Box::new(AST::Variable {
                        name: "first".to_string()
                    }),
                    rhs: Box::new(AST::Literal { value: 3 }),
                }),
                then: Box::new(AST::Assign {
                    lhs: Box::new(AST::Variable {
                        name: "second".to_string()
                    }),
                    rhs: Box::new(AST::Operator {
                        kind: OpKind::Div,
                        lhs: Box::new(AST::Variable {
                            name: "first".to_string()
                        }),
                        rhs: Box::new(AST::Literal { value: 2 }),
                    }),
                }),
                els: Box::new(Some(AST::Assign {
                    lhs: Box::new(AST::Variable {
                        name: "second".to_string()
                    }),
                    rhs: Box::new(AST::Operator {
                        kind: OpKind::Mul,
                        lhs: Box::new(AST::Variable {
                            name: "first".to_string()
                        }),
                        rhs: Box::new(AST::Literal { value: 2 }),
                    }),
                })),
            }
        );
    }

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
