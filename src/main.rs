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
    let mut env = Environment {
        vars: HashMap::new(),
        counter: 0,
    };
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

#[derive(PartialEq, Debug, Clone)]
struct Environment {
    vars: HashMap<String, usize>,
    counter: usize,
}
impl Environment {
    fn obtain_offset(&self, name: String) -> (usize, Environment) {
        match self.vars.get(&name) {
            Some(offset) => (*offset, self.clone()),
            None => {
                let next_offset = match self.vars.values().max() {
                    Some(offset) => offset + 8,
                    None => 8,
                };
                let next_vars = self
                    .vars
                    .clone()
                    .into_iter()
                    .chain(vec![(name.clone(), next_offset)])
                    .collect();
                (
                    next_offset,
                    Environment {
                        vars: next_vars,
                        counter: self.counter,
                    },
                )
            }
        }
    }

    fn obtain_label(&self, prefix: &str) -> (String, Environment) {
        (
            format!("{}{}", prefix, self.counter),
            Environment {
                vars: self.vars.clone(),
                counter: self.counter + 1,
            },
        )
    }

    fn offset(&self) -> usize {
        *self.vars.values().max().unwrap_or(&0)
    }
}

fn gen_lvar(tree: AST, env: Environment) -> (String, Environment) {
    match tree {
        AST::Variable { name } => {
            let (offset, next_env) = env.obtain_offset(name);
            (
                vec![
                    "  mov rax, rbp",
                    format!("  sub rax, {}", offset).as_str(),
                    "  push rax",
                ]
                .join("\n"),
                next_env,
            )
        }
        _ => panic!("The left side value of the assignment is not a variable."),
    }
}

fn gen(tree: AST, env: Environment) -> (String, Environment) {
    match tree {
        AST::Literal { value } => (format!("  push {}", value), env),
        AST::Variable { name: _ } => {
            let (gen, env) = gen_lvar(tree, env);
            (
                vec![gen.as_str(), "  pop rax", "  mov rax, [rax]", "  push rax"].join("\n"),
                env,
            )
        }
        AST::Assign { lhs, rhs } => {
            let (lhs_gen, env) = gen_lvar(*lhs, env);
            let (rhs_gen, env) = gen(*rhs, env);
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
                env,
            )
        }
        AST::If { cond, then, els } => {
            let (els_label, env) = env.obtain_label(".Lelse");
            let (end_label, env) = env.obtain_label(".Lend");
            let (cond_gen, env) = gen(*cond, env);
            let (then_gen, env) = gen(*then, env);
            if let Some(ast) = *els {
                let (els_gen, env) = gen(ast, env);
                (
                    vec![
                        cond_gen,
                        "  pop rax".to_string(),
                        "  cmp rax, 0".to_string(),
                        format!("  je {}", els_label),
                        then_gen,
                        format!("  jmp {}", end_label),
                        format!("{}:", els_label),
                        els_gen,
                        format!("{}:", end_label),
                    ]
                    .join("\n"),
                    env,
                )
            } else {
                (
                    vec![
                        cond_gen,
                        "  pop rax".to_string(),
                        "  cmp rax, 0".to_string(),
                        format!("  je {}", end_label),
                        then_gen,
                        format!("{}:", end_label),
                    ]
                    .join("\n"),
                    env,
                )
            }
        }
        AST::While { cond, then } => {
            let (bgn_label, env) = env.obtain_label(".Lbgn");
            let (end_label, env) = env.obtain_label(".Lend");
            let (cond_gen, env) = gen(*cond, env);
            let (then_gen, env) = gen(*then, env);
            (
                vec![
                    format!("{}:", bgn_label),
                    cond_gen,
                    "  pop rax".to_string(),
                    "  cmp rax, 0".to_string(),
                    format!("  je {}", end_label),
                    then_gen,
                    format!("  jmp {}", bgn_label),
                    format!("{}:", end_label),
                ]
                .join("\n"),
                env,
            )
        }
        AST::For {
            init,
            cond,
            inc,
            then,
        } => {
            let (bgn_label, env) = env.obtain_label(".Lbgn");
            let (end_label, env) = env.obtain_label(".Lend");
            let (init_gen, env) = if let Some(ast) = *init {
                let (gen, _env) = gen(ast, env);
                (vec![gen], _env)
            } else {
                (vec![], env)
            };
            let (cond_gen, env) = if let Some(ast) = *cond {
                let (gen, _env) = gen(ast, env);
                (
                    vec![vec![
                        gen.as_str(),
                        "  pop rax",
                        "  cmp rax, 0",
                        format!("  je {}", end_label).as_str(),
                    ]
                    .join("\n")],
                    _env,
                )
            } else {
                (vec![], env)
            };
            let (then_gen, env) = {
                let (gen, _env) = gen(*then, env);
                (vec![gen], _env)
            };
            let (inc_gen, env) = if let Some(ast) = *inc {
                let (gen, _env) = gen(ast, env);
                (vec![gen], _env)
            } else {
                (vec![], env)
            };
            (
                vec![
                    init_gen,
                    vec![format!("{}:", bgn_label)],
                    cond_gen,
                    then_gen,
                    inc_gen,
                    vec![format!("  jmp {}", bgn_label), format!("{}:", end_label)],
                ]
                .concat()
                .join("\n"),
                env,
            )
        }
        AST::Return { lhs } => {
            let (lhs_gen, env) = gen(*lhs, env);
            (
                vec![
                    lhs_gen.as_str(),
                    "  pop rax",
                    "  mov rsp, rbp",
                    "  pop rbp",
                    "  ret",
                ]
                .join("\n"),
                env,
            )
        }
        AST::Operator { kind, lhs, rhs } => {
            let (lhs_gen, env) = gen(*lhs, env);
            let (rhs_gen, env) = gen(*rhs, env);
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
                env,
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
