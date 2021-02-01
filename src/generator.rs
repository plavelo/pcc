use crate::ast::*;
use std::collections::HashMap;

pub fn generate(asts: Vec<AST>) -> String {
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
        "# prologue",
        "  push rbp",
        "  mov rbp, rsp",
        "# allocate >>>>>",
        format!("  sub rsp, {}", env.offset()).as_str(),
        "# <<<<< allocate",
        acc.join("\n").as_str(),
        // epilogue
        "# epilogue",
        "  mov rsp, rbp",
        "  pop rbp",
        "  ret",
    ]
    .join("\n")
}

#[derive(PartialEq, Debug, Clone, Default)]
struct Environment {
    vars: HashMap<String, usize>,
    counter: usize,
}
impl Environment {
    fn offset(&self) -> usize {
        *self.vars.values().max().unwrap_or(&0)
    }

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
        AST::Literal { value } => (
            vec![
                "# literal >>>>>",
                format!("  push {}", value).as_str(),
                "# <<<<< literal",
            ]
            .join("\n"),
            env,
        ),
        AST::Variable { name: _ } => {
            let (gen, env) = gen_lvar(tree, env);
            (
                vec![
                    "# variable >>>>>",
                    gen.as_str(),
                    "  pop rax",
                    "  mov rax, [rax]",
                    "  push rax",
                    "# <<<<< variable",
                ]
                .join("\n"),
                env,
            )
        }
        AST::Assign { lhs, rhs } => {
            let (lhs_gen, env) = gen_lvar(*lhs, env);
            let (rhs_gen, env) = gen(*rhs, env);
            (
                vec![
                    "# assign >>>>>",
                    "# variable >>>>>",
                    lhs_gen.as_str(),
                    "# <<<<< variable",
                    rhs_gen.as_str(),
                    "  pop rdi",
                    "  pop rax",
                    "  mov [rax], rdi",
                    "  push rdi",
                    "# <<<<< assign",
                ]
                .join("\n"),
                env,
            )
        }
        AST::If { cond, then, els } => {
            let (els_label, env) = env.obtain_label(".Lifelse");
            let (end_label, env) = env.obtain_label(".Lifend");
            let (cond_gen, env) = gen(*cond, env);
            let (then_gen, env) = gen(*then, env);
            if let Some(ast) = *els {
                let (els_gen, env) = gen(ast, env);
                (
                    vec![
                        "# if >>>>>".to_string(),
                        "# if-cond >>>>>".to_string(),
                        cond_gen,
                        "# <<<<< if-cond".to_string(),
                        "  pop rax".to_string(),
                        "  cmp rax, 0".to_string(),
                        format!("  je {}", els_label),
                        "# if-then >>>>>".to_string(),
                        then_gen,
                        "# <<<<< if-then".to_string(),
                        format!("  jmp {}", end_label),
                        format!("{}:", els_label),
                        "# if-else >>>>>".to_string(),
                        els_gen,
                        "# <<<<< if-else".to_string(),
                        format!("{}:", end_label),
                        "# <<<<< if".to_string(),
                    ]
                    .join("\n"),
                    env,
                )
            } else {
                (
                    vec![
                        "# if >>>>>".to_string(),
                        "# if-cond >>>>>".to_string(),
                        cond_gen,
                        "# <<<<< if-cond".to_string(),
                        "  pop rax".to_string(),
                        "  cmp rax, 0".to_string(),
                        format!("  je {}", end_label),
                        "# if-then >>>>>".to_string(),
                        then_gen,
                        "# <<<<< if-then".to_string(),
                        format!("{}:", end_label),
                        "# <<<<< if".to_string(),
                    ]
                    .join("\n"),
                    env,
                )
            }
        }
        AST::While { cond, then } => {
            let (bgn_label, env) = env.obtain_label(".Lwhilebgn");
            let (end_label, env) = env.obtain_label(".Lwhileend");
            let (cond_gen, env) = gen(*cond, env);
            let (then_gen, env) = gen(*then, env);
            (
                vec![
                    "# while >>>>>".to_string(),
                    format!("{}:", bgn_label),
                    "# while-cond >>>>>".to_string(),
                    cond_gen,
                    "# <<<<< while-cond".to_string(),
                    "  pop rax".to_string(),
                    "  cmp rax, 0".to_string(),
                    format!("  je {}", end_label),
                    "# while-then >>>>>".to_string(),
                    then_gen,
                    "# <<<<< while-then".to_string(),
                    format!("  jmp {}", bgn_label),
                    format!("{}:", end_label),
                    "# <<<<< while".to_string(),
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
            let (bgn_label, env) = env.obtain_label(".Lforbgn");
            let (end_label, env) = env.obtain_label(".Lforend");
            let (init_gen, env) = if let Some(ast) = *init {
                let (gen, _env) = gen(ast, env);
                (
                    vec![
                        "# for-init >>>>>".to_string(),
                        gen,
                        "# <<<<< for-init".to_string(),
                    ],
                    _env,
                )
            } else {
                (vec!["# for-init (skip)".to_string()], env)
            };
            let (cond_gen, env) = if let Some(ast) = *cond {
                let (gen, _env) = gen(ast, env);
                (
                    vec![
                        "# for-cond >>>>>".to_string(),
                        gen,
                        "# <<<<< for-cond".to_string(),
                        "  pop rax".to_string(),
                        "  cmp rax, 0".to_string(),
                        format!("  je {}", end_label),
                    ],
                    _env,
                )
            } else {
                (vec!["# for-cond (skip)".to_string()], env)
            };
            let (then_gen, env) = {
                let (gen, _env) = gen(*then, env);
                (
                    vec![
                        "# for-then >>>>>".to_string(),
                        gen,
                        "# <<<<< for-then".to_string(),
                    ],
                    _env,
                )
            };
            let (inc_gen, env) = if let Some(ast) = *inc {
                let (gen, _env) = gen(ast, env);
                (
                    vec![
                        "# for-inc >>>>>".to_string(),
                        gen,
                        "# <<<<< for-inc".to_string(),
                    ],
                    _env,
                )
            } else {
                (vec!["# for-inc (skip)".to_string()], env)
            };
            (
                vec![
                    vec!["# for >>>>>".to_string(), "# for-init >>>>>".to_string()],
                    init_gen,
                    vec![
                        "# <<<<< for-init".to_string(),
                        format!("{}:", bgn_label),
                        "# for-cond >>>>>".to_string(),
                    ],
                    cond_gen,
                    vec![
                        "# <<<<< for-cond".to_string(),
                        "# for-then >>>>>".to_string(),
                    ],
                    then_gen,
                    vec![
                        "# <<<<< for-then".to_string(),
                        "# for-inc >>>>>".to_string(),
                    ],
                    inc_gen,
                    vec![
                        "# <<<<< for-inc".to_string(),
                        format!("  jmp {}", bgn_label),
                        format!("{}:", end_label),
                        "# <<<<< for".to_string(),
                    ],
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
                    "# return >>>>>",
                    lhs_gen.as_str(),
                    "  pop rax",
                    "  mov rsp, rbp",
                    "  pop rbp",
                    "  ret",
                    "# <<<<< return",
                ]
                .join("\n"),
                env,
            )
        }
        AST::Block { stmts } => {
            let mut next_env = env;
            let mut acc = vec!["# block >>>>>".to_string()];
            for stmt in stmts.into_iter() {
                let (gen, _env) = gen(stmt, next_env);
                next_env = _env;
                acc.push(gen);
            }
            acc.push("# <<<<< block".to_string());
            (acc.join("\n"), next_env)
        }
        AST::Operator { kind, lhs, rhs } => {
            let (lhs_gen, env) = gen(*lhs, env);
            let (rhs_gen, env) = gen(*rhs, env);
            (
                [
                    vec![
                        lhs_gen.as_str(),
                        rhs_gen.as_str(),
                        "# operator >>>>>",
                        "  pop rdi",
                        "  pop rax",
                    ],
                    match kind {
                        OpKind::Add => vec!["# add >>>>>", "  add rax, rdi", "# <<<<< add"],
                        OpKind::Sub => vec!["# sub >>>>>", "  sub rax, rdi", "# <<<<< sub"],
                        OpKind::Mul => vec!["# mul >>>>>", "  imul rax, rdi", "# <<<<< mul"],
                        OpKind::Div => vec!["# div >>>>>", "  cqo", "  idiv rdi", "# <<<<< div"],
                        OpKind::Eq => {
                            vec![
                                "# eq >>>>>",
                                "  cmp rax, rdi",
                                "  sete al",
                                "  movzb rax, al",
                                "# <<<<< eq",
                            ]
                        }
                        OpKind::Ne => vec![
                            "# ne >>>>>",
                            "  cmp rax, rdi",
                            "  setne al",
                            "  movzb rax, al",
                            "# <<<<< ne",
                        ],
                        OpKind::Lt => vec![
                            "# lt >>>>>",
                            "  cmp rax, rdi",
                            "  setl al",
                            "  movzb rax, al",
                            "# <<<<< lt",
                        ],
                        OpKind::Le => vec![
                            "# le >>>>>",
                            "  cmp rax, rdi",
                            "  setle al",
                            "  movzb rax, al",
                            "# <<<<< le",
                        ],
                    },
                    vec!["  push rax", "# <<<<< operator"],
                ]
                .concat()
                .join("\n"),
                env,
            )
        }
    }
}
