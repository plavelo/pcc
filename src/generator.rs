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
        acc.join("\n").as_str(),
    ]
    .join("\n")
}

#[derive(PartialEq, Debug, Clone, Default)]
struct Environment {
    vars: HashMap<String, usize>,
    alignment: usize,
    label_counter: usize,
}
impl Environment {
    fn local_env(&self) -> Self {
        Environment {
            vars: HashMap::new(),
            alignment: 0,
            label_counter: self.label_counter,
        }
    }

    fn take_env(&self, env: Environment) -> Self {
        Environment {
            vars: self.vars.clone(),
            alignment: self.alignment,
            label_counter: env.label_counter,
        }
    }

    fn obtain_var_offset(&self, name: String) -> (usize, Self) {
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
                        alignment: self.alignment,
                        label_counter: self.label_counter,
                    },
                )
            }
        }
    }

    fn sp_offset(&self) -> usize {
        self.alignment
    }

    fn move_sp_offset(&self, value: isize) -> (usize, Self) {
        let next_sp_offset = (self.alignment as isize + value) as usize;
        (
            next_sp_offset,
            Environment {
                vars: self.vars.clone(),
                alignment: next_sp_offset,
                label_counter: self.label_counter,
            },
        )
    }

    fn obtain_label(&self, prefix: &str) -> (String, Self) {
        (
            format!("{}{}", prefix, self.label_counter),
            Environment {
                vars: self.vars.clone(),
                alignment: self.alignment,
                label_counter: self.label_counter + 1,
            },
        )
    }
}

fn zip_with_reg<T>(vec: Vec<T>) -> Vec<(T, String)> {
    vec.into_iter()
        .zip(
            ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
                .iter()
                .map(|n| n.to_string()),
        )
        .collect()
}

fn gen_push(reg: &str, env: Environment) -> (String, Environment) {
    let (_, next_env) = env.move_sp_offset(8);
    (format!("  push {}", reg), next_env)
}

fn gen_pop(reg: &str, env: Environment) -> (String, Environment) {
    let (_, next_env) = env.move_sp_offset(-8);
    (format!("  pop {}", reg), next_env)
}

fn gen_alloc(args: Vec<AST>) -> (usize, Vec<String>) {
    let mut map = HashMap::new();
    for arg in args {
        match arg {
            AST::Variable { name } => {
                map.insert(name, 8);
            }
            _ => panic!("The value is not a variable."),
        }
    }
    (map.values().cloned().sum(), map.keys().cloned().collect())
}

fn gen_lvar(name: String, env: Environment) -> (String, Environment) {
    let (offset, next_env) = env.obtain_var_offset(name);
    let (push_rax, next_env) = gen_push("rax", next_env);
    (
        vec![
            "  mov rax, rbp".to_string(),
            format!("  sub rax, {}", offset),
            push_rax,
        ]
        .join("\n"),
        next_env,
    )
}

fn gen(tree: AST, env: Environment) -> (String, Environment) {
    match tree {
        AST::Address { lhs } => match *lhs {
            AST::Variable { name } => gen_lvar(name, env),
            _ => panic!("The value is not a variable."),
        },
        AST::Assign { lhs, rhs } => {
            let (lhs_gen, env) = match *lhs {
                AST::Variable { name } => gen_lvar(name, env),
                _ => panic!("The left side value of the assignment is not a variable."),
            };
            let (rhs_gen, env) = gen(*rhs, env);
            let (pop_rdi, env) = gen_pop("rdi", env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (push_rdi, env) = gen_push("rdi", env);
            (
                vec![
                    "# assign >>>>>",
                    "# variable >>>>>",
                    lhs_gen.as_str(),
                    "# <<<<< variable",
                    rhs_gen.as_str(),
                    pop_rdi.as_str(),
                    pop_rax.as_str(),
                    "  mov [rax], rdi",
                    push_rdi.as_str(),
                    "# <<<<< assign",
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
        AST::Call { name, args } => {
            let mut next_env = env;
            let mut acc = vec!["# calling >>>>>".to_string()];
            for (arg, reg) in zip_with_reg(args) {
                let (gen, _env) = gen(arg, next_env);
                let (pop_reg, _env) = gen_pop(reg.as_str(), _env);
                next_env = _env;
                acc.push(gen);
                acc.push(pop_reg);
            }
            // bsp must be divisible by 16.
            let adjustment = next_env.sp_offset() % 16;
            if adjustment > 0 {
                acc.push(format!("  sub rsp, {}", 16 - adjustment));
            }
            acc.push(format!("  call {}", name));
            if adjustment > 0 {
                acc.push(format!("  add rsp, {}", 16 - adjustment));
            }
            let (push_rax, next_env) = gen_push("rax", next_env);
            acc.push(push_rax);
            acc.push("# <<<<< calling".to_string());
            (acc.join("\n"), next_env)
        }
        AST::Dereference { lhs } => {
            let (gen_lhs, env) = gen(*lhs, env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (push_rax, env) = gen_push("rax", env);
            (
                vec![gen_lhs, pop_rax, "  mov rax, [rax]".to_string(), push_rax].join("\n"),
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
                let (pop_rax, _env) = gen_pop("rax", _env);
                (
                    vec![
                        "# for-cond >>>>>".to_string(),
                        gen,
                        "# <<<<< for-cond".to_string(),
                        pop_rax,
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
        AST::Function { name, args, body } => {
            let local_env = env.local_env();
            let mut acc = vec![];
            let (push_rbp, mut local_env) = gen_push("rbp", local_env);
            let (alloc, names) = gen_alloc(args);
            for (name, reg) in zip_with_reg(names) {
                let (gen, _env) = gen_lvar(name, local_env);
                let (pop_rax, _env) = gen_pop("rax", _env);
                local_env = _env;
                acc.push(
                    vec![
                        "# assign argument >>>>>",
                        "# variable >>>>>",
                        gen.as_str(),
                        "# <<<<< variable",
                        pop_rax.as_str(),
                        format!("  mov [rax], {}", reg).as_str(),
                        "# <<<<< assign",
                    ]
                    .join("\n"),
                );
            }
            let (gen, local_env) = gen(*body, local_env);
            (
                vec![
                    vec![
                        "# function >>>>>".to_string(),
                        format!("{}:", name),
                        "# prologue >>>>>".to_string(),
                        push_rbp,
                        "  mov rbp, rsp".to_string(),
                        "# <<<<< prologue".to_string(),
                        "# allocate >>>>>".to_string(),
                        format!("  sub rsp, {}", alloc),
                        "# <<<<< allocate".to_string(),
                    ],
                    acc,
                    vec![gen, "# <<<<< function".to_string()],
                ]
                .concat()
                .join("\n"),
                env.take_env(local_env),
            )
        }
        AST::If { cond, then, els } => {
            let (els_label, env) = env.obtain_label(".Lifelse");
            let (end_label, env) = env.obtain_label(".Lifend");
            let (cond_gen, env) = gen(*cond, env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (then_gen, env) = gen(*then, env);
            if let Some(ast) = *els {
                let (els_gen, env) = gen(ast, env);
                (
                    vec![
                        "# if >>>>>",
                        "# if-cond >>>>>",
                        cond_gen.as_str(),
                        "# <<<<< if-cond",
                        pop_rax.as_str(),
                        "  cmp rax, 0",
                        format!("  je {}", els_label).as_str(),
                        "# if-then >>>>>",
                        then_gen.as_str(),
                        "# <<<<< if-then",
                        format!("  jmp {}", end_label).as_str(),
                        format!("{}:", els_label).as_str(),
                        "# if-else >>>>>",
                        els_gen.as_str(),
                        "# <<<<< if-else",
                        format!("{}:", end_label).as_str(),
                        "# <<<<< if",
                    ]
                    .join("\n"),
                    env,
                )
            } else {
                (
                    vec![
                        "# if >>>>>",
                        "# if-cond >>>>>",
                        cond_gen.as_str(),
                        "# <<<<< if-cond",
                        pop_rax.as_str(),
                        "  cmp rax, 0",
                        format!("  je {}", end_label).as_str(),
                        "# if-then >>>>>",
                        then_gen.as_str(),
                        "# <<<<< if-then",
                        format!("{}:", end_label).as_str(),
                        "# <<<<< if",
                    ]
                    .join("\n"),
                    env,
                )
            }
        }
        AST::Literal { value } => {
            let (push_value, env) = gen_push(value.to_string().as_str(), env);
            (
                vec!["# literal >>>>>", push_value.as_str(), "# <<<<< literal"].join("\n"),
                env,
            )
        }
        AST::Operator { kind, lhs, rhs } => {
            let (lhs_gen, env) = gen(*lhs, env);
            let (rhs_gen, env) = gen(*rhs, env);
            let (pop_rdi, env) = gen_pop("rdi", env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (push_rax, env) = gen_push("rax", env);
            (
                [
                    vec![
                        lhs_gen.as_str(),
                        rhs_gen.as_str(),
                        "# operator >>>>>",
                        pop_rdi.as_str(),
                        pop_rax.as_str(),
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
                    vec![push_rax.as_str(), "# <<<<< operator"],
                ]
                .concat()
                .join("\n"),
                env,
            )
        }
        AST::Return { lhs } => {
            let (lhs_gen, env) = gen(*lhs, env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (pop_rbp, env) = gen_pop("rbp", env);
            (
                vec![
                    "# return >>>>>",
                    lhs_gen.as_str(),
                    pop_rax.as_str(),
                    "  mov rsp, rbp",
                    pop_rbp.as_str(),
                    "  ret",
                    "# <<<<< return",
                ]
                .join("\n"),
                env,
            )
        }
        AST::Variable { name } => {
            let (gen, env) = gen_lvar(name, env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (push_rax, env) = gen_push("rax", env);
            (
                vec![
                    "# variable >>>>>",
                    gen.as_str(),
                    pop_rax.as_str(),
                    "  mov rax, [rax]",
                    push_rax.as_str(),
                    "# <<<<< variable",
                ]
                .join("\n"),
                env,
            )
        }
        AST::While { cond, then } => {
            let (bgn_label, env) = env.obtain_label(".Lwhilebgn");
            let (end_label, env) = env.obtain_label(".Lwhileend");
            let (cond_gen, env) = gen(*cond, env);
            let (pop_rax, env) = gen_pop("rax", env);
            let (then_gen, env) = gen(*then, env);
            (
                vec![
                    "# while >>>>>",
                    format!("{}:", bgn_label).as_str(),
                    "# while-cond >>>>>",
                    cond_gen.as_str(),
                    "# <<<<< while-cond",
                    pop_rax.as_str(),
                    "  cmp rax, 0",
                    format!("  je {}", end_label).as_str(),
                    "# while-then >>>>>",
                    then_gen.as_str(),
                    "# <<<<< while-then",
                    format!("  jmp {}", bgn_label).as_str(),
                    format!("{}:", end_label).as_str(),
                    "# <<<<< while",
                ]
                .join("\n"),
                env,
            )
        }
    }
}
