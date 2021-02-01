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
        AST::Block { stmts } => {
            let mut next_env = env;
            let mut acc = vec![];
            for stmt in stmts.into_iter() {
                let (gen, _env) = gen(stmt, next_env);
                next_env = _env;
                acc.push(gen);
            }
            (acc.join("\n"), next_env)
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
