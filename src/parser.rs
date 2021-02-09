use crate::ast::*;
use crate::combinator::*;

pub fn parse_program(source: &str) -> Result<Success<Vec<AST>>, Failure> {
    parse(then(whitespace(), program()), source)
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

/// stmt       = stmt_expr
///            | stmt_block
///            | stmt_if
///            | stmt_while
///            | stmt_for
///            | stmt_return
#[derive(Clone)]
struct Stmt;
impl<'a> Parser<'a, AST> for Stmt {
    fn parse(&self, input: &'a str, position: usize) -> Result<Success<AST>, Failure> {
        or(
            or(
                or(or(or(stmt_expr(), stmt_block()), stmt_if()), stmt_while()),
                stmt_for(),
            ),
            stmt_return(),
        )
        .parse(input, position)
    }
}
fn stmt<'a>() -> impl Parser<'a, AST> {
    Stmt
}

/// stmt_expr  = expr ";"
fn stmt_expr<'a>() -> impl Parser<'a, AST> {
    skip(expr(), token(string(";")))
}

/// stmt_block = "{" stmt* "}"
fn stmt_block<'a>() -> impl Parser<'a, AST> {
    map(
        skip(then(token(string("{")), many(stmt())), token(string("}"))),
        |input| AST::Block { stmts: input },
    )
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
            rest.into_iter()
                .fold(init, |node, (kind, next)| AST::Operator {
                    kind: if kind == "==" { OpKind::Eq } else { OpKind::Ne },
                    lhs: Box::new(node),
                    rhs: Box::new(next),
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
            rest.into_iter()
                .fold(init, |node, (kind, next)| match kind.as_str() {
                    l @ "<" | l @ "<=" => AST::Operator {
                        kind: if l == "<" { OpKind::Lt } else { OpKind::Le },
                        lhs: Box::new(node),
                        rhs: Box::new(next),
                    },
                    g => AST::Operator {
                        kind: if g == ">" { OpKind::Lt } else { OpKind::Le },
                        lhs: Box::new(next),
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
            rest.into_iter()
                .fold(init, |node, (kind, next)| AST::Operator {
                    kind: if kind == "+" {
                        OpKind::Add
                    } else {
                        OpKind::Sub
                    },
                    lhs: Box::new(node),
                    rhs: Box::new(next),
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
            rest.into_iter()
                .fold(init, |node, (kind, next)| AST::Operator {
                    kind: if kind == "*" {
                        OpKind::Mul
                    } else {
                        OpKind::Div
                    },
                    lhs: Box::new(node),
                    rhs: Box::new(next),
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

/// ident   = 1*(ALPHA / DIGIT / "_")
///         | 1*(ALPHA / DIGIT / "_") "(" ")"
fn ident<'a>() -> impl Parser<'a, AST> {
    or(
        map(
            skip(
                skip(
                    token(regex("[a-zA-Z_][a-zA-Z0-9_]*", 0)),
                    token(string("(")),
                ),
                token(string(")")),
            ),
            |input| AST::Function { name: input },
        ),
        map(token(regex("[a-zA-Z_][a-zA-Z0-9_]*", 0)), |input| {
            AST::Variable { name: input }
        }),
    )
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
