#[derive(PartialEq, Debug, Clone)]
pub enum OpKind {
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
pub enum AST {
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
