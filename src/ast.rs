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
    Address {
        lhs: Box<AST>,
    },
    Assign {
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    Block {
        stmts: Vec<AST>,
    },
    Call {
        name: String,
        args: Vec<AST>,
    },
    Definition {
        name: String,
    },
    Dereference {
        lhs: Box<AST>,
    },
    For {
        init: Box<Option<AST>>,
        cond: Box<Option<AST>>,
        inc: Box<Option<AST>>,
        then: Box<AST>,
    },
    Function {
        name: String,
        args: Vec<AST>,
        body: Box<AST>,
    },
    If {
        cond: Box<AST>,
        then: Box<AST>,
        els: Box<Option<AST>>,
    },
    Literal {
        value: usize,
    },
    Operator {
        kind: OpKind,
        lhs: Box<AST>,
        rhs: Box<AST>,
    },
    Return {
        lhs: Box<AST>,
    },
    Variable {
        name: String,
    },
    While {
        cond: Box<AST>,
        then: Box<AST>,
    },
}
