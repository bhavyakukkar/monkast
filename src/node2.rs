use kast_ast::Token;

type IdentType = String;

/// Node2 is obtained after the following conversions have been performed on Node
/// 1. BindVal.binding: Box<Self> --> BindVal.binding: Box<IdentType>
/// 2. validating that Fn args are
/// 3. flattening tuples from their recursive trees into a vec of expressions
pub enum Node2 {
    Simple(Token),

    Paren(Box<Self>),

    Add((Box<Self>, Box<Self>)),
    Sub((Box<Self>, Box<Self>)),
    Mul((Box<Self>, Box<Self>)),
    Div((Box<Self>, Box<Self>)),
    Mod((Box<Self>, Box<Self>)),
    Pow((Box<Self>, Box<Self>)),
    Eq((Box<Self>, Box<Self>)),
    Neq((Box<Self>, Box<Self>)),
    Gt((Box<Self>, Box<Self>)),
    Gte((Box<Self>, Box<Self>)),
    Lt((Box<Self>, Box<Self>)),
    Lte((Box<Self>, Box<Self>)),

    Fn {
        args: Vec<IdentType>,
        body: Option<Box<Self>>,
    },
    Statement(Box<Self>, Option<Box<Self>>),
    BindVal {
        binding: IdentType,
        value: Box<Self>,
    },
    Tuple(Vec<Self>),
    Map(Box<Self>),
    MapField {
        key: Box<Self>,
        value: Box<Self>,
    },
    Arr(Vec<Self>),
    ArrIndex {
        arr: Box<Self>,
        index: Box<Self>,
    },
    FnCall {
        fn_bind: Box<Self>,
        args: Option<Box<Self>>,
    },
    If {
        condition: Box<Self>,
        if_expr: Box<Self>,
        else_expr: Option<Box<Self>>,
    },
    Return(Option<Box<Self>>),
}
