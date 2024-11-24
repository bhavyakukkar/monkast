// enum Node {
//     Paren { expr: Box<Node> },
//     Add { lhs: Box<Node>, rhs: Box<Node> },
//     Sub { lhs: Box<Node>, rhs: Box<Node> },
//     Mul { lhs: Box<Node>, rhs: Box<Node> },
//     Div { lhs: Box<Node>, rhs: Box<Node> },
//     Mod { lhs: Box<Node>, rhs: Box<Node> },
//     Pow { lhs: Box<Node>, rhs: Box<Node> },
//     Eq { lhs: Box<Node>, rhs: Box<Node> },

//     Fn { args: Option<IdentType> },
// }

use std::{collections::HashMap, fmt, mem};

use kast_ast::{Ast, Token};
use kast_util::Span;

#[derive(Debug, Clone)]
pub enum Node {
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
        args: Option<Box<Self>>,
        body: Option<Box<Self>>,
    },
    Statement(Box<Self>, Option<Box<Self>>),
    BindVal {
        binding: Box<Self>,
        value: Box<Self>,
    },
    ReAssign {
        binding: Box<Self>,
        value: Box<Self>,
    },
    Tuple(Box<Self>, Box<Self>),
    Map(Box<Self>),
    MapField {
        key: Box<Self>,
        value: Box<Self>,
    },
    Arr(Box<Self>),
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

impl Node {
    pub fn statements(self) -> Statements {
        Statements { node: Some(self) }
    }

    pub fn pretty_printer(self) -> NodePrettyPrinter {
        NodePrettyPrinter {
            node: self,
            indent: 0,
        }
    }
}

impl Default for Node {
    fn default() -> Self {
        Node::Simple(Token::Eof)
    }
}

#[derive(Debug)]
pub enum TryFromAstErrorKind {
    MissingNamedBinding(String),
    // ExpectedNumExprs(usize),
    UnexpectedAstName(String),
    CannotParseSyntaxDef,
    UnexpectedAstType(Ast, Ast),
}

#[derive(Debug)]
pub struct TryFromAstError {
    pub kind: TryFromAstErrorKind,
    pub loc: Span,
}

impl fmt::Display for TryFromAstError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { kind, loc } = self;
        let kind = match kind {
            //TODO: panic inside display is hilarious
            TryFromAstErrorKind::MissingNamedBinding(field) => panic!(
                "Missing named binding `{field}`. This error only originates in inconsistencies \
                between the TryFrom<&Ast>: Node impl & syntax.ks"
            ),
            //TODO: panic inside display is hilarious
            TryFromAstErrorKind::UnexpectedAstName(name) => panic!(
                "Unexpected ast root name `{name}`. This error only originates in inconsistencies \
                between the TryFrom<&Ast>: Node impl & syntax.ks"
            ),
            //TODO: panic inside display is hilarious
            TryFromAstErrorKind::CannotParseSyntaxDef => panic!(
                "Parsed something as a syntax-definition, this is not allowed in a monkey source \
                file"
            ),
            TryFromAstErrorKind::UnexpectedAstType(_, _) => format!(""),
        };

        write!(f, "error translating AST: {kind} at {loc}",)
    }
}

impl TryFrom<&Ast> for Node {
    type Error = TryFromAstError;
    fn try_from(ast: &Ast) -> Result<Self, TryFromAstError> {
        use Node::*;
        use TryFromAstErrorKind::*;

        Ok(match ast {
            Ast::Simple { token, .. } => Simple(token.clone()),
            Ast::Complex {
                definition,
                values,
                data,
            } => {
                let mut named: HashMap<&str, &Ast> = values.named().collect();
                match definition.name.as_str() {
                    "paren" => Paren(Box::new(
                        get_named_field(&mut named, "expr", data)?.try_into()?,
                    )),
                    "add" => Add((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "sub" => Sub((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "mul" => Mul((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "div" => Div((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "mod" => Mod((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "pow" => Pow((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "eq" => Eq((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "neq" => Neq((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "gt" => Gt((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "gte" => Gte((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "lt" => Lt((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),
                    "lte" => Lte((
                        Box::new(get_named_field(&mut named, "lhs", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "rhs", data)?.try_into()?),
                    )),

                    "fn" => Fn {
                        args: named
                            .remove("args")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                        body: named
                            .remove("body")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                    },

                    "statement" => Statement(
                        Box::new(get_named_field(&mut named, "expr1", data)?.try_into()?),
                        named
                            .remove("expr2")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                    ),

                    "bind_value" => BindVal {
                        binding: Box::new(
                            get_named_field(&mut named, "binding", data)?.try_into()?,
                        ),
                        value: Box::new(get_named_field(&mut named, "value", data)?.try_into()?),
                    },
                    "reassign" => ReAssign {
                        binding: Box::new(
                            get_named_field(&mut named, "binding", data)?.try_into()?,
                        ),
                        value: Box::new(get_named_field(&mut named, "value", data)?.try_into()?),
                    },
                    "tuple" => Tuple(
                        Box::new(get_named_field(&mut named, "expr1", data)?.try_into()?),
                        Box::new(get_named_field(&mut named, "expr2", data)?.try_into()?),
                    ),
                    "map" => Map(Box::new(
                        get_named_field(&mut named, "fields", data)?.try_into()?,
                    )),
                    "map_field" => MapField {
                        key: Box::new(get_named_field(&mut named, "key", data)?.try_into()?),
                        value: Box::new(get_named_field(&mut named, "value", data)?.try_into()?),
                    },
                    "arr" => Arr(Box::new(
                        get_named_field(&mut named, "items", data)?.try_into()?,
                    )),
                    "arr_index" => ArrIndex {
                        arr: Box::new(get_named_field(&mut named, "arr", data)?.try_into()?),
                        index: Box::new(get_named_field(&mut named, "index", data)?.try_into()?),
                    },
                    "fn_call" => FnCall {
                        fn_bind: Box::new(
                            get_named_field(&mut named, "fn_bind", data)?.try_into()?,
                        ),
                        args: named
                            .remove("args")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                    },
                    "if" => If {
                        condition: Box::new(
                            get_named_field(&mut named, "condition", data)?.try_into()?,
                        ),
                        if_expr: Box::new(
                            get_named_field(&mut named, "if_expr", data)?.try_into()?,
                        ),
                        else_expr: named
                            .remove("else_expr")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                    },
                    "return" => Return(
                        named
                            .remove("expr")
                            .map(|ast| ast.try_into())
                            .transpose()?
                            .map(|node| Box::new(node)),
                    ),

                    _ => {
                        return Err(TryFromAstError {
                            kind: UnexpectedAstName(definition.name.clone()),
                            loc: data.clone(),
                        });
                    }
                }
            }
            Ast::SyntaxDefinition { data, .. } => {
                return Err(TryFromAstError {
                    kind: TryFromAstErrorKind::CannotParseSyntaxDef,
                    loc: data.clone(),
                });
            }
            Ast::FromScratch { next: _, data } => {
                return Err(TryFromAstError {
                    kind: TryFromAstErrorKind::CannotParseSyntaxDef,
                    loc: data.clone(),
                });
            }
        })
    }
}

fn get_named_field<'a>(
    fields: &mut HashMap<&str, &'a Ast>,
    field: &str,
    data: &Span,
) -> Result<&'a Ast, TryFromAstError> {
    use TryFromAstErrorKind::MissingNamedBinding;
    Ok(
        fields.remove(field).ok_or(TryFromAstError {
            kind: MissingNamedBinding(field.to_owned()),
            loc: data.clone(),
        })?, // .try_into()?,
    )
}

#[derive(Clone)]
pub struct NodePrettyPrinter {
    node: Node,
    indent: usize,
}
impl NodePrettyPrinter {
    pub fn new(node: Node, indent: usize) -> Self {
        Self { node, indent }
    }
}

impl fmt::Display for NodePrettyPrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { node, indent: i } = self.clone();
        let indent = |n| " ".repeat(2 * n);

        match node {
            Node::Simple(t) => write!(
                f,
                "{}",
                match t {
                    Token::Ident { name, .. } => name,
                    Token::Punctuation { raw } => raw,
                    Token::String { contents, .. } => format!("\"{}\"", contents),
                    Token::Number { raw } => raw.to_string(),
                    Token::Comment { .. } => "".to_owned(),
                    Token::Eof => "".to_owned(),
                }
            ),
            Node::Paren(e) => write!(f, "({})", Self::new(*e, self.indent + 1)),

            Node::Add((l, r)) => write!(f, "{} + {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Sub((l, r)) => write!(f, "{} - {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Mul((l, r)) => write!(f, "{} * {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Div((l, r)) => write!(f, "{} / {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Mod((l, r)) => write!(f, "{} % {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Pow((l, r)) => write!(f, "{} ^ {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Eq((l, r)) => write!(f, "{} == {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Neq((l, r)) => write!(f, "{} != {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Gt((l, r)) => write!(f, "{} > {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Gte((l, r)) => write!(f, "{} >= {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Lt((l, r)) => write!(f, "{} < {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),
            Node::Lte((l, r)) => write!(f, "{} <= {}", Self::new(*l, i + 1), Self::new(*r, i + 1)),

            Node::Fn { args, body } => write!(
                f,
                "fn({}) {{{}}}",
                args.map(|node| Self::new(*node, i + 1).to_string())
                    .unwrap_or(String::new()),
                body.map(|node| format!(
                    "\n{}{}\n{}",
                    indent(i + 1),
                    Self::new(*node, i + 1).to_string(),
                    indent(i),
                ))
                .unwrap_or(" ".to_owned()),
            ),
            Node::Statement(e1, e2) => write!(
                f,
                "{};\n{}",
                Self::new(*e1, i),
                e2.map(|node| format!("{}{}", indent(i), Self::new(*node, i)))
                    .unwrap_or(String::new())
            ),
            Node::BindVal { binding, value } => write!(
                f,
                "let {} = {}",
                Self::new(*binding, i + 1),
                Self::new(*value, i + 1)
            ),
            Node::ReAssign { binding, value } => write!(
                f,
                "{} = {}",
                Self::new(*binding, i + 1),
                Self::new(*value, i + 1),
            ),
            Node::Tuple(e1, e2) => {
                write!(f, "{}, {}", Self::new(*e1, i + 1), Self::new(*e2, i + 1))
            }
            Node::Map(fields) => write!(f, "{{ {} }}", Self::new(*fields, i + 1)),
            Node::MapField { key, value } => write!(
                f,
                "{}: {}",
                Self::new(*key, i + 1),
                Self::new(*value, i + 1)
            ),
            Node::Arr(items) => write!(f, "[{}]", Self::new(*items, i + 1)),
            Node::ArrIndex { arr, index } => write!(
                f,
                "{}[{}]",
                Self::new(*arr, i + 1),
                Self::new(*index, i + 1)
            ),
            Node::FnCall { fn_bind, args } => write!(
                f,
                "{}[{}]",
                Self::new(*fn_bind, i + 1),
                args.map(|node| Self::new(*node, i + 1).to_string())
                    .unwrap_or(String::new()),
            ),
            Node::If {
                condition,
                if_expr,
                else_expr,
            } => write!(
                f,
                "if({}) {{\n{}{}\n}}{}",
                Self::new(*condition, i + 1),
                indent(i),
                Self::new(*if_expr, i + 1),
                else_expr
                    .map(|node| format!(
                        " else {{\n{}{}\n}}\n",
                        indent(i),
                        Self::new(*node, i + 1).to_string()
                    ))
                    .unwrap_or(" ".to_owned()),
            ),
            Node::Return(e) => f.write_str(
                &e.map(|node| format!("return {}", Self::new(*node, i + 1)))
                    .unwrap_or("return".to_owned()),
            ),
        }
    }
}

pub struct Statements {
    node: Option<Node>,
}

impl Iterator for Statements {
    type Item = Node;
    fn next(&mut self) -> Option<Node> {
        let next_node;
        let ret;
        match &mut self.node {
            Some(Node::Statement(e1, None)) => {
                next_node = None;
                ret = Some(mem::take(&mut **e1));
            }
            Some(Node::Statement(e1, Some(e2))) => {
                next_node = Some(mem::take(&mut **e2));
                ret = Some(mem::take(&mut **e1));
            }
            Some(_) => return None,
            None => return None,
        }
        self.node = next_node;
        ret
    }
}
