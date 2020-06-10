//! Provide an abstract syntax tree for the C++ pattern matching grammer.

#[derive(Clone, Copy, Debug)]
pub struct Guard {}

#[derive(Clone, Copy, Debug)]
pub enum ConstExpression {
    True,
    False,
    Num(i32),
    Other,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard,
    ConstExpression(ConstExpression),
    StructuredBinding(Vec<Pattern>),
}

#[derive(Clone, Debug)]
pub struct InspectExpressionCase {
    pub pattern: Pattern,
    pub guard: Option<Guard>,
}