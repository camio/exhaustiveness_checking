//! Provide an abstract syntax tree for the C++ pattern matching grammer.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Guard {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConstExpression {
    True,
    False,
    Num(i32),
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Constructor {
    ClassConstructor { num_fields: usize },
    True,
    False,
    Num(i32),
}

impl Constructor {
    pub fn is_class_constructor(self: &Constructor) -> bool {
        if let Constructor::ClassConstructor { num_fields: _ } = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    ConstExpression(ConstExpression),
    StructuredBinding(Vec<Pattern>),
}

impl Pattern {
    pub fn is_structured_binding(self: &Pattern) -> bool {
        match self {
            Pattern::StructuredBinding(_) => true,
            _ => false,
        }
    }

    /// Return 'true' if this pattern is a constant expression evaluating to 'true', and 'false'
    /// otherwise.
    pub fn is_true(self: &Pattern) -> bool {
        match self {
            Pattern::ConstExpression(ConstExpression::True) => true,
            _ => false,
        }
    }

    /// Return 'true' if this pattern is a constant expression evaluating to 'false', and 'false'
    /// otherwise.
    pub fn is_false(self: &Pattern) -> bool {
        match self {
            Pattern::ConstExpression(ConstExpression::False) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InspectArm {
    pub pattern: Pattern,
    pub guard: Option<Guard>,
}
