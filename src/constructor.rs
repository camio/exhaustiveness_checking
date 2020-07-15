//! Provide a representation for type constructor selections.

use crate::types::Class;
use crate::types::Primitive;
use crate::types::Type;
use crate::pat;
use crate::pat::Pattern;

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

pub fn constructor_from_const_expression(ce: &pat::ConstExpression) -> Constructor {
    match ce {
        pat::ConstExpression::True => Constructor::True,
        pat::ConstExpression::False => Constructor::False,
        pat::ConstExpression::Num(n) => Constructor::Num(*n),
        pat::ConstExpression::Other => {
            panic!("Cannot convert \"other\" expression to pat::Constructor")
        }
    }
}

pub fn constructor_from_pattern(p: &Pattern) -> Constructor {
    match p {
        Pattern::StructuredBinding(pats) => Constructor::ClassConstructor {
            num_fields: pats.len(),
        },
        Pattern::ConstExpression(ce) => constructor_from_const_expression(ce),
        Pattern::Wildcard => panic!("Cannot convert wildcard to constructor"),
    }
}

impl Type {
    /// Return a list of all this type's constructors. The behavior is undefined unless this type
    /// is not an 'int'. Note that 'int' types have too many constructors to enumerate.
    pub fn constructors(self: &Type) -> Vec<Constructor> {
        // TODO: needs test
        match self {
            Type::Primitive(Primitive::Bool) => {
                vec![Constructor::True, Constructor::False]
            }
            Type::Primitive(Primitive::Int) => panic!("Cannot enumerate 'int' constructors."),
            Type::Class(Class {
                derived_eq: _,
                fields,
            }) => vec![Constructor::ClassConstructor {
                num_fields: fields.len(),
            }],
        }
    }
}
