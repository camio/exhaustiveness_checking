//! Provide an in-memory representation for C++ types.

use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
pub enum Primitive {
    Bool,
    Int,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub derived_eq: bool,
    pub fields: Vec<Rc<Type>>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(Primitive),
    Class(Class),
}

impl Class {
    /// Return 'true' if the specified 'class_type' has a deeply derived equality
    /// operator. A class is said to have a deeply derived equality operator if it
    /// has a derived equality operator and has fields that are all either
    /// primitive types or have a deeply derived equality operator.
    pub fn deep_derived_eq(self: &Class) -> bool {
        self.derived_eq
            && self.fields.iter().all(|t| match &(**t) {
                Type::Primitive(_) => true,
                Type::Class(c) => c.deep_derived_eq(),
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deep_derived_eq() {
        assert_eq!(
            Class {
                derived_eq: true,
                fields: vec![]
            }
            .deep_derived_eq(),
            true
        );
        assert_eq!(
            Class {
                derived_eq: false,
                fields: vec![]
            }
            .deep_derived_eq(),
            false
        );
        assert_eq!(
            Class {
                derived_eq: true,
                fields: vec![Rc::new(Type::Primitive(Primitive::Bool))]
            }
            .deep_derived_eq(),
            true
        );
        assert_eq!(
            Class {
                derived_eq: true,
                fields: vec![Rc::new(Type::Class(Class {
                    derived_eq: true,
                    fields: vec![]
                }))]
            }
            .deep_derived_eq(),
            true
        );
        assert_eq!(
            Class {
                derived_eq: true,
                fields: vec![Rc::new(Type::Class(Class {
                    derived_eq: false,
                    fields: vec![]
                }))]
            }
            .deep_derived_eq(),
            false
        );
    }
}
