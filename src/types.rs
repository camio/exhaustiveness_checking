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
