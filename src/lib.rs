pub mod contrib;
pub mod pat;
pub mod types;

use pat::Pattern;
use types::Class;
use types::Primitive;
use types::Type;

use std::rc::Rc;

// TODO:
// - 'ConstExpression' and 'StructuredBinding' are both "constructed patterns". The underlying type
//   could reflect the differentiation between these and wildcard and this code will get simpler.
// - 'ConstExpression' needs to be revisited, especially as it relates to 'Other'. These are really
//   primitive values and we should use StructuredBinding to represent class values that can be
//   destructured. This could be described as an earlier pass of the pattern. I suppose we could
//   leave Other after that pass.

pub fn constructor_from_const_expression(ce: &pat::ConstExpression) -> pat::Constructor {
    match ce {
        pat::ConstExpression::True => pat::Constructor::True,
        pat::ConstExpression::False => pat::Constructor::False,
        pat::ConstExpression::Num(n) => pat::Constructor::Num(*n),
        pat::ConstExpression::Other => {
            panic!("Cannot convert \"other\" expression to pat::Constructor")
        }
    }
}

pub fn constructor_from_pattern(p: &Pattern) -> pat::Constructor {
    match p {
        Pattern::StructuredBinding(pats) => pat::Constructor::ClassConstructor {
            num_fields: pats.len(),
        },
        Pattern::ConstExpression(ce) => constructor_from_const_expression(ce),
        Pattern::Wildcard => panic!("Cannot convert wildcard to constructor"),
    }
}

impl Type {
    /// Return a list of all this type's constructors. The behavior is undefined unless this type
    /// is not an 'int'. Note that 'int' types have too many constructors to enumerate.
    fn constructors(self: &Type) -> Vec<pat::Constructor> {
        // TODO: needs test
        match self {
            Type::Primitive(types::Primitive::Bool) => {
                vec![pat::Constructor::True, pat::Constructor::False]
            }
            Type::Primitive(types::Primitive::Int) => {
                panic!("Cannot enumerate 'int' constructors.")
            }
            Type::Class(Class {
                derived_eq: _,
                fields,
            }) => vec![pat::Constructor::ClassConstructor {
                num_fields: fields.len(),
            }],
        }
    }
}

/// Return the types corresponding to the result of an 's' invocation assuming 's' incoming 'p'
/// argument has the specified 'types'.
pub fn s_types(types: &Vec<Rc<Type>>) -> Vec<Rc<Type>> {
    // TODO: At some point decide if '!types.is_empty()' should be a precondition. If so, it would
    // also be a precondition for 's'.
    if types.is_empty() {
        Vec::new()
    } else {
        let mut result: Vec<Rc<Type>> = Vec::new();
        let t1 = &*types[0];
        match t1 {
            Type::Primitive(_) => (),
            Type::Class(types::Class {
                derived_eq: _,
                fields,
            }) => result.extend(fields.iter().cloned()),
        }
        result.extend(types[1..].iter().cloned());
        result
    }
}

pub fn s(c: &pat::Constructor, p: &Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>> {
    let mut result: Vec<Vec<Pattern>> = Vec::new();

    for p_i in p {
        let p_i_1 = &p_i[0];
        match p_i_1 {
            // constexpr constructed pattern
            Pattern::ConstExpression(const_expr) => {
                if constructor_from_const_expression(const_expr) == *c {
                    result.push(p_i[1..].to_vec())
                } else {
                    // If the constexpr value doesn't match the constructor 'c' then don't add a
                    // row.
                    ()
                }
            }

            // structured binding constructed pattern
            Pattern::StructuredBinding(pats) => {
                debug_assert!(c.is_class_constructor());
                let mut row: Vec<Pattern> = Vec::new();
                row.extend_from_slice(pats);
                row.extend_from_slice(&p_i[1..]);
                result.push(row)
            }

            Pattern::Wildcard => match c {
                // Wildcard pattern with strutured binding gets _'s followed by p_i_2 to p_i_n
                pat::Constructor::ClassConstructor { num_fields } => {
                    let mut row: Vec<Pattern> = Vec::new();
                    row.extend(
                        std::iter::repeat(&Pattern::Wildcard)
                            .cloned()
                            .take(*num_fields as usize),
                    );
                    row.extend_from_slice(&p_i[1..]);
                    result.push(row)
                }
                _ => result.push(p_i[1..].to_vec()),
            },
        }
    }
    return result;
}

pub fn d(_c: &pat::Constructor, _p: &Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>> {
    // TODO
    unimplemented!();
}

pub fn useful2(types: &Vec<Rc<Type>>, p: &Vec<Vec<Pattern>>, q: &Vec<Pattern>) -> bool {
    // This is a version of 'useful' that uses pattern matrix style patterns. The original pattern
    // must be preprocessed before this function can be used.

    let n = q.len();
    let m = p.len();

    if n == 0 {
        if m > 0 {
            return false;
        } else {
            return true;
        }
    }

    let q1 = &q[0];
    let t1 = &*types[0];

    match q1 {
        Pattern::Wildcard => {
            let complete_root_constructors: bool = match t1 {
                Type::Primitive(types::Primitive::Int) => false,
                Type::Primitive(types::Primitive::Bool) => {
                    p.iter().find(|&row| row[0].is_true()).is_some()
                        && p.iter().find(|&row| row[0].is_false()).is_some()
                }
                Type::Class(_) => p
                    .iter()
                    .find(|&row| row[0].is_structured_binding())
                    .is_some(),
            };

            if complete_root_constructors {
                t1.constructors()
                    .iter()
                    .find(|c_k| useful2(&s_types(types), &s(c_k, &p), &s(c_k, &vec![q.clone()])[0]))
                    .is_some()
            } else {
                // TODO: Implement
                unimplemented!()
            }
        }

        // All other patterns are constructed patterns.
        constructed_pattern => {
            let c = constructor_from_pattern(constructed_pattern);
            useful2(&s_types(types), &s(&c, &p), &s(&c, &vec![q.clone()])[0])
        }
    }
}

/// Return 'true' if there exists a value of the specified `pattern_type` that the specified
/// 'pattern' matches that is not matched by the specified 'pattern_matrix' and 'false' otherwise.
/// The behavior is undefined unless 'pat_contributes(pat) = true' for all patterns 'pat' within
/// 'pattern_matrix'. The behavior is also undefined unless 'pat_contributes(pat) = true'.
pub fn useful(pattern_type: &Type, p: &Vec<Pattern>, q: &Pattern) -> bool {
    debug_assert!(p.iter().all(|p| contrib::pat_contributes(pattern_type, p)));
    debug_assert!(contrib::pat_contributes(pattern_type, q));

    if p.is_empty() {
        // Base case where pattern matrix is empty
        true
    } else if pattern_type.is_monotype() {
        // Base case where the pattern matrix is non-empty and the type is a
        // monotype.
        false
    } else {
        unimplemented!();
    }
}

/// Return 'true' if the specified 'patterns' form an exhaustive set for the specified
/// 'pattern_type' and 'false' otherwise.
pub fn is_exhaustive(pattern_type: &Type, pattern_matrix: &Vec<Pattern>) -> bool {
    !useful(pattern_type, pattern_matrix, &Pattern::Wildcard)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_s_types() {
        // s_types( μ⟦ [] ⟧ ) → μ⟦ [] ⟧
        assert_eq!(s_types(&Vec::new()), Vec::new() as Vec<Rc<Type>>);

        // s_types( μ⟦ [bool, int] ⟧ ) → μ⟦ [int] ⟧
        assert_eq!(
            s_types(&vec![
                Rc::new(Type::Primitive(Primitive::Bool)),
                Rc::new(Type::Primitive(Primitive::Int))
            ]),
            vec![Rc::new(Type::Primitive(Primitive::Int))]
        );

        // s_types( μ⟦ [class { bool, int }, int] ⟧ ) → μ⟦ [bool, int, int] ⟧
        assert_eq!(
            s_types(&vec![
                Rc::new(Type::Class(Class {
                    derived_eq: true,
                    fields: vec![
                        Rc::new(Type::Primitive(Primitive::Bool)),
                        Rc::new(Type::Primitive(Primitive::Int)),
                    ]
                })),
                Rc::new(Type::Primitive(Primitive::Int))
            ]),
            vec![
                Rc::new(Type::Primitive(Primitive::Bool)),
                Rc::new(Type::Primitive(Primitive::Int)),
                Rc::new(Type::Primitive(Primitive::Int)),
            ]
        );
    }

    #[test]
    fn test_s() {
        assert_eq!(
            s(&pat::Constructor::True, &Vec::new()),
            Vec::new() as Vec<Vec<Pattern>>
        );

        let pat_true = pat::Pattern::ConstExpression(pat::ConstExpression::True);
        let pat1 = pat::Pattern::ConstExpression(pat::ConstExpression::Num(1));
        let pat2 = pat::Pattern::ConstExpression(pat::ConstExpression::Num(2));

        // s( μ⟦ true ⟧, μ⟦ true 1 2 ⟧ ) ⇒ μ⟦ 1 2 ⟧
        assert_eq!(
            s(
                &pat::Constructor::True,
                &vec![vec![pat_true.clone(), pat1.clone(), pat2.clone()]]
            ),
            vec![vec![pat1.clone(), pat2.clone()]]
        );

        // s( μ⟦ true ⟧, μ⟦ false 1 2 ⟧ ) ⇒ μ⟦ ⟧
        assert_eq!(
            s(
                &pat::Constructor::False,
                &vec![vec![pat_true.clone(), pat1.clone(), pat2.clone()]]
            ),
            Vec::new() as Vec<Vec<Pattern>>
        );

        // s( μ⟦ true ⟧, μ⟦ _ 1 2 ⟧ ) ⇒ μ⟦ 1 2 ⟧
        assert_eq!(
            s(
                &pat::Constructor::True,
                &vec![vec![Pattern::Wildcard, pat1.clone(), pat2.clone()]]
            ),
            vec![vec![pat1.clone(), pat2.clone()]]
        );

        // s( μ⟦ {}³ ⟧, μ⟦ [1 2 _] 1 2 ⟧ ) ⇒ μ⟦ 1 2 _ 1 2 ⟧
        assert_eq!(
            s(
                &pat::Constructor::ClassConstructor { num_fields: 3 },
                &vec![vec![
                    Pattern::StructuredBinding(vec![pat1.clone(), pat2.clone(), Pattern::Wildcard]),
                    pat1.clone(),
                    pat2.clone()
                ]]
            ),
            vec![vec![
                pat1.clone(),
                pat2.clone(),
                Pattern::Wildcard,
                pat1.clone(),
                pat2.clone()
            ]]
        );

        // s( μ⟦ {}³ ⟧, μ⟦ _ 1 2 ⟧ ) ⇒ μ⟦ _ _ _ 1 2 ⟧
        assert_eq!(
            s(
                &pat::Constructor::ClassConstructor { num_fields: 3 },
                &vec![vec![Pattern::Wildcard, pat1.clone(), pat2.clone()]]
            ),
            vec![vec![
                Pattern::Wildcard,
                Pattern::Wildcard,
                Pattern::Wildcard,
                pat1.clone(),
                pat2.clone()
            ]]
        );
    }

    #[test]
    fn test_useful_empty_matrix_base_case() {
        // let int_type = μ⟦ int ⟧
        let int_type = Rc::new(Type::Primitive(types::Primitive::Int));

        // let mat = μ⟦ ⟧
        let empty_matrix: Vec<Pattern> = Vec::new();

        // let ty = μ⟦ class C { int a; int b; }
        let ty = Type::Class(types::Class {
            derived_eq: false,
            fields: vec![int_type.clone(), int_type.clone()],
        });

        // let pat = μ⟦ __ ⟧
        let pat = Pattern::Wildcard;

        assert_eq!(useful(&ty, &empty_matrix, &pat), true);
    }

    #[test]
    fn test_useful_monotype_base_case() {
        // let ty = μ⟦ class c { bool operator==(const c&) = default; } ⟧
        let ty = Type::Class(types::Class {
            derived_eq: true,
            fields: Vec::new(),
        });
        // let mat = μ⟦ c() ⟧
        let mat = vec![Pattern::ConstExpression(pat::ConstExpression::Other)];
        // let pat = μ⟦ __ ⟧
        let pat = Pattern::Wildcard;
        assert_eq!(useful(&ty, &mat, &pat), false);
    }
}
