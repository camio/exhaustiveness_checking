//! Provide exhaustiveness checking algorithms for C++ Pattern Matching.
//!
//! `exhaustiveness_checking` is a crate that provides data structures and algorithms that
//! implement algorithms for exhaustiveness checking for [Pattern
//! Matching](http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/p1371r2.pdf) in C++.

pub mod constructor;
pub mod contrib;
pub mod pat;
pub mod types;

use pat::Pattern;
use types::Primitive;
use types::Type;
use constructor::Constructor;

use std::rc::Rc;

// TODO:
// - 'ConstExpression' and 'StructuredBinding' are both "constructed patterns". The underlying type
//   could reflect the differentiation between these and wildcard and this code will get simpler.
// - 'ConstExpression' needs to be revisited, especially as it relates to 'Other'. These are really
//   primitive values and we should use StructuredBinding to represent class values that can be
//   destructured. This could be described as an earlier pass of the pattern. I suppose we could
//   leave Other after that pass.


/// Return the types corresponding to the result of an `s` invocation assuming `s` incoming `p`
/// argument has the specified `types`.
fn s_types(types: &Vec<Rc<Type>>) -> Vec<Rc<Type>> {
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

fn s(c: &Constructor, p: &Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>> {
    let mut result: Vec<Vec<Pattern>> = Vec::new();

    for p_i in p {
        let p_i_1 = &p_i[0];
        match p_i_1 {
            // constexpr constructed pattern
            Pattern::ConstExpression(const_expr) => {
                if constructor::constructor_from_const_expression(const_expr) == *c {
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
                Constructor::ClassConstructor { num_fields } => {
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

fn d_types(types: &Vec<Rc<Type>>) -> Vec<Rc<Type>> {
    types[1..].to_vec()
}

fn d(p: &Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>> {
    let mut result = Vec::new();
    for p_i in p {
        let p_i_1 = &p_i[0];
        match p_i_1 {
            // constexpr constructed pattern
            Pattern::ConstExpression(_) => (),

            // structured binding constructed pattern
            Pattern::StructuredBinding(_) => (),

            Pattern::Wildcard => result.push(p_i[1..].to_vec()),
        }
    }
    result
}

/// Determine whether or not a single pattern matches values that a collection of patterns does
/// not.
///
/// Return `true` if there exists a sequence of values with the specified `types` that the
/// specified `q` pattern sequence matches that is not matched by one of he pattern sequences in the
/// specified `p` and `false` otherwise. The behavior is undefined unless `pat_contributes(pat) =
/// true` for all patterns `pat` within `p`. The behavior is also undefined unless
/// `pat_contributes(q) = true`.
pub fn useful(types: &Vec<Rc<Type>>, p: &Vec<Vec<Pattern>>, q: &Vec<Pattern>) -> bool {
    // TODO: Determine if this can be elegantly extended to handle non-contributing patterns.
    // TODO: Investigate if the arguments to this function would more appropriately be slices.

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
                Type::Primitive(Primitive::Int) => false,
                Type::Primitive(Primitive::Bool) => {
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
                    .find(|c_k| useful(&s_types(types), &s(c_k, &p), &s(c_k, &vec![q.clone()])[0]))
                    .is_some()
            } else {
                useful(
                    &d_types(types),
                    &d(&p),
                    &q.iter().skip(1).cloned().collect(),
                )
            }
        }

        // All other patterns are constructed patterns.
        constructed_pattern => {
            let c = constructor::constructor_from_pattern(constructed_pattern);
            useful(&s_types(types), &s(&c, &p), &s(&c, &vec![q.clone()])[0])
        }
    }
}

/// Determine when a collection of patterns is exhaustive (i.e. matches all values for a particular
/// type).
///
/// Return `true` if the specified `patterns` form an exhaustive set for the specified
/// `pattern_type` and `false` otherwise.
pub fn is_exhaustive(pattern_type: &Type, patterns: &Vec<pat::InspectArm>) -> bool {
    // TODO: write some tests for this
    !useful(
        &vec![Rc::new(pattern_type.clone())],
        &contrib::filter_noncontributors(pattern_type, patterns)
            .iter()
            .map(|p| vec![p.clone()])
            .collect(),
        &vec![Pattern::Wildcard],
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    use types::Class;

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
            s(&Constructor::True, &Vec::new()),
            Vec::new() as Vec<Vec<Pattern>>
        );

        let pat_true = pat::Pattern::ConstExpression(pat::ConstExpression::True);
        let pat1 = pat::Pattern::ConstExpression(pat::ConstExpression::Num(1));
        let pat2 = pat::Pattern::ConstExpression(pat::ConstExpression::Num(2));

        // s( μ⟦ true ⟧, μ⟦ true 1 2 ⟧ ) ⇒ μ⟦ 1 2 ⟧
        assert_eq!(
            s(
                &Constructor::True,
                &vec![vec![pat_true.clone(), pat1.clone(), pat2.clone()]]
            ),
            vec![vec![pat1.clone(), pat2.clone()]]
        );

        // s( μ⟦ true ⟧, μ⟦ false 1 2 ⟧ ) ⇒ μ⟦ ⟧
        assert_eq!(
            s(
                &Constructor::False,
                &vec![vec![pat_true.clone(), pat1.clone(), pat2.clone()]]
            ),
            Vec::new() as Vec<Vec<Pattern>>
        );

        // s( μ⟦ true ⟧, μ⟦ _ 1 2 ⟧ ) ⇒ μ⟦ 1 2 ⟧
        assert_eq!(
            s(
                &Constructor::True,
                &vec![vec![Pattern::Wildcard, pat1.clone(), pat2.clone()]]
            ),
            vec![vec![pat1.clone(), pat2.clone()]]
        );

        // s( μ⟦ {}³ ⟧, μ⟦ [1 2 _] 1 2 ⟧ ) ⇒ μ⟦ 1 2 _ 1 2 ⟧
        assert_eq!(
            s(
                &Constructor::ClassConstructor { num_fields: 3 },
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
                &Constructor::ClassConstructor { num_fields: 3 },
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
    fn test_useful_bool() {
        // useful( μ⟦ [bool] ⟧, μ⟦ [ true ] ⟧, μ⟦ true ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(Type::Primitive(Primitive::Bool))],
                &vec![vec![Pattern::ConstExpression(pat::ConstExpression::True)]],
                &vec![Pattern::ConstExpression(pat::ConstExpression::True)]
            ),
            false
        );

        // useful( μ⟦ [bool] ⟧, μ⟦ [ true ] ⟧, μ⟦ false ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![Rc::new(Type::Primitive(Primitive::Bool))],
                &vec![vec![Pattern::ConstExpression(pat::ConstExpression::True)]],
                &vec![Pattern::ConstExpression(pat::ConstExpression::False)]
            ),
            true
        );

        // useful( μ⟦ [bool] ⟧, μ⟦ [ true ] ⟧, μ⟦ _ ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![Rc::new(Type::Primitive(Primitive::Bool))],
                &vec![vec![Pattern::ConstExpression(pat::ConstExpression::True)]],
                &vec![Pattern::Wildcard]
            ),
            true
        );

        // useful( μ⟦ [bool] ⟧, μ⟦ [ true, false ] ⟧, μ⟦ _ ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(Type::Primitive(Primitive::Bool))],
                &vec![
                    vec![Pattern::ConstExpression(pat::ConstExpression::True)],
                    vec![Pattern::ConstExpression(pat::ConstExpression::False)],
                ],
                &vec![Pattern::Wildcard]
            ),
            false
        );

        // useful( μ⟦ [bool] ⟧, μ⟦ [ true, false ] ⟧, μ⟦ true ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(Type::Primitive(Primitive::Bool))],
                &vec![
                    vec![Pattern::ConstExpression(pat::ConstExpression::True)],
                    vec![Pattern::ConstExpression(pat::ConstExpression::False)],
                ],
                &vec![Pattern::ConstExpression(pat::ConstExpression::True)]
            ),
            false
        );
    }

    #[test]
    fn test_useful_class() {
        // let ty = μ⟦ class c { bool operator==(const c&) = default; bool a; bool b;} ⟧
        let ty = Type::Class(Class {
            derived_eq: true,
            fields: vec![
                Rc::new(Type::Primitive(Primitive::Bool)),
                Rc::new(Type::Primitive(Primitive::Bool)),
            ],
        });

        // useful( μ⟦ [c] ⟧, μ⟦ [ {true, _} ] ⟧, μ⟦ {false, _} ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![Rc::new(ty.clone())],
                &vec![vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ])]],
                &vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::False),
                    Pattern::Wildcard
                ])]
            ),
            true
        );

        // useful( μ⟦ [c] ⟧, μ⟦ [ {true, _} ] ⟧, μ⟦ {true, _} ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(ty.clone())],
                &vec![vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ])]],
                &vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ])]
            ),
            false
        );

        // useful( μ⟦ [c] ⟧, μ⟦ [ {true, _} ] ⟧, μ⟦ {false, true} ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![Rc::new(ty.clone())],
                &vec![vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ])]],
                &vec![Pattern::StructuredBinding(vec![
                    Pattern::ConstExpression(pat::ConstExpression::False),
                    Pattern::ConstExpression(pat::ConstExpression::True),
                ])]
            ),
            true
        );

        // useful(
        //   μ⟦ [c] ⟧,
        //   μ⟦ [ {true, _},
        //        {false, true},
        //        {false, false} ] ⟧,
        //   μ⟦ {_, _} ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(ty.clone())],
                &vec![
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::True),
                        Pattern::Wildcard,
                    ])],
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::True),
                    ])],
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::False),
                    ])],
                ],
                &vec![Pattern::StructuredBinding(vec![
                    Pattern::Wildcard,
                    Pattern::Wildcard,
                ])]
            ),
            false
        );

        // useful(
        //   μ⟦ [c] ⟧,
        //   μ⟦ [ {true, _},
        //        {false, true},
        //        {false, false} ] ⟧,
        //   μ⟦ _ ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![Rc::new(ty.clone())],
                &vec![
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::True),
                        Pattern::Wildcard,
                    ])],
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::True),
                    ])],
                    vec![Pattern::StructuredBinding(vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::False),
                    ])],
                ],
                &vec![Pattern::Wildcard]
            ),
            false
        );
    }

    #[test]
    fn test_useful_bools() {
        // useful( μ⟦ [bool, bool] ⟧, μ⟦ [ true _ ] ⟧, μ⟦ false _ ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![
                    Rc::new(Type::Primitive(Primitive::Bool)),
                    Rc::new(Type::Primitive(Primitive::Bool)),
                ],
                &vec![vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ]],
                &vec![
                    Pattern::ConstExpression(pat::ConstExpression::False),
                    Pattern::Wildcard
                ]
            ),
            true
        );

        // useful( μ⟦ [bool, bool] ⟧, μ⟦ [ true _ ] ⟧, μ⟦ true _ ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![
                    Rc::new(Type::Primitive(Primitive::Bool)),
                    Rc::new(Type::Primitive(Primitive::Bool)),
                ],
                &vec![vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ]],
                &vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ]
            ),
            false
        );

        // useful( μ⟦ [bool, bool] ⟧, μ⟦ [ true _ ] ⟧, μ⟦ false true ⟧ ) ⇒ true
        assert_eq!(
            useful(
                &vec![
                    Rc::new(Type::Primitive(Primitive::Bool)),
                    Rc::new(Type::Primitive(Primitive::Bool)),
                ],
                &vec![vec![
                    Pattern::ConstExpression(pat::ConstExpression::True),
                    Pattern::Wildcard
                ]],
                &vec![
                    Pattern::ConstExpression(pat::ConstExpression::False),
                    Pattern::ConstExpression(pat::ConstExpression::True),
                ]
            ),
            true
        );

        // useful(
        //   μ⟦ [bool, bool] ⟧,
        //   μ⟦ [ true _,
        //        false true,
        //        false false ] ⟧,
        //   μ⟦ _ _ ⟧ ) ⇒ false
        assert_eq!(
            useful(
                &vec![
                    Rc::new(Type::Primitive(Primitive::Bool)),
                    Rc::new(Type::Primitive(Primitive::Bool)),
                ],
                &vec![
                    vec![
                        Pattern::ConstExpression(pat::ConstExpression::True),
                        Pattern::Wildcard,
                    ],
                    vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::True),
                    ],
                    vec![
                        Pattern::ConstExpression(pat::ConstExpression::False),
                        Pattern::ConstExpression(pat::ConstExpression::False),
                    ],
                ],
                &vec![Pattern::Wildcard, Pattern::Wildcard]
            ),
            false
        );
    }
}
