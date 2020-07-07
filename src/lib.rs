mod pat;
mod types;

use std::rc::Rc;

/// Return 'true' if the specified 'class_type' has a deeply derived equality
/// operator. A class is said to have a deeply derived equality operator if it
/// has a derived equality operator and has fields that are all either
/// primitive types or have a deeply derived equality operator.
pub fn deep_derived_eq(class_type: &types::Class) -> bool {
    class_type.derived_eq
        && class_type.fields.iter().all(|t| match &(**t) {
            types::Type::Primitive(_) => true,
            types::Type::Class(c) => deep_derived_eq(c),
        })
}

/// Return `true` if the specified `pattern`, assumed to have the specified
/// `type`, contributes to the computation of whether or not the enclosing
/// inspect statement is exhaustive.
pub fn pat_contributes(r#type: &types::Type, pattern: &pat::Pattern) -> bool {
    match (r#type, pattern) {
        (_, pat::Pattern::Wildcard) => true,
        (types::Type::Primitive(types::Primitive::Int), pat::Pattern::ConstExpression(_)) => false,
        (types::Type::Primitive(_), _) => true,
        (types::Type::Class(c), pat::Pattern::ConstExpression(_)) => deep_derived_eq(c),
        (
            types::Type::Class(types::Class {
                derived_eq: _,
                fields,
            }),
            pat::Pattern::StructuredBinding(pats),
        ) => fields
            .iter()
            .zip(pats.iter())
            .all(|(t, p)| pat_contributes(t, p)),
    }
}

/// Return `true` if the specified `arm`, assumed to match the specified
/// `type`, contributes to the computation of whether or not the enclosing
/// inspect statement is exhaustive.
pub fn arm_contributes(r#type: &types::Type, arm: &pat::InspectArm) -> bool {
    if arm.guard.is_some() {
        false
    } else {
        pat_contributes(&r#type, &arm.pattern)
    }
}

/// Return patterns within the specified `arms`, all assumed to match the
/// specified `type`, that contribute to the computation of whether or not the
/// enclosing inspect statement is exhaustive.
pub fn filter_noncontributors(
    r#type: types::Type,
    arms: &Vec<pat::InspectArm>,
) -> Vec<pat::Pattern> {
    arms.iter()
        .filter(|c| arm_contributes(&r#type, c))
        .map(|c| c.pattern.clone())
        .collect()
}

/// Return 'true' if the specified 'type' is inhabited by exactly one value.
pub fn is_monotype(r#type: &types::Type) -> bool {
    match r#type {
        types::Type::Class(types::Class {
            derived_eq: _,
            fields,
        }) => fields.iter().all(|t| is_monotype(&**t)),
        _ => false,
    }
}

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

pub fn constructor_from_pattern(p: &pat::Pattern) -> pat::Constructor {
    match p {
        pat::Pattern::StructuredBinding(pats) => pat::Constructor::ClassConstructor {
            num_fields: pats.len(),
        },
        pat::Pattern::ConstExpression(ce) => constructor_from_const_expression(ce),
        pat::Pattern::Wildcard => panic!("Cannot convert wildcard to constructor"),
    }
}

pub fn s(c: &pat::Constructor, p: &Vec<Vec<pat::Pattern>>) -> Vec<Vec<pat::Pattern>> {
    let mut result: Vec<Vec<pat::Pattern>> = Vec::new();

    for p_i in p {
        let p_i_1 = &p_i[0];
        match p_i_1 {
            // constexpr constructed pattern
            pat::Pattern::ConstExpression(const_expr) => {
                if constructor_from_const_expression(const_expr) == *c {
                    result.push(p_i[1..].to_vec())
                } else {
                    // If the constexpr value doesn't match the constructor 'c' then don't add a
                    // row.
                    ()
                }
            }

            // structured binding constructed pattern
            pat::Pattern::StructuredBinding(pats) => {
                debug_assert!(c.is_class_constructor());
                let mut row: Vec<pat::Pattern> = Vec::new();
                row.extend_from_slice(pats);
                row.extend_from_slice(&p_i[1..]);
                result.push(row)
            }

            pat::Pattern::Wildcard => match c {
                // Wildcard pattern with strutured binding gets _'s followed by p_i_2 to p_i_n
                pat::Constructor::ClassConstructor { num_fields } => {
                    let mut row: Vec<pat::Pattern> = Vec::new();
                    row.extend(
                        std::iter::repeat(&pat::Pattern::Wildcard)
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

pub fn useful2(
    types: &Vec<Rc<types::Type>>,
    p: &Vec<Vec<pat::Pattern>>,
    q: &Vec<pat::Pattern>,
) -> bool {
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
        pat::Pattern::Wildcard => {
            let complete_root_constructors: bool = match t1 {
                types::Type::Primitive(types::Primitive::Int) => false,
                types::Type::Primitive(types::Primitive::Bool) => {
                    // TODO: Find a way to make this cleaner
                    p.iter()
                        .find(|&row| match row[0] {
                            pat::Pattern::ConstExpression(pat::ConstExpression::True) => true,
                            _ => false,
                        })
                        .is_some()
                        && p.iter()
                            .find(|&row| match row[0] {
                                pat::Pattern::ConstExpression(pat::ConstExpression::False) => true,
                                _ => false,
                            })
                            .is_some()
                }

                types::Type::Class(_) => p
                    .iter()
                    .find(|&row| row[0].is_structured_binding())
                    .is_some(),
            };

            // TODO: Implement 'unimplemented!' sections

            if complete_root_constructors {
                match t1 {
                    types::Type::Primitive(types::Primitive::Bool) => {
                        // TODO: types_prime is computed in the same way both here and below. First
                        // need to verify that it is correct. Second I need to somehow abstract out
                        // the code.

                        // Set 'types_prime' to the type associated with the recursive call to 'useful2'.
                        let mut types_prime: Vec<Rc<types::Type>> = Vec::new();
                        match t1 {
                            types::Type::Primitive(_) => (),
                            types::Type::Class(types::Class {
                                derived_eq: _,
                                fields,
                            }) => types_prime.extend(fields.iter().cloned()),
                        }
                        types_prime.extend(types[1..].iter().cloned());

                        [pat::Constructor::True, pat::Constructor::False]
                            .iter()
                            .find(|c_k| {
                                useful2(&types_prime, &s(c_k, &p), &s(c_k, &vec![q.clone()])[0])
                            })
                            .is_some()
                    }
                    types::Type::Class(_) => unimplemented!(),
                    _ => panic!("Impossible!"),
                }
            } else {
                unimplemented!()
            }
        }

        // All other patterns are constructed patterns.
        constructed_pattern => {
            let c = constructor_from_pattern(constructed_pattern);
            // Set 'types_prime' to the type associated with the recursive call to 'useful2'.
            let mut types_prime: Vec<Rc<types::Type>> = Vec::new();
            match t1 {
                types::Type::Primitive(_) => (),
                types::Type::Class(types::Class {
                    derived_eq: _,
                    fields,
                }) => types_prime.extend(fields.iter().cloned()),
            }
            types_prime.extend(types[1..].iter().cloned());

            useful2(&types_prime, &s(&c, &p), &s(&c, &vec![q.clone()])[0])
        }
    }
}

/// Return 'true' if there exists a value of the specified `type` that the
/// specified 'pattern' matches that is not matched by the specified
/// 'pattern_matrix' and 'false' otherwise. The behavior is undefined unless
/// 'pat_contributes(pat) = true' for all patterns 'pat' within
/// 'pattern_matrix'. The behavior is also undefined unless
/// 'pat_contributes(pat) = true'.
pub fn useful(r#type: &types::Type, p: &Vec<pat::Pattern>, q: &pat::Pattern) -> bool {
    debug_assert!(p.iter().all(|p| pat_contributes(r#type, p)));
    debug_assert!(pat_contributes(r#type, q));

    if p.is_empty() {
        // Base case where pattern matrix is empty
        true
    } else if is_monotype(r#type) {
        // Base case where the pattern matrix is non-empty and the type is a
        // monotype.
        false
    } else {
        unimplemented!();
    }
}

/// Return 'true' if the specified 'patterns' form an exhaustive set for the
/// specified 'type' and 'false' otherwise.
pub fn is_exhaustive(ty: &types::Type, pattern_matrix: &Vec<pat::Pattern>) -> bool {
    !useful(ty, pattern_matrix, &pat::Pattern::Wildcard)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_filter_noncontributors() {
        // filter_noncontributors( μ⟦ bool ⟧, [] ) ⇒ []
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let arms: Vec<pat::InspectArm> = Vec::new();
        assert_eq!(filter_noncontributors(ty, &arms), Vec::new());

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ __ ⟧] ) ⇒ [μ⟦ __ ⟧]
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let arms = vec![pat::InspectArm {
            pattern: pat::Pattern::Wildcard,
            guard: None,
        }];
        assert_eq!(
            filter_noncontributors(ty, &arms),
            vec![pat::Pattern::Wildcard]
        );

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ __ if (g()) ⟧] ) ⇒ [μ⟦ __ ⟧]
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let arms = vec![pat::InspectArm {
            pattern: pat::Pattern::Wildcard,
            guard: Some(pat::Guard {}),
        }];
        assert_eq!(filter_noncontributors(ty, &arms), vec![]);
    }

    #[test]
    fn test_deep_derived_eq() {
        assert_eq!(
            deep_derived_eq(&types::Class {
                derived_eq: true,
                fields: vec![]
            }),
            true
        );
        assert_eq!(
            deep_derived_eq(&types::Class {
                derived_eq: false,
                fields: vec![]
            }),
            false
        );
        assert_eq!(
            deep_derived_eq(&types::Class {
                derived_eq: true,
                fields: vec![Rc::new(types::Type::Primitive(types::Primitive::Bool))]
            }),
            true
        );
        assert_eq!(
            deep_derived_eq(&types::Class {
                derived_eq: true,
                fields: vec![Rc::new(types::Type::Class(types::Class {
                    derived_eq: true,
                    fields: vec![]
                }))]
            }),
            true
        );
        assert_eq!(
            deep_derived_eq(&types::Class {
                derived_eq: true,
                fields: vec![Rc::new(types::Type::Class(types::Class {
                    derived_eq: false,
                    fields: vec![]
                }))]
            }),
            false
        );
    }

    #[test]
    fn test_pat_contributes() {
        assert_eq!(
            pat_contributes(
                &types::Type::Primitive(types::Primitive::Bool),
                &pat::Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &types::Type::Primitive(types::Primitive::Int),
                &pat::Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            false
        );
        assert_eq!(
            pat_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: true,
                    fields: vec![]
                }),
                &pat::Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &pat::Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            false
        );
        assert_eq!(
            pat_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: true,
                    fields: vec![]
                }),
                &pat::Pattern::Wildcard,
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &pat::Pattern::Wildcard,
            ),
            true
        );

        // Verify that matching a constant expression for a class without deep derived equality
        // (but with derived equality) doesn't contribute to exhaustiveness. Note that class 'd'
        // below has derived equality, but not deep derived equality.
        //
        // ```c++
        // class c { bool operator==(const c&) { return true; } };
        // class d { C c; bool operator==(const d&) = default; };
        // ```
        // pat_contributes( μ⟦d⟧, μ⟦d()⟧ ) ⇒ false

        let c = Rc::new(types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![],
        }));
        let d = Rc::new(types::Type::Class(types::Class {
            derived_eq: true,
            fields: vec![c.clone()],
        }));
        assert_eq!(
            pat_contributes(
                &d,
                &pat::Pattern::ConstExpression(pat::ConstExpression::Other)
            ),
            false
        );

        // ```c++
        // class c { bool i; bool j; };
        // constexpr c c_val{true,false};
        // class d { C c; bool b; };
        // ```

        let c = Rc::new(types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![
                Rc::new(types::Type::Primitive(types::Primitive::Bool)),
                Rc::new(types::Type::Primitive(types::Primitive::Bool)),
            ],
        }));

        let c_val = pat::ConstExpression::Other;

        let d = Rc::new(types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![
                c.clone(),
                Rc::new(types::Type::Primitive(types::Primitive::Bool)),
            ],
        }));

        // let pat1 = μ⟦[__, __]⟧
        // pat_contributes( μ⟦C⟧, pat1) ⇒  true

        let pat1 =
            pat::Pattern::StructuredBinding(vec![pat::Pattern::Wildcard, pat::Pattern::Wildcard]);
        assert_eq!(pat_contributes(&c, &pat1), true);

        // let pat2 = μ⟦[case c_val, true]⟧
        // pat_contributes( μ⟦d⟧, pat2 ) ⇒  false

        let pat2 = pat::Pattern::StructuredBinding(vec![
            pat::Pattern::ConstExpression(c_val.clone()),
            pat::Pattern::ConstExpression(pat::ConstExpression::True),
        ]);
        assert_eq!(pat_contributes(&d, &pat2), false);

        // let pat3 = μ⟦[[true,false], true]⟧
        // pat_contributes( μ⟦d⟧, pat3 ) ⇒  true
        let pat3 = pat::Pattern::StructuredBinding(vec![
            pat::Pattern::StructuredBinding(vec![
                pat::Pattern::ConstExpression(pat::ConstExpression::True),
                pat::Pattern::ConstExpression(pat::ConstExpression::False),
            ]),
            pat::Pattern::ConstExpression(pat::ConstExpression::True),
        ]);
        assert_eq!(pat_contributes(&d, &pat3), true);
    }
    #[test]
    fn test_arm_contributes() {
        // Check guard case
        assert_eq!(
            arm_contributes(
                &types::Type::Primitive(types::Primitive::Bool),
                &pat::InspectArm {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: Some(pat::Guard {})
                }
            ),
            false
        );
        // Check no-guard case 1
        assert_eq!(
            arm_contributes(
                &types::Type::Primitive(types::Primitive::Bool),
                &pat::InspectArm {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            true
        );
        // Check no-guard case 2
        assert_eq!(
            arm_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &pat::InspectArm {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            false
        );
    }
    #[test]
    fn test_is_monotype() {
        assert_eq!(
            is_monotype(&types::Type::Primitive(types::Primitive::Bool)),
            false
        );
        //```c++
        //class c {};
        //```
        let c = types::Type::Class(types::Class {
            derived_eq: false,
            fields: Vec::new(),
        });
        assert_eq!(is_monotype(&c), true);

        //```c++
        //class d { c c1; };
        //```
        let d = types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![Rc::new(c.clone())],
        });

        assert_eq!(is_monotype(&d), true);

        //```c++
        //class e { c c1; int i; };
        //```
        let e = types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![
                Rc::new(c.clone()),
                Rc::new(types::Type::Primitive(types::Primitive::Int)),
            ],
        });
        assert_eq!(is_monotype(&e), false);

        //```c++
        //class f { c c1; d d1; };
        //```
        let f = types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![Rc::new(c.clone()), Rc::new(d.clone())],
        });
        assert_eq!(is_monotype(&f), true);
    }
    #[test]
    fn test_useful_empty_matrix_base_case() {
        // let int_type = μ⟦ int ⟧
        let int_type = Rc::new(types::Type::Primitive(types::Primitive::Int));

        // let mat = μ⟦ ⟧
        let empty_matrix: Vec<pat::Pattern> = Vec::new();

        // let ty = μ⟦ class C { int a; int b; }
        let ty = types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![int_type.clone(), int_type.clone()],
        });

        // let pat = μ⟦ __ ⟧
        let pat = pat::Pattern::Wildcard;

        assert_eq!(useful(&ty, &empty_matrix, &pat), true);
    }

    #[test]
    fn test_useful_monotype_base_case() {
        // let ty = μ⟦ class c { bool operator==(const c&) = default; } ⟧
        let ty = types::Type::Class(types::Class {
            derived_eq: true,
            fields: Vec::new(),
        });
        // let mat = μ⟦ c() ⟧
        let mat = vec![pat::Pattern::ConstExpression(pat::ConstExpression::Other)];
        // let pat = μ⟦ __ ⟧
        let pat = pat::Pattern::Wildcard;
        assert_eq!(useful(&ty, &mat, &pat), false);
    }
}

fn main() {}
