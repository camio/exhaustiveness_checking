mod pat;
mod types;

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
                fields: types,
            }),
            pat::Pattern::StructuredBinding(pats),
        ) => types
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
            fields: flds,
        }) => flds.iter().all(|t| is_monotype(&**t)),
        _ => false,
    }
}

/// Return 'true' if there exists a value of the specified `type` that the
/// specified 'pattern' matches that is not matched by the specified
/// 'pattern_matrix' and 'false' otherwise.
pub fn useful(
    r#type: &types::Type,
    pattern_matrix: &Vec<pat::Pattern>,
    _pattern: &pat::Pattern,
) -> bool {
    if pattern_matrix.is_empty() {
        // Base case where pattern matrix is empty
        return true;
    }

    // Base case where the pattern matrix is non-empty and the type is a
    // monotype.
    if is_monotype(r#type) {
        return false;
    }

    unimplemented!();
}

/// Return 'true' if the specified 'patterns' form an exhaustive set for the
/// specified 'type' and 'false' otherwise.
pub fn is_exhaustive(_type: &types::Type, _patterns: &Vec<pat::Pattern>) -> bool {
    unimplemented!();
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

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ _ ⟧] ) ⇒ [μ⟦ _ ⟧]
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let arms = vec![pat::InspectArm {
            pattern: pat::Pattern::Wildcard,
            guard: None,
        }];
        assert_eq!(
            filter_noncontributors(ty, &arms),
            vec![pat::Pattern::Wildcard]
        );

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ _ if (g()) ⟧] ) ⇒ [μ⟦ _ ⟧]
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

        // let pat1 = μ⟦[_, _]⟧
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
}

fn main() {}
