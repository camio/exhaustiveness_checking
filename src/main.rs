mod pat;
mod types;

/// Return 'true' if the specified 'class_type' has a deeply derived equality
/// operator. A class is said to have a deeply derived equality operator if it
/// has a derived equality operator and has fields that are all either
/// primitive types or have a deeply derived equality operator.
pub fn deep_derived_eq(class_type: &types::Class) -> bool {
    return class_type.derived_eq
        && class_type.fields.iter().all(|t| match &(**t) {
            types::Type::Primitive(_) => true,
            types::Type::Class(c) => deep_derived_eq(c),
        });
}

/// Return `true` if the specified `pattern`, assumed to have the specified
/// `type`, contributes to the computation of whether or not the enclosing
/// inspect statement is exhaustive.
pub fn pat_contributes(r#type: &types::Type, pattern: &pat::Pattern) -> bool {
    return match (r#type, pattern) {
        (_, pat::Pattern::Wildcard) => true,
        (types::Type::Primitive(_), _) => true,
        (
            types::Type::Class(types::Class {
                derived_eq: true,
                fields: _,
            }),
            pat::Pattern::ConstExpression(_),
        ) => true,
        (
            types::Type::Class(types::Class {
                derived_eq: false,
                fields: _,
            }),
            pat::Pattern::ConstExpression(_),
        ) => false,
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
    };
}

/// Return `true` if the specified `inspect_expression_case`, assumed to have
/// the specified `type`, contributes to the computation of whether or not the
/// enclosing inspect statement is exhaustive.
pub fn case_contributes(
    r#type: &types::Type,
    inspect_expression_case: &pat::InspectExpressionCase,
) -> bool {
    if inspect_expression_case.guard.is_some() {
        return false;
    }
    return pat_contributes(&r#type, &inspect_expression_case.pattern);
}

/// Return patterns within the specified `cases`, all assumed to have the
/// specified `type`, that contribute to the computation of whether or not the
/// enclosing inspect statement is exhaustive.
pub fn filter_noncontributors(
    r#type: types::Type,
    cases: &Vec<pat::InspectExpressionCase>,
) -> Vec<pat::Pattern> {
    return cases
        .iter()
        .filter(|c| case_contributes(&r#type, c))
        .map(|c| c.pattern.clone())
        .collect();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_filter_noncontributors() {
        // filter_noncontributors( μ⟦ bool ⟧, [] ) ⇒ []
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let cases: Vec<pat::InspectExpressionCase> = Vec::new();
        assert_eq!(filter_noncontributors(ty, &cases), Vec::new());

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ _ ⟧] ) ⇒ [μ⟦ _ ⟧]
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let cases = vec![pat::InspectExpressionCase {
            pattern: pat::Pattern::Wildcard,
            guard: None,
        }];
        assert_eq!(
            filter_noncontributors(ty, &cases),
            vec![pat::Pattern::Wildcard]
        );

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ _ if (g()) ⟧] ) ⇒ [μ⟦ _ ⟧]
        let ty = types::Type::Primitive(types::Primitive::Bool);
        let cases = vec![pat::InspectExpressionCase {
            pattern: pat::Pattern::Wildcard,
            guard: Some(pat::Guard{}),
        }];
        assert_eq!(
            filter_noncontributors(ty, &cases),
            vec![]
        );

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

        // ```c++
        // class c { int i; int j; };
        // constexpr c c_val{1,2};
        // class d { C c; bool b; };
        // ```

        let c = Rc::new(types::Type::Class(types::Class {
            derived_eq: false,
            fields: vec![
                Rc::new(types::Type::Primitive(types::Primitive::Int)),
                Rc::new(types::Type::Primitive(types::Primitive::Int)),
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

        // let pat3 = μ⟦[[2,3], true]⟧
        // pat_contributes( μ⟦d⟧, pat3 ) ⇒  true
        let pat3 = pat::Pattern::StructuredBinding(vec![
            pat::Pattern::StructuredBinding(vec![
                pat::Pattern::ConstExpression(pat::ConstExpression::Num(2)),
                pat::Pattern::ConstExpression(pat::ConstExpression::Num(3)),
            ]),
            pat::Pattern::ConstExpression(pat::ConstExpression::True),
        ]);
        assert_eq!(pat_contributes(&d, &pat3), true);
    }
    #[test]
    fn test_case_contributes() {
        // Check guard case
        assert_eq!(
            case_contributes(
                &types::Type::Primitive(types::Primitive::Bool),
                &pat::InspectExpressionCase {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: Some(pat::Guard {})
                }
            ),
            false
        );
        // Check no-guard case 1
        assert_eq!(
            case_contributes(
                &types::Type::Primitive(types::Primitive::Bool),
                &pat::InspectExpressionCase {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            true
        );
        // Check no-guard case 2
        assert_eq!(
            case_contributes(
                &types::Type::Class(types::Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &pat::InspectExpressionCase {
                    pattern: pat::Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            false
        );
    }
}

fn main() {}
