//! Provide algorithms that determine whether patterns contribute to exhaustiveness checking.

use crate::pat;
use crate::pat::Pattern;
use crate::types::Class;
use crate::types::Primitive;
use crate::types::Type;

/// Return `true` if the specified `pattern`, assumed to have the specified `pattern_type`,
/// contributes to the computation of whether or not the enclosing inspect statement is exhaustive.
pub fn pat_contributes(pattern_type: &Type, pattern: &Pattern) -> bool {
    match (pattern_type, pattern) {
        (_, Pattern::Wildcard) => true,
        (Type::Primitive(Primitive::Int), Pattern::ConstExpression(_)) => false,
        (Type::Primitive(_), _) => true,
        (Type::Class(c), Pattern::ConstExpression(_)) => c.deep_derived_eq(),
        (
            Type::Class(Class {
                derived_eq: _,
                fields,
            }),
            Pattern::StructuredBinding(pats),
        ) => fields
            .iter()
            .zip(pats.iter())
            .all(|(t, p)| pat_contributes(t, p)),
    }
}

/// Return `true` if the specified `arm`, assumed to match the specified `pattern_type`,
/// contributes to the computation of whether or not the enclosing inspect statement is exhaustive.
pub fn arm_contributes(pattern_type: &Type, arm: &pat::InspectArm) -> bool {
    if arm.guard.is_some() {
        false
    } else {
        pat_contributes(&pattern_type, &arm.pattern)
    }
}

/// Return patterns within the specified `arms`, all assumed to have the specified `pattern_type`,
/// that contribute to the computation of whether or not the enclosing inspect statement is
/// exhaustive.
pub fn filter_noncontributors(pattern_type: &Type, arms: &Vec<pat::InspectArm>) -> Vec<Pattern> {
    arms.iter()
        .filter(|c| arm_contributes(&pattern_type, c))
        .map(|c| c.pattern.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_filter_noncontributors() {
        // filter_noncontributors( μ⟦ bool ⟧, [] ) ⇒ []
        let ty = Type::Primitive(Primitive::Bool);
        let arms: Vec<pat::InspectArm> = Vec::new();
        assert_eq!(filter_noncontributors(&ty, &arms), Vec::new());

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ __ ⟧] ) ⇒ [μ⟦ __ ⟧]
        let ty = Type::Primitive(Primitive::Bool);
        let arms = vec![pat::InspectArm {
            pattern: Pattern::Wildcard,
            guard: None,
        }];
        assert_eq!(filter_noncontributors(&ty, &arms), vec![Pattern::Wildcard]);

        // filter_noncontributors( μ⟦ bool ⟧, [μ⟦ __ if (g()) ⟧] ) ⇒ [μ⟦ __ ⟧]
        let ty = Type::Primitive(Primitive::Bool);
        let arms = vec![pat::InspectArm {
            pattern: Pattern::Wildcard,
            guard: Some(pat::Guard {}),
        }];
        assert_eq!(filter_noncontributors(&ty, &arms), vec![]);
    }

    #[test]
    fn test_pat_contributes() {
        assert_eq!(
            pat_contributes(
                &Type::Primitive(Primitive::Bool),
                &Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &Type::Primitive(Primitive::Int),
                &Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            false
        );
        assert_eq!(
            pat_contributes(
                &Type::Class(Class {
                    derived_eq: true,
                    fields: vec![]
                }),
                &Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &Type::Class(Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &Pattern::ConstExpression(pat::ConstExpression::Other),
            ),
            false
        );
        assert_eq!(
            pat_contributes(
                &Type::Class(Class {
                    derived_eq: true,
                    fields: vec![]
                }),
                &Pattern::Wildcard,
            ),
            true
        );
        assert_eq!(
            pat_contributes(
                &Type::Class(Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &Pattern::Wildcard,
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

        let c = Rc::new(Type::Class(Class {
            derived_eq: false,
            fields: vec![],
        }));
        let d = Rc::new(Type::Class(Class {
            derived_eq: true,
            fields: vec![c.clone()],
        }));
        assert_eq!(
            pat_contributes(&d, &Pattern::ConstExpression(pat::ConstExpression::Other)),
            false
        );

        // ```c++
        // class c { bool i; bool j; };
        // constexpr c c_val{true,false};
        // class d { C c; bool b; };
        // ```

        let c = Rc::new(Type::Class(Class {
            derived_eq: false,
            fields: vec![
                Rc::new(Type::Primitive(Primitive::Bool)),
                Rc::new(Type::Primitive(Primitive::Bool)),
            ],
        }));

        let c_val = pat::ConstExpression::Other;

        let d = Rc::new(Type::Class(Class {
            derived_eq: false,
            fields: vec![c.clone(), Rc::new(Type::Primitive(Primitive::Bool))],
        }));

        // let pat1 = μ⟦[__, __]⟧
        // pat_contributes( μ⟦C⟧, pat1) ⇒  true

        let pat1 = Pattern::StructuredBinding(vec![Pattern::Wildcard, Pattern::Wildcard]);
        assert_eq!(pat_contributes(&c, &pat1), true);

        // let pat2 = μ⟦[case c_val, true]⟧
        // pat_contributes( μ⟦d⟧, pat2 ) ⇒  false

        let pat2 = Pattern::StructuredBinding(vec![
            Pattern::ConstExpression(c_val.clone()),
            Pattern::ConstExpression(pat::ConstExpression::True),
        ]);
        assert_eq!(pat_contributes(&d, &pat2), false);

        // let pat3 = μ⟦[[true,false], true]⟧
        // pat_contributes( μ⟦d⟧, pat3 ) ⇒  true
        let pat3 = Pattern::StructuredBinding(vec![
            Pattern::StructuredBinding(vec![
                Pattern::ConstExpression(pat::ConstExpression::True),
                Pattern::ConstExpression(pat::ConstExpression::False),
            ]),
            Pattern::ConstExpression(pat::ConstExpression::True),
        ]);
        assert_eq!(pat_contributes(&d, &pat3), true);
    }
    #[test]
    fn test_arm_contributes() {
        // Check guard case
        assert_eq!(
            arm_contributes(
                &Type::Primitive(Primitive::Bool),
                &pat::InspectArm {
                    pattern: Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: Some(pat::Guard {})
                }
            ),
            false
        );
        // Check no-guard case 1
        assert_eq!(
            arm_contributes(
                &Type::Primitive(Primitive::Bool),
                &pat::InspectArm {
                    pattern: Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            true
        );
        // Check no-guard case 2
        assert_eq!(
            arm_contributes(
                &Type::Class(Class {
                    derived_eq: false,
                    fields: vec![],
                }),
                &pat::InspectArm {
                    pattern: Pattern::ConstExpression(pat::ConstExpression::Other),
                    guard: None
                }
            ),
            false
        );
    }
}
