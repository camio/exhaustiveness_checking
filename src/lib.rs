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
            Type::Primitive(Primitive::Bool) => {
                vec![pat::Constructor::True, pat::Constructor::False]
            }
            Type::Primitive(Primitive::Int) => panic!("Cannot enumerate 'int' constructors."),
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

pub fn d_types(types: &Vec<Rc<Type>>) -> Vec<Rc<Type>> {
    types[1..].to_vec()
}
pub fn d(p: &Vec<Vec<Pattern>>) -> Vec<Vec<Pattern>> {
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

/// Return 'true' if there exists a sequence of values with the specified `types` that the
/// specified 'q' pattern sequence matches that is not matched by one of he pattern sequences in he
/// specified 'p' and 'false' otherwise. The behavior is undefined unless 'pat_contributes(pat) =
/// true' for all patterns 'pat' within 'p'. The behavior is also undefined unless
/// 'pat_contributes(q) = true'.
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
            let c = constructor_from_pattern(constructed_pattern);
            useful(&s_types(types), &s(&c, &p), &s(&c, &vec![q.clone()])[0])
        }
    }
}

/// Return 'true' if the specified 'patterns' form an exhaustive set for the specified
/// 'pattern_type' and 'false' otherwise.
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
