pub mod pat;
pub mod types;
pub mod contrib;

use pat::Pattern;
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

            // TODO: Implement 'unimplemented!' sections

            if complete_root_constructors {
                match t1 {
                    Type::Primitive(types::Primitive::Bool) => {
                        // TODO: types_prime is computed in the same way both here and below. First
                        // need to verify that it is correct. Second I need to somehow abstract out
                        // the code.

                        // Set 'types_prime' to the type associated with the recursive call to 'useful2'.
                        let mut types_prime: Vec<Rc<Type>> = Vec::new();
                        match t1 {
                            Type::Primitive(_) => (),
                            Type::Class(types::Class {
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
                    Type::Class(_) => unimplemented!(),
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
            let mut types_prime: Vec<Rc<Type>> = Vec::new();
            match t1 {
                Type::Primitive(_) => (),
                Type::Class(types::Class {
                    derived_eq: _,
                    fields,
                }) => types_prime.extend(fields.iter().cloned()),
            }
            types_prime.extend(types[1..].iter().cloned());

            useful2(&types_prime, &s(&c, &p), &s(&c, &vec![q.clone()])[0])
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
