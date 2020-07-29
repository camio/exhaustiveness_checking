# C++ Pattern Matching Exhaustiveness Checking

This proposal incorporates a feature called "exhaustiveness checking" which
enables the compile time diagnosis of several common bugs related to usage of
pattern matching. A simple example of such a bug can be found in
the following snippet where a boolean is converted to a string.

```c++
bool b = /* <snip> */;
const std::string str = inspect(b) {
    true => "true";                     // ERROR: Value 'false' not matched by
                                        // any pattern.
};
```

Note that compilers must produce diagnostics for failures identified by the
exhaustivness checker.

## Exhaustiveness Checking Semantics

For an expression `e` of type `T`, any `inspect` expression,

```c++
inspect(e) {
  /case₁/ => code₁  // arm₁
  /case₂/ => code₂  // arm₂
         ⋮
  /caseₙ/ => codeₙ  // armₙ
}
```

, must, for every *q*-value (*qᵢ*) of `T`, include an arm (armⱼ) that
*q*-matches that *q*-value (*q*-match(*qᵢ*, armⱼ) = `true`). *q*-values are defined on a
per-type basis and the *q*-match function is defined on a per-pattern basis.

### *q*-match and guards

Arms with guards do not contribute to compile-time exhaustiveness checking due
to their runtime semantics. Therefore, we have the following rule:

* *q*-match( *v*, `pat` *inspect-guard*) is `false` for every *q*-value
  *v* and pattern `pat`.

### *q*-values for fundamental types

* `std::nullptr_t` is defined to have a single *q*-value, `nullptr`.
* `bool` is defined to have two *q*-values, `true`, and `false`.
* The remaining fundamental types are defined to each have a single *q*-value
  *ε*.

### *q*-match for fundamental types

Three patterns apply to fundamental types: wildcards (*wildcard-pattern*),
bindings (*binding-pattern*), and expressions (*expression-pattern*).

Wildcards and bindings, unsurprisingly, match any *q*-value.

* *q*-match( *v*, *wildcard-pattern* ) is `true` for every *q*-value *v*
* *q*-match( *v*, *binding-pattern* ) is `true` for every *q*-value *v*

Expression patterns *q*-match only when the expression evaluates to the
particular *q*-value.

* *q*-match( *v*, *expression-pattern* ) is `true` if the expression pattern
  evaluates to *q*-value *v* and `false` otherwise.

Note that because expression patterns cannot evaluate to ε, *q*-match( ε,
*expression-pattern* ) is always `false`.

### Classes

Classes without data members have a single *q*-value `{}` and we have the
following rules:

* *q*-match( `{}`, *wildcard-pattern* ) is `true`
* *q*-match( `{}`, *binding-pattern* ) is `true`
* *q*-match( `{}`, *expression-pattern* ) is `true`
* *q*-match( `{}`, `[]` ) is `true`

Classes with data members have *q*-values based on their fields. These *q*-values
are of the form `{` *v₁*, *v₂*, …, *vₙ* `}` where *vᵢ* ranges over the
*q*-values of the *i*th data member of the class. The following *q*-match rules
apply:

* *q*-match( `{` *v₁*, *v₂*, …, *vₙ* `}`, *wildcard-pattern* ) is `true`
* *q*-match( `{` *v₁*, *v₂*, …, *vₙ* `}`, *binding-pattern* ) is `true`
* *q*-match( `{` *v₁*, *v₂*, …, *vₙ* `}`, `[` pat₁, pat₂, …, patₙ `]` ) is
  `true` if *q*-match(*vᵢ*, patᵢ)=`true` for every *i*, and `false` otherwise.

Expression patterns *q*-match classes only if the class type is said to have
*deep derived equality*.

* *q*-match( `{` *v₁*, *v₂*, …, *vₙ* `}`, *expression-pattern* ) is `true` if
  `{` *v₁*, *v₂*, …, *vₙ* `}` has the same value as the *expression-pattern*
  and the class being matched has *deep derived equality*.

A class `C` has *deep derived equality* if the following conditions are met:

1. `C` has a defaulted `operator==`.
2. All of `C`'s fields are `std::nullptr_t`, `bool`, or are classes having *deep
   derived equality*.

Finally, classes that are polymorphic have the additional *q*-match rule:

* *q*-match( *v*, < *type* > *pattern* ) is `false`

### Tuple-like types

Tuple-like types are those that opt-in to structured binding syntax by
specializing `std::tuple_size`, `std::get`, and `std::tuple_element`. Like
classes, tuple-like types `T` have *q*-values of the form `{` *v₁*, *v₂*, …,
*vₙ* `}`, but where *vᵢ* ranges over the *q*-values of `std::tuple_element<*i*,
T>::type`.

The *q*-match rules are identical to those with classes except
*q*-match always returns false for *expression-patterns*.

* *q*-match( `{` *v₁*, *v₂*, …, *vₙ* `}`, *expression-pattern* ) is `false` if
  the class being matched is a tuple-like type.

### Variant-like types

Variant-like types are those that opt-in to pattern matching syntax by
specializing `std::variant_size`, `std::holds_alternative`, `std::get`, and
`std::variant_alternative`.  *q*-values of variant-like types `V` are of the
form (*i*, *v*) where 0 <= *i* < `std::variant_size<V>::value` and *v* ranges
over the *q*-values of `std::variant_alternative<*i*, V>::type`.

Our matching rules are as follows:

* *q*-match( `(*i*, *v*)`, *wildcard-pattern* ) is `true`
* *q*-match( `(*i*, *v*)`, *binding-pattern* ) is `true`
* *q*-match( `(*i*, *v*)`, *expression-pattern* ) where *epat* is an
  *expression-pattern* is `true` if and only if

  1. the expression evaluates to a value *w* where
     `std::holds_alternative<*i*>(*w*) = true`,
  2. *q*-match( *v*, `std::get<*i*>(*w*)` ) = `true`,
  3. the `std::holds_alternative<*i*>` specialization is `constexpr`, and
  4. the `std::get<*i*>` specialization is `constexpr`.
* *q*-match( `(*i*, *v*)`, < auto > *pat* ) is `true` if and only if *q*-match(
  *v*, *pat* ) is true.
* *q*-match( `(*i*, *v*)`, < *concept* > *pat* ) is `true` if and only if
  `std::variant_alternative<i,V>::type` satisfies the concept and *q*-match(
  *v*, *pat* ) is true.
* *q*-match( `(*i*, *v*)`, < *type* > *pat* ) is `true` if and only if
  `std::variant_alternative<i,V>::type` is the same as *type* and *q*-match(
  *v*, *pat* ) is true.
* *q*-match( `(*i*, *v*)`, < *constant-expression* > *pat* ) is `true` if and
  only if the expression evaluates to *i* and *q*-match( *v*, *pat* ) is true.

### Any-like types

Any-like types are those that opt-in to pattern matching syntax by specializing
the `any_cast` function template. All such types `A` have a single *q*-value ε
with the following *q*-match rules.

* *q*-match( ε, *wildcard-pattern* ) is `true`
* *q*-match( ε, *binding-pattern* ) is `true`
* *q*-match( ε, *expression-pattern* ) is `false`
* *q*-match( ε, < *type* > *pattern* ) is `false`

### Pointer-like types

TBD

### Extractor patterns

TBD

## Required library extensions for variant-like types

* We make use of the `std::holds_alternative<*i*>` expression where *i* is a
  `constexpr` integral value. This currently isn't supported.
