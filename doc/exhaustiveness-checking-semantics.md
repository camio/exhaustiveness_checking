# C++ Pattern Matching Exhaustiveness Checking

This proposal incorporates a feature called "exhaustiveness checking" which
enables several common bugs related to usage of pattern matching to be detected
and diagnosed at compile time. A simple example of such a bug can be found in
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
  /pat₁/ => code₁  // arm₁
  /pat₂/ => code₂  // arm₂
         ⋮
  /patₙ/ => codeₙ  // armₙ
}
```

, must, for every *q*-value (*qᵢ*) of `T`, include an arm (armⱼ) that
*q*-matches that *q*-value (*q*-match(*qᵢ*, armⱼ)). *q*-values are defined on a
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

Wildcards and bindings, unsurprisingly match any *q*-value.

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

Classes with data members have *q*-values based on its fields. These *q*-values
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
  and class being matched has *deep derived equality*.

A class `C` has *deep derived equality* if the following conditions are met:

1. `C` has a defaulted `operator==`.
2. All of `C`'s fields are `std::nullptr_t`, `bool`, or are classes having *deep
   derived equality*.

### Tuple-like types

### Variant-like types
