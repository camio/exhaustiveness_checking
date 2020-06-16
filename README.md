# `exhaustiveness_checking`

**PURPOSE:** Provide exhaustiveness checking algorithms for C++ Pattern Matching.

## Description

This repository contains data structures and algorithms that implement
algorithms for exhaustiveness checking for [Pattern
Matching](http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/p1371r2.pdf)
in C++. The code here is not intended to be complete and merely prototypes the
essential aspects of the implementation.

There are currently three modules implementing various aspects of the system:
`types` implements data structures representing types in C++, `pat` provides an
abstract syntax tree for C++ patterns, and `main` implements the core
algorithms.

## TODO

- Switch to 99 line width.
- Create tests for base cases of the `useful` function.
- Continue implementation of `useful`.
- Implement `is_exhaustive`
- Extend `pat_contributes` to filter out `int` primitive literals. E.g.
  `pat_contributes( μ⟦int⟧, μ⟦1⟧) ⇒ false`.
