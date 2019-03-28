# pl-lab

Self-study on programming languages design and implementation.

# LamCalc.Untyped.Naive

A naive implementation of capture avoiding substitution, weak head normal and normal form conversion, α- and β-equivalence.

# Building & running

The elementary REPL computes the normal form of an untyped λ-calculus expression.

```
$ stack build && stack exec pl-lab-exe
λ> (\x y z. x z (y z)) (\x y. x) (\x y. x)
λz. z
λ> :q
```
