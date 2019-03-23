# pl-lab

Self-study on programming languages design and implementation.

# Building & running

```
$ stack build && stack exec pl-lab-exe
λ> \x. \y. \z. x z (y z) 
Lam ["x"] (Lam ["y"] (Lam ["z"] (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
λ> :q
```
