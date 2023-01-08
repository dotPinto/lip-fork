# De Bruijn Notation

Write an interpreter for the pure untyped lambda calculus that represents terms internally using the de Bruijn notation.

## Syntax

The concrete syntax of the language is that of named terms, whose abstract syntax is defined as follows:

```ocaml
type namedterm =
    NamedVar of string
  | NamedAbs of string * namedterm
  | NamedApp of namedterm * namedterm
```
A [lexer](lib/lexer.mll) and a [parser](lib/parser.mly) are provided for the above abstract syntax.

The named AST of a term is converted to its nameless equivalent for the purposes of computation. The conversion may be carried out inside the ```parse``` wrapper. Its output should be a tree of the following type:

```ocaml
type dbterm =
    DBVar of int
  | DBAbs of dbterm
  | DBApp of dbterm * dbterm
```

where a variable is represented by its de Bruijn index, which is a pointer to its binding abstraction: the number k encodes the name bound by the k-th enclosing λ. Abstractions need not specify the name they bind anymore, they just carry the body subterm. Applications carry the two subterms being applied.

## Conversion to nameless representation

Define a function ```removenames``` that returns the nameless representation of a named term: 

```ocaml
removenames : namedterm -> int StringMap.t -> dbterm
```
Besides the term to convert, it takes in a naming context for the numbering of free variables, which is addressed in the next paragraph.

Binders are numbered from the inside out, starting from the index 0. 
For example, in the named term ```λx.λy.x y``` the inner λ is numbered 0 and the outer λ is numbered 1. The bound variables x and y are converted to their binder's index, 1 and 0 respectively:
```
λ. λ. 1 0
```

## Free variables and naming context

Clearly the convention for the numbering of bound variables cannot be applied to the free variables of a term, since, by definition, they do not point to any abstraction. To address this problem, an assignment of de Bruijn indexes to names, known as a **naming context**, must be established.

Define a function ```getcontext``` that returns a map from the free variables of a named term to arbitrary, unique integers:

```ocaml
getcontext : namedterm -> int StringMap.t
```
The integers of the mapping range over `[0; n-1]`, where `n` is the number of free variables of the argument named term, and are assigned to the names in their alphabetical order.

Note that when the conversion routine crosses a λ the indeces returned by the context need to be shifted up by one to avoid being captured. We can think of the context becoming "one variable longer", as 0 is now a reserved index that points to said λ. 

For example, under the context Γ that maps x to 0 and y to 1, the correct representation of  ```λz.y z x ``` would be ```λ.2 0 1 ```, not  ```λ.1 0 0 ```.

## Shifting

The shifting operation is essential to nameless substitution. When a term s is substituted under an abstraction, the indeces of its free variables with respect to a context Γ must be shifted up so as to avoid being captured.

However, the bound variables of s must not be shifted: the shifting operation is allowed to alter only those indeces that are greater than a _cutoff_ index, which records the number of abstractions "entered" by the recursion.

Define a function ```shift``` that returns the d-place shift of a term t above a cutoff c:
```ocaml
shift : int -> int -> dbterm -> dbterm
```

## Substitution

The nameless substitution of a term s for a variable index j in a term t, written `[j := s] t`, is defined by the following rules:

```
[j := s] k          = | s, if j = k
                      | k otherwise

[j := s] (λ.t1)     = λ. [j+1 := ↑(1,0)(s)] t1

[j := s] (t1 t2)    = ([j := s] t1) ([j := s] t2)
```

where ```↑(1,0)(s)``` is notation for the 1-place shift of all subterms of s. 

Note that when substitution goes under a lambda, the context becomes one variable larger than the original: the target index is increased by one so that it doesn't clash with the names of the new context, and so are the free variables of s.

Define a function ```subst``` with type:

```ocaml
subst : int -> dbterm -> dbterm -> dbterm
```
where the first parameter is the index to substitute for, the second is the replacement term and the third is the recipient of the substitution. 

The interpretation of the index j in ```[j := s] t``` can be quite confusing. Suppose we want to replace the variable bound by the outermost λ in ```t = λ. λ. 1``` with ```s = 0 1```. A naive, yet tempting attempt at this would be the substitution:
```
[1 := 0 1] (λ. λ. 1) = λ. λ. 1
```
this would be wrong, since by the time the bound variable in question is reached, j would not match its index (see by applying the definition!).

j actually stands for the index of the variable to substitute for in the _outer_ context of t. In particular, ```j = 0``` stands for "the free variable numbered 0 outside of t's binders". If we want to address one the binders of t directly we need to use a negative index. Each time a λ is entered, the context is shifted up by one, and so is j. This logic leads us to the correct attempt:
```
[-1 := 0 1] (λ. λ. 1) = λ. λ. 2 3
``` 
where the free variables of s have been shifted appropriately.

For the purposes of reduction, we are solely concerned with substituting for ```j = 0```.

## β reduction

The way the β-reduction rule employs substitution in the de Bruijn syntax is not as straightforward as it is in the named syntax.

Compare the β reduction rule of named terms, where substitution works on the premises of α conversion:
```
(λx.t12) t2 ~> [x -> t2] t12
```
to that of nameless terms:
```
(λ.t12) t2 ~> ↑(-1,0) ([0 := ↑(1,0) t2] t12)
```

The free variables of the argument are shifted up by one before substitution to ensure they refer to the same names in the new context, which is one variable longer.

Reducing a redex also uses up the outermost λ of the left hand side term, so the result needs to be shifted down by one to preserve the context invariant.

Define a function ```substTop``` that performs β reduction on the subterms of a nameless redex, with type:
```ocaml
substTop : dbterm -> dbterm -> dbterm
```

## Small-step semantics

The small-step semantics of the interpreter employ the normal order strategy. This is a normalizing strategy, that is, it reduces a term to its normal form - if it exists.
```
t -> t'
--------------------- [NO-Abs]
λ. t -> fun x. t'

------------------------------ [NO-AppAbs]
(λ. t1) t2 -> substTop t2 t1

t1 -> t1'
--------------- [NO-App1]
t1 t2 -> t1' t2

t2 -> t2' 
--------------- [NO-App2]
t1 t2 -> t1 t2'
```
Define a function ```trace``` that performs a given number of reductions according to the above rewriting rules:
```ocaml
trace : int -> dbterm -> dbterm list
```
