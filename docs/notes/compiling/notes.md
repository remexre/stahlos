Categorical Abstract Machine
============================

These are some notes on the CAM. Read the original paper for more detail.

Static Combinators
------------------

Nullary:

-	`app`
-	`fst`
-	`snd`
-	`'` (takes a value as argument)

Unary: `Λ`

Binary: `_ ∘ _`, `<_, _>`

### Translation from λ

(where `0`, `1+n` are de Brujin indices)

-	`[[0]] = snd`
-	`[[1+n]] = [[n]] ∘ fst`
-	`[[c]] = 'c`
-	`[[f x]] = app ∘ <[[f]], [[x]]>`
-	`[[λ x]] = Λ([[x]])`

Dynamic Combinators
-------------------

At runtime (if executing by term rewriting), pairing and combinator application need to be introduced.

-	`_ $ _` (not to be confused with `app`, confusingly)
-	`(_, _)`

### Evaluation

The following evaluation rules are applied repeatedly to reduce an expression.

-	`((x ∘ y) $ z) → x $ (y $ z)`
-	`(fst $ (x, y)) → x`
-	`(snd $ (x, y)) → y`
-	`(<x, y> $ z) → (x $ z, y $ z)`
-	`(app $ (Λ(x) $ y, z)) → (x $ (y, z))`
-	`('(x) $ y) → x`

Simple Example
--------------

As an example, we use the program `K I u v`, where `K` is the function with the type `α → β → α`, `I` is the function with the type `α → α`, and `u` and `v` are constants.

### Compiling

-	`K I u v`
-	`(λx. λy. x) (λx. x) u v` (expand `K`, `I` to lambdas)
-	`(λλ1) (λ0) u v` (convert to de Brujin indices)
-	`[[(λλ1) (λ0) u v]]` (start compiling)
-	`app ∘ <[[(λλ1) (λ0) u]], [[v]]>`
-	`app ∘ <[[(λλ1) (λ0) u]], 'v>`
-	`app ∘ <app ∘ <[[(λλ1) (λ0)]], [[u]]>, 'v>`
-	`app ∘ <app ∘ <[[(λλ1) (λ0)]], 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <[[λλ1]], [[λ0]]>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <[[λλ1]], Λ([[0]])>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <[[λλ1]], Λ(snd)>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <Λ([[λ1]]), Λ(snd)>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <Λ(Λ([[1]])), Λ(snd)>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <Λ(Λ([[0]] ∘ fst)), Λ(snd)>, 'u>, 'v>`
-	`app ∘ <app ∘ <app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)>, 'u>, 'v>`

### Executing

To execute, we apply a token for the empty environment, here `∅`.

-	`app ∘ <app ∘ <app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)>, 'u>, 'v> $ ∅`
-	`app $ (<app ∘ <app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)>, 'u>, 'v> $ ∅)`
-	`app $ (app ∘ <app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)>, 'u> $ ∅, 'v $ ∅)`
-	`app $ (app $ (<app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)>, 'u> $ ∅), 'v $ ∅)`
-	`app $ (app $ (app ∘ <Λ(Λ(snd ∘ fst)), Λ(snd)> $ ∅, 'u $ ∅), 'v $ ∅)`
-	`app $ (app $ (app $ (<Λ(Λ(snd ∘ fst)), Λ(snd)> $ ∅), 'u $ ∅), 'v $ ∅)`
-	`app $ (app $ (app $ (Λ(Λ(snd ∘ fst)) $ ∅, Λ(snd) $ ∅), 'u $ ∅), 'v $ ∅)`
-	`app $ (app $ (Λ(snd ∘ fst) $ (∅, Λ(snd) $ ∅), 'u $ ∅), 'v $ ∅)`
-	`app $ ((snd ∘ fst) $ ((∅, Λ(snd) $ ∅), 'u $ ∅), 'v $ ∅)`
-	`app $ (snd $ (fst $ ((∅, Λ(snd) $ ∅), 'u $ ∅)), 'v $ ∅)`
-	`app $ (snd $ (∅, Λ(snd) $ ∅), 'v $ ∅)`
-	`app $ (Λ(snd) $ ∅, 'v $ ∅)`
-	`snd $ (∅, 'v $ ∅)`
-	`'v $ ∅`
-	`v`

Machine Interpretation
----------------------

`fst` and `snd` have an obvious interpretation as traversers on a linked list composed of `(_, _)` pairs. We compile composition as concatenation, i.e. we compile `x ∘ y` to `y x`. We can assign an interpretation to `<_, _>` by assigning meaning to each token:

-	`<` -- push term to top of stack
-	`,` -- swap term with top of stack
-	`>` -- make a pair out of the top of stack and the term, replacing the current term, and pop the stack

For `Λ(C)`, we replace the current term `s` with `C:s`. (This is just a change of syntax, since `Λ(C)s` is a value if `s` is a value.)

`app` expects a term `(C:s, t)` and replaces it with `(s, t)`, then runs `C` before the remaining code.

Lastly, `'` acts largely as before, but replaces the current term with the associated value.

To summarize, the compilation rules from static combinators are:

-	`[[app]]    → app`
-	`[[fst]]    → fst`
-	`[[snd]]    → snd`
-	`[['x]]     → quote(x)`
-	`[[Λ(x)]]   → lam([[x]])`
-	`[[x ∘ y]]  → [[y]] [[x]]`
-	`[[<x, y>]] → push [[x]] swap [[y]] cons`

And the evaluation rules (on a `[Code | Term | Stack]` triple) are:

-	`[app     ,C | ((B:X), Y) |   S] → [B,C | (X, Y) |   S]`
-	`[lam(B)  ,C |      X     |   S] → [  C |   B:X  |   S]`
-	`[fst     ,C |   (X, Y)   |   S] → [  C |    X   |   S]`
-	`[snd     ,C |   (X, Y)   |   S] → [  C |    Y   |   S]`
-	`[cons    ,C |      Y     | X,S] → [  C | (X, Y) |   S]`
-	`[push    ,C |      X     |   S] → [  C |    X   | X,S]`
-	`[quote(X),C |      Y     |   S] → [  C |    X   |   S]`
-	`[swap    ,C |      X     | Y,S] → [  C |    Y   | X,S]`

Optimization
------------

First, we notice that:

-	`fst ∘ <x, y> = x`
-	`snd ∘ <x, y> = y`

We then introduce an identity combinator, `id`, and the rewrite rule `app ∘ <Λ(x), y> = x ∘ <id, y>`.

We can compile `id` to an empty sequence of instructions. Because of this, the sequence `push swap` is now possibly present -- this can be optimized to `push`, since the top of stack will be the same as the current term.

Compiling to Forth
==================

In Forth, we only have the data stack, rather than a data stack and a current term, so we use the top slot of the data stack as the current term. We also assign names to the code sequences in the `lam` instruction, which are compiled to Forth words.
