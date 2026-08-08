---
title: Church encodings
nav: Church encodings
tldr: >
  The pure λ-calculus is Turing-equivalent. Currying encodes multi-argument
  functions, selector functions encode booleans and conditionals, Church
  numerals encode arithmetic, callbacks encode lists, and the Y combinator
  recovers recursion—using nothing but unary lambdas.
---

If a language is Turing equivalent (i.e., of the same expressive power as a Turing machine) then we would expect it to be able to do an unbounded amount of work and therefore nonterminate. As we have seen before, we can express the $\Omega$ term in the pure λ-calculus, which exemplifies non-termination. But this alone is not too convincing. The question remains: how can we express arbitrary Scheme programs in the pure λ-calculus, using only unary function definition, unary function application, and variable reference?

The answer is called Church compiling or Church encoding, and represents either a way of using the λ-calculus as a practical programming language or a way of systematically compiling a richer language (like Scheme) down to the λ-calculus. There are various alternative approaches to Church encoding basic values like numbers, lists, or Booleans, but we will focus here on a particular, traditional set of encodings.

## Currying

First, we may encode k-ary functions as unary higher-order functions by taking each parameter one at a time—a technique known as *Currying*, named for logician Haskell Curry who popularized the technique. A binary function thus becomes two nested unary functions:

```latex
\texttt{(\lm{} (x y) e)} \;\equiv\; \texttt{(\lm{} (x) (\lm{} (y) e))}
```

We must transform k-ary application into a Curried form as well. For example:

```latex
\texttt{(f x y z)} \;\equiv\; \texttt{(((f x) y) z)}
```

A Curried binary function is applied one argument at a time—here, a function expecting two arguments (that returns its first) is applied on 1, returning a function which is then applied on 2:

``` racket run mode=expr
(((λ (x) (λ (y) x)) 1) 2)
```

Thunks, or lambdas with no parameters, can be encoded by taking a dummy parameter that goes unused. Similarly, application of a thunk can be converted to provide a arbitrary value as its argument.

## Booleans and conditionals

What about true and false?

```latex
\begin{array}{lcl}
\texttt{\#t} & \equiv & \texttt{(\lm{} (x) (\lm{} (y) x))}\\[4pt]
\texttt{\#f} & \equiv & \texttt{(\lm{} (x) (\lm{} (y) y))}
\end{array}
```

We encode Booleans as selecting functions that take two arguments and select one. The function encoding `#t` returns the first argument provided and ignores the second whereas `#f` returns the second argument, discarding the first.

The live examples in this section share the definitions below (Racket needs names for our encodings, so we call them `tru` and `fls`; the `provide` makes them visible to the examples that include this file):

``` racket run name=bools.rkt no-run label="bools.rkt"
(provide (all-defined-out))

;; Booleans as selector functions
(define tru (λ (x) (λ (y) x)))
(define fls (λ (x) (λ (y) y)))
```

``` racket run mode=expr include=bools.rkt
((tru 1) 2)
```

We can encode conditional `if` forms as application of the guard expression on the then-and-else subexpressions.

```latex
\texttt{(if g t f)} \;\equiv\; \texttt{((g t) f)}
```

In the plain λ-calculus, without a fixed evaluation order, or using CBN or Normal evaluation, this encoding will work fine because argument evaluation is delayed/lazy. In CBV evaluation however, we have just changed the termination behavior of a program like $\texttt{(if \#f \ensuremath{\Omega} \#f)}$, because arguments will be evaluated before selection instead of being delayed. To fix this for CBV, we can η expand the two branches, or explicitly wrap them in thunks and then apply the selected thunk after returning or inside the Boolean (encoded as a function). E.g.,

```latex
\begin{array}{lcl}
\texttt{(if g t f)} & \equiv & \texttt{(((g (\lm{} (\_) t))}\\
 & & \texttt{\phantom{((}(\lm{} (\_) f))}\\
 & & \texttt{\phantom{(}(\lm{} (x) x))}
\end{array}
```

Racket is CBV, so the thunked encoding is the one that runs correctly—here the guard is `fls`, so the second thunk is selected and applied:

``` racket run mode=expr include=bools.rkt
(((fls (λ (_) 1)) (λ (_) 2)) (λ (x) x))
```

## Church numerals

We can encode a natural number $n$ as a function that accepts another function $f$ and returns a function that performs $f$, iterated $n$ times. For example:

```latex
\begin{array}{lcl}
\texttt{0} & \equiv & \texttt{(\lm{} (f) (\lm{} (x) x))}\\[4pt]
\texttt{1} & \equiv & \texttt{(\lm{} (f) (\lm{} (x) (f x)))}\\[4pt]
\texttt{2} & \equiv & \texttt{(\lm{} (f) (\lm{} (x) (f (f x))))}
\end{array}
```

The encoding for a binary addition function takes two Church encoded numbers, `n` and `m`, and returns a number, i.e., some `(λ (f) (λ (x) ...))`, which encodes the number $n+m$ by using `n` and then `m` to apply `f`, $n$ times followed by $m$ times:

```latex
\begin{array}{lcl}
\texttt{+} & \equiv & \texttt{(\lm{} (n) (\lm{} (m)}\\
 & & \texttt{\phantom{((}(\lm{} (f) (\lm{} (x)}\\
 & & \texttt{\phantom{(((\ }((m f) ((n f) x))))))}
\end{array}
```

The encoding for a binary multiplication function is quite similar, except it exploits the equality $(f^{m})^{n} = f^{(m \cdot n)}$ to accomplish multiplication:

```latex
\begin{array}{lcl}
\texttt{*} & \equiv & \texttt{(\lm{} (n) (\lm{} (m)}\\
 & & \texttt{\phantom{((}(\lm{} (f) (\lm{} (x)}\\
 & & \texttt{\phantom{(((\ }((n (m f)) x)))))}
\end{array}
```

Again the live examples share a definitions file. Note `church->number` at the bottom: it decodes a Church numeral by applying it to Racket's `add1` and `0`—performing "add one to zero" $n$ times:

``` racket run name=nums.rkt no-run label="nums.rkt"
(provide (all-defined-out))

;; Church numerals
(define zero  (λ (f) (λ (x) x)))
(define one   (λ (f) (λ (x) (f x))))
(define two   (λ (f) (λ (x) (f (f x)))))
(define three (λ (f) (λ (x) (f (f (f x))))))

;; Arithmetic
(define plus  (λ (n) (λ (m) (λ (f) (λ (x) ((m f) ((n f) x)))))))
(define times (λ (n) (λ (m) (λ (f) (λ (x) ((n (m f)) x))))))

;; Decode a Church numeral as an ordinary Racket number
(define (church->number n) ((n add1) 0))
```

``` racket run mode=expr include=nums.rkt
(church->number ((plus two) three))
```

``` racket run mode=expr include=nums.rkt
(church->number ((times two) three))
```

## Encoding lists

To encode linked lists in Scheme, we need an encoding for cons cells and for null that distinguishes between these two cases and that can destructure a cons cell and give us back its car and cdr values. Null and cons can be encoded, in a similar manner to Booleans, as functions that take two callback functions, invoking the first when it encodes a cons call and the second when it encodes null. The callback for a cons cell takes two values (the contents of the cons cell) while the callback for null is an encoding of a thunk and takes no meaningful parameters.

```latex
\begin{array}{lcl}
\texttt{'()} & \equiv & \texttt{(\lm{} (when-cons)}\\
 & & \texttt{\phantom{(}(\lm{} (when-null)}\\
 & & \texttt{\phantom{((}(when-null (\lm{} (x) x))))}\\[8pt]
\texttt{cons} & \equiv & \texttt{(\lm{} (a) (\lm{} (b)}\\
 & & \texttt{\phantom{((}(\lm{} (when-cons)}\\
 & & \texttt{\phantom{(((}(\lm{} (when-null)}\\
 & & \texttt{\phantom{((((}((when-cons a) b)))))}
\end{array}
```

Thus we can write a definition for `car` like so:

```latex
\begin{array}{lcl}
\texttt{car} & \equiv & \texttt{(\lm{} (p) ((p (\lm{} (a) (\lm{} (b) a)))}\\
 & & \texttt{\phantom{(\lm{} (p) (}(\lm{} (\_) $\Omega$)))}
\end{array}
```

Note that `(car '())` is an error, which we simply represent here as nontermination. Based on the encodings above, how might you write other primitive utilities like `null?` and `cdr`?

These definitions run as-is in Racket—module-level definitions are allowed to shadow the built-in `cons`, `car`, and `null`:

``` racket run name=lists.rkt no-run label="lists.rkt"
(provide (all-defined-out))

;; The empty list invokes its second callback
(define null (λ (when-cons)
               (λ (when-null)
                 (when-null (λ (x) x)))))

;; A cons cell invokes its first callback on its two contents
(define cons (λ (a) (λ (b)
               (λ (when-cons)
                 (λ (when-null)
                   ((when-cons a) b))))))

;; car selects the first field; car of null diverges (Ω)
(define car (λ (p)
              ((p (λ (a) (λ (b) a)))
               (λ (_) ((λ (u) (u u)) (λ (u) (u u)))))))
```

``` racket run mode=expr include=lists.rkt
(car ((cons 1) 2))
```

``` racket run mode=expr include=lists.rkt label="car of null is Ω (a timeout)"
(car null)
```

## Recursion: the U and Y combinators

Finally, we need a way to implement recursion as the plain λ-calculus has no recursive binding forms like `define` or `letrec` in Scheme, which we could otherwise use to define a function in terms of itself. Instead, the only binding form we have is λ. A standard approach is to define a fixpoint combinator, which can be used to compute a fixed point for a function, so that to define a function `f = (λ (x) ...)` in terms of itself, we wrap this function in a function binding `f`, i.e., `(λ (f) (λ (x) ...))`, and then compute a fixed point for this function, which would be a function `f` that is closed over a binding for itself. A fixed-point combinator is known as a Y combinator and may be defined for the call-by-value λ-calculus like so:

```latex
\begin{array}{lcl}
\texttt{U} & \equiv & \texttt{(\lm{} (a) (a a))}\\[8pt]
\texttt{Y} & \equiv & \texttt{(U (\lm{} (y)}\\
 & & \texttt{\phantom{(U\ }(\lm{} (f)}\\
 & & \texttt{\phantom{(U\ (}(f (\lm{} (x)}\\
 & & \texttt{\phantom{(U\ ((f\ }(((U y) f) x))))))}
\end{array}
```

``` racket run name=rec.rkt no-run label="rec.rkt"
(provide (all-defined-out))

;; U: self-application; Y: a call-by-value fixpoint combinator
(define U (λ (a) (a a)))
(define Y (U (λ (y)
               (λ (f)
                 (f (λ (x)
                      (((U y) f) x)))))))
```

To see how this may be used, consider the example below, where we calculate the factorial of the number 5 using the Y combinator:

``` racket run mode=expr include=rec.rkt
((Y (λ (f)
      (λ (x)
        (if (= x 0)
            1
            (* x (f (- x 1)))))))
 5)
```

The Y combinator may be used to simulate recursion by using the U combinator instead of having the ability to directly create self-referential definitions. U is used to duplicate the function at each call so that it may both be called and also passed down to be called yet again if needed. Try the code above and work out how each part of the Y combinator functions. How might you compile a `letrec` form in Scheme to the plain λ-calculus using Y?
