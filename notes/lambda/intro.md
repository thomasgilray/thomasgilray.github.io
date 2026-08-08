---
title: The three essential forms
nav: Three essential forms
tldr: >
  The heart of any functional programming language is the λ-calculus: a tiny
  language with just unary functions, function application, and variable
  reference. Everything else in Scheme—or any other programming language—can
  be compiled down to these three forms.
---

The heart of any functional programming language are the three essential forms of the *lambda calculus*: lambda abstraction (defining a function), application (invoking a function), and variable reference. In fact, the rest of Scheme (or any other programming language) can be compiled down into the language consisting of just these three forms: defining unary (single input) functions that bind a variable when invoked, invoking functions on a single argument expression, and referencing a variable. Compiling down to the pure lambda calculus is called *Church compiling/encoding*, after the creator of the lambda calculus, Alonzo Church.

## A grammar for λ-terms

While there are many different lambda calculi—systems for calculating (calculi) using functions (lambdas)—*the lambda calculus* generally refers to a specific classic system: the untyped, three-form lambda calculus with unary functions. We can define a grammar for its expressions/terms like so:

```latex
\begin{array}{rcll}
e,t \in \mathrm{E} & ::= & \texttt{(\lm{} (x) e$_{b}$)} & [\text{lambda abstraction}]\\[3pt]
 & \vert & \texttt{(e$_{f}$ e$_{a}$)} & [\text{application}]\\[3pt]
 & \vert & \texttt{x} & [\text{variable reference}]\\[9pt]
x,y \in \mathrm{Var} & = & \multicolumn{2}{l}{\langle \text{program identifiers} \rangle}
\end{array}
```

It's also common to see a notation without (required) parentheses and with a dot before the body of lambdas—written $\lm x.e_{b}$, $e_{f}\;e_{b}$, and $x$—where $\lm$ is highest precedence.

## First-class functions

Lambda calculus is found within many programming languages; it is the heart of functional programming languages such as Racket, Haskell, OCaml, but is also found within multi-paradigm languages permitting first-class functions such as Python, Ruby, Javascript, and even Java, and C++. First-class functions are now in most modern languages, e.g.:

```
Python:       id = lambda x: x
Java:         Function<Object, Object> id = x -> x;
Javascript:   const id = x => x;
```

## Applying the identity function

With an application form, we may apply an identity function on a value, which will yield that value unchanged. Let's take the identity function `(λ (x) x)` and apply it to the number 5. This expression may be textually simplified using an evaluation-step relation ($\rightarrow$): $\texttt{((\lm{} (x) x) 5)} \rightarrow \texttt{5}$. Racket agrees—this example is live, so press run (and edit it, if you like):

``` racket run mode=expr
((λ (x) x) 5)
```

In the pure lambda calculus, we do not have the ability to represent a constant like 5, but could still apply the identity function on itself: $\texttt{((\lm{} (x) x) (\lm{} (y) y))} \rightarrow \texttt{(\lm{} (y) y)}$.

``` racket run mode=expr
((λ (x) x) (λ (y) y))
```

Racket prints the resulting value as an opaque `#<procedure>`; in the pure λ-calculus we would write the answer as the term `(λ (y) y)` itself.
