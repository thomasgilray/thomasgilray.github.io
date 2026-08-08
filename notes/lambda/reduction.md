---
title: Reducing expressions
nav: Reducing expressions
tldr: >
  A reducible expression, or redex, can be simplified by applying a reduction
  rule. β-reduction simulates function application using capture-avoiding
  substitution, α-conversion renames variables without changing meaning, and
  η-reduction eliminates a lambda that merely defers to another function.
---

## β-reduction

A *reducible expression*, or *redex*, is an expression that can be simplified by applying a reduction rule. The primary reduction rule for the λ-calculus is *β reduction*, which simulates function application. We define β reduction for an application expression with a lambda, ready to apply, in function position and any expression in argument position:

```latex
\overbrace{\texttt{((\lm{} (x) e$_{\mathit{body}}$) e$_{\mathit{arg}}$)}}^{\text{redex}}
\;\;\rightarrow_{\beta}\;\;
\overbrace{\texttt{e$_{\mathit{body}}$[x $\mapsto$ e$_{\mathit{arg}}$]}}^{\text{capture-avoiding substitution}}
```

Given a lambda applied on an argument expression, we can textually simulate function application by substituting every occurrence of the formal parameter in the body of the function with the argument. The notation $e_{b}[x \mapsto e_{a}]$ refers to $e_{b}$, the body of the function, except with each reference to $x$ swapped with the argument expression $e_{a}$. This is called *capture-avoiding substitution* (defined below), which is simply a substitution operation on terms in the λ-calculus that respects the rules of scope and binding for the language; for example, substitution must respect shadowing variables:

```latex
\begin{array}{l}
\texttt{((\lm{} (x) (x (\lm{} (x) x))) (\lm{} (y) y))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(x (\lm{} (x) x))[x $\mapsto$ (\lm{} (y) y)]}\\[4pt]
\quad=\;\;\texttt{((\lm{} (y) y) (\lm{} (x) x))}
\end{array}
```

An outer reference to `x` is replaced with the argument (i.e., `(λ (y) y)`) but an inner reference to `x` is not. This is because *static scope* dictates that the `x` in `(λ (x) x)` always refers to the closest enclosing declaration, making this always the identity function, in all contexts. (None of the variables in `(λ (x) x)` are free—none are defined outside the function.) Racket implements exactly this scoping discipline; the value printed below is the inner identity function:

``` racket run mode=expr
((λ (x) (x (λ (x) x))) (λ (y) y))
```

## Free and bound variables

An occurrence of a variable in an expression may be either free or bound. A variable is called *free* in an expression if it is used within that expression but not defined above its use within that expression. λ is a binding operator as it binds a variable within a particular scope (its body). In the identity function, `(λ (x) x)`, the variable `x` in the body is bound by the λ. However, given the function `(λ (b) a)`, we may conclude the variable `a` is a free variable in our function, meaning it must be defined in the context surrounding this lambda in order to have a meaning. In the function `(λ (a) (λ (b) a))`, however, `a` is not free as it is defined by the outer lambda. With respect to the inner lambda, `a` is free because its binding lambda must be located in the *context* of this expression, surrounding it. A variable alone is never called free, but always relative to a sub-expression (scope).

We may formally define free variables like so:

```latex
\begin{array}{lcl}
\mathit{FV}(\texttt{x}) & \triangleq & \{\texttt{x}\}\\[3pt]
\mathit{FV}(\texttt{(e$_{f}$ e$_{a}$)}) & \triangleq & \mathit{FV}(\texttt{e$_{f}$}) \cup \mathit{FV}(\texttt{e$_{a}$})\\[3pt]
\mathit{FV}(\texttt{(\lm{} (x) e$_{b}$)}) & \triangleq & \mathit{FV}(\texttt{e$_{b}$}) - \{\texttt{x}\}
\end{array}
```

To aid in understanding, consider the following examples with their free variables:

```latex
\begin{array}{l@{\qquad\qquad}l}
\texttt{((\lm{} (x) x) y)} & \mathit{FV} = \{\texttt{y}\}\\[3pt]
\texttt{((\lm{} (x) (x x)) (\lm{} (x) (x x)))} & \mathit{FV} = \{\}\\[3pt]
\texttt{((\lm{} (x) (z y)) x)} & \mathit{FV} = \{\texttt{x},\texttt{y},\texttt{z}\}
\end{array}
```

*Open* λ-calculus terms contain free variables, while *closed* terms have no free variables. Closed lambdas are known as *combinators* and are self-contained functions with a stable interpretation across all contexts. Consider some examples:

```latex
\begin{array}{l@{\qquad\qquad}r}
\texttt{(\lm{} (x) (x x))} & \textit{closed}\\[3pt]
\texttt{((\lm{} (x) x) y)} & \textit{open}\\[3pt]
\texttt{(\lm{} (x) ((\lm{} (y) y) ((\lm{} (z) (z z)) d)))} & \textit{open}
\end{array}
```

In Racket, a free variable is an *unbound identifier*—a closed term can evaluate, while an open term is a scoping error before it ever runs:

``` racket run mode=expr label="An open term is an error"
((λ (x) x) y)
```

## Capture-avoiding substitution

As we perform substitution, we must take care to avoid inadvertent capture of any otherwise-bound variables. Let us take the abstract example `((λ (x) e_body) e_arg)`. At a β reduction, we would replace any instances of $x$ within $e_{\mathit{body}}$ with $e_{\mathit{arg}}$. The two expressions involved within the substitution may use the same variable name at distinct binding sites (lambdas). If so, this could lead to us changing the meaning of the resulting expression if we are not careful. To illustrate this, consider the naïve (non-capture-avoiding) substitution:

```latex
\begin{array}{l}
\texttt{((\lm{} (x) ((\lm{} (y) x) z)) y)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (y) y) z)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{z}
\end{array}
```

We've directly substituted `y` for `x` in the expression `((λ (y) x) z)` and that's how we ended up with `(λ (y) y)`. This changes the meaning of the inner lambda (to match the identity function), because the `y` we originally wanted to substitute for was a free variable in the overall expression, and not the same `y` that is bound by the inner lambda. In our resulting expression, the `y` we substituted became incorrectly captured by the inner λ. Our aim is to avoid this through *capture-avoiding substitution*. This works by preventing such substitutions that would lead to a reference being captured by the wrong binding lambda in cases with shadowing.

We can also simply rename variables to eliminate shadowing, using *α-conversion*. Equivalent expressions that only differ in the names of variables are *α-equivalent*. α-conversion does not alter the structure of the lambda like a β reduction would. E.g., $\texttt{(\lm{} (x) x)} \equiv_{\alpha} \texttt{(\lm{} (y) y)}$. In this example, both are equally the identity function. We can define *α equivalence* (or directionally, *α conversion*) using substitution, just as with β, but also must use capture-avoiding substitution:

```latex
\begin{array}{c}
\texttt{(\lm{} (x) e$_{b}$)} \;\equiv_{\alpha}\; \texttt{(\lm{} (y) e$_{b}$[x $\mapsto$ y])}\\[4pt]
\text{where } y \notin \mathit{FV}(e_{b})
\end{array}
```

Renaming variables can enable a correct substitution that would otherwise be *incorrect* using naïve substitution and *undefined* using capture-avoiding substitution. Let's take a look at the same example but with a preparatory renaming before substitution:

```latex
\begin{array}{l}
\texttt{((\lm{} (x) ((\lm{} (y) x) z)) y)}\\[4pt]
\quad\equiv_{\alpha}\;\;\texttt{((\lm{} (x) ((\lm{} (a) x) z)) y)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (a) y) z)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{y}
\end{array}
```

We notice by first α renaming our variables we preserve the meaning of our lambda expression while only changing the names of variables. Now we may formally define *capture-avoiding substitution* so that we stop substitution at a shadowing binding, and forbid substitution where we'd substitute a free variable under a shadowing binding site:

```latex
\begin{array}{lcll}
\texttt{y[x $\mapsto$ e$_{x}$]} & \triangleq & \texttt{e$_{x}$}, & \text{where } x = y\\[5pt]
\texttt{y[x $\mapsto$ e$_{x}$]} & \triangleq & \texttt{y}, & \text{where } x \neq y\\[5pt]
\texttt{(\lm{} (y) e$_{\mathit{body}}$)[x $\mapsto$ e$_{x}$]} & \triangleq & \texttt{(\lm{} (y) e$_{\mathit{body}}$)}, & \text{where } x = y\\[5pt]
\texttt{(\lm{} (y) e$_{\mathit{body}}$)[x $\mapsto$ e$_{x}$]} & \triangleq & \texttt{(\lm{} (y) e$_{\mathit{body}}$[x $\mapsto$ e$_{x}$])}, & \text{where } x \neq y,\; y \notin \mathit{FV}(e_{x})\\[5pt]
\texttt{(e$_{f}$ e$_{a}$)[x $\mapsto$ e$_{x}$]} & \triangleq & \multicolumn{2}{l}{\texttt{(e$_{f}$[x $\mapsto$ e$_{x}$] e$_{a}$[x $\mapsto$ e$_{x}$])}}
\end{array}
```

## η-reduction

We've seen the two principal notions of intensional evaluation: β reduction and α equivalence. Now we'll present a third and final notion of extensional equivalence: η reduction/expansion. While we may avoid η reduction during evaluation (which may use only β), it assists us in establishing meaningful equalities among λ-calculus terms. η equivalence is formally defined like so:

```latex
\begin{array}{c}
\texttt{(\lm{} (x) (e$_{f}$ x))} \;\equiv_{\eta}\; \texttt{e$_{f}$}\\[4pt]
\text{where } x \notin \mathit{FV}(e_{f})
\end{array}
```

In essence, η reduction says any lambda that simply defers to its function-position expression ($e_{f}$) may be reduced to that expression. In this way, it anticipates a β reduction that may or may not be performed later and eliminates the need for it, preemptively. η expansion says that every sub-expression which reduces to a value reduces to a unary lambda equivalent to applying the sub-expression on a fresh formal parameter. η expansion lets us reify an arbitrary expression as the value it denotes by making that lambda explicit. If the expression would non-terminate, this new explicit lambda non-terminates when invoked, and if it yields a value, this new lambda will be equivalent to it. Any expression may be η expanded, but only expressions with the particular shape above may be η reduced.

## Normal forms and Ω

To summarize, the concepts of free variables, α-conversion, β-reduction, η-reduction and capture-avoiding substitution are fundamental to the inner workings of lambda calculus and functional programming. We've seen how we may apply these reductions non-deterministically, essentially using a combined reduction relation ($\rightarrow$):

```latex
(\rightarrow) \;\triangleq\; (\equiv_{\alpha}) \,\cup\, (\rightarrow_{\beta}) \,\cup\, (\equiv_{\eta})
```

We can also define the reflexive, transitive closure of this step relation ($\rightarrow^{*}$), in order to take multiple steps at once. You can read the proposition $e \rightarrow e'$ as "e steps to e prime" or "e reduces to e prime, in one step", while $e \rightarrow^{*} e'$ means "e reduces to e prime" (in some number of steps—which may be zero, meaning they are already equal).

By applying these notions of equivalence or reduction to a term, we may eventually reach a *normal form*: a term that cannot be further reduced and has no internal redexes. However, not all terms in the lambda calculus can be reduced to a normal form. The smallest such term, known as $\Omega$, is `((λ (u) (u u)) (λ (u) (u u)))`. Just `(λ (u) (u u))` is known as the U combinator and is the abstraction of self-application. $\Omega$ is just `(U U)`, the U combinator applied to itself, which reduces to itself in a single beta reduction.

Since Ω β-reduces only to itself, forever, running it never produces a value—the sandbox eventually gives up and reports a timeout:

``` racket run mode=expr label="Ω reduces only to itself"
((λ (u) (u u)) (λ (u) (u u)))
```
