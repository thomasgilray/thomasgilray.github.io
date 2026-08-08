---
title: Evaluation strategies
nav: Evaluation strategies
tldr: >
  The reductions of the λ-calculus may be applied anywhere in a term,
  nondeterministically—an evaluation strategy fixes which redex is next. We
  define applicative-order, normal-order, call-by-value, and call-by-name
  evaluation, formalize them with evaluation contexts, and see how the choice
  of strategy changes termination.
---

We have now seen the principal notions of reduction and equivalence for the λ-calculus, applied non-deterministically to redexes within terms. These rules allow us to rewrite expressions to another equivalent expression, which may be simpler or closer to a normal form. α conversion allows us to rename variables, β-reduction simulates function application (i.e., invocation, instantiation), and η expansion or reduction characterizes a notion of extensional equivalence: that any term is equivalent to that term wrapped in a lambda that applies it, in all possible contexts. We can also choose a specific evaluation strategy and enforce deterministic evaluation.

## Four evaluation strategies

We'll define four primary evaluation strategies: applicative-order evaluation (AOE), normal-order evaluation (NOE), call by value (CBV), and call by name (CBN). In *applicative-order evaluation*, a function's argument is reduced before applying the function itself (i.e., the *rightmost* β redex is always next) while in *normal-order evaluation* we apply the function first without reducing its argument (i.e., the *leftmost* β redex is always next). NOE's delayed evaluation for arguments can also be viewed as a form of *lazy* evaluation, contrasted with the *eager* evaluation of AOE.

Reduction using AOE:

```latex
\begin{array}{l}
\texttt{((\lm{} (y) y) ((\lm{} (z) z) w))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (y) y) w)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{w}
\end{array}
```

Reduction using NOE:

```latex
\begin{array}{l}
\texttt{((\lm{} (y) y) ((\lm{} (z) z) w))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (z) z) w)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{w}
\end{array}
```

Call-by-value (CBV) order is exactly the same as applicative order except it does not reduce under a lambda. Call-by-name (CBN) evaluation is exactly the same as normal-order evaluation except it does not reduce under a lambda. So CBV and CBN are the same as AOE and NOE, respectively, except that we skip over any redexes that are within a (not-yet-applied) lambda.

Reduction using CBV:

```latex
\begin{array}{l}
\texttt{((\lm{} (x) (\lm{} (y) ((\lm{} (z) z) y)))}\\
\texttt{ ((\lm{} (a) a) (\lm{} (b) b)))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (x) (\lm{} (y) ((\lm{} (z) z) y))) (\lm{} (b) b))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) ((\lm{} (z) z) y))}
\end{array}
```

Reduction using CBN:

```latex
\begin{array}{l}
\texttt{((\lm{} (x) (\lm{} (y) ((\lm{} (z) z) y)))}\\
\texttt{ ((\lm{} (a) a) (\lm{} (b) b)))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) ((\lm{} (z) z) y))}
\end{array}
```

Note that CBV reduces arguments before applying functions on them, similar to AOE, while CBN applies functions on argument expressions before reducing them, similar to NOE. Neither CBV or CBN, however, will evaluate under a lambda until that lambda is applied—exposing its body.

## One example, four ways

To see a summary example, let's reduce the following lambda expression using all four reduction strategies:

```latex
\texttt{((\lm{} (x) (\lm{} (y) (x y))) ((\lm{} (a) a) (\lm{} (w) w)))}
```

Reduction using CBV:

```latex
\begin{array}{l}
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (x) (\lm{} (y) (x y))) (\lm{} (w) w))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) ((\lm{} (w) w) y))}
\end{array}
```

Reduction using CBN:

```latex
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) (((\lm{} (a) a) (\lm{} (w) w)) y))}
```

Reduction using AOE:

```latex
\begin{array}{l}
\quad\rightarrow_{\beta}\;\;\texttt{((\lm{} (x) (\lm{} (y) (x y))) (\lm{} (w) w))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) ((\lm{} (w) w) y))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) y)}
\end{array}
```

Reduction using NOE:

```latex
\begin{array}{l}
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) (((\lm{} (a) a) (\lm{} (w) w)) y))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) ((\lm{} (w) w) y))}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (y) y)}
\end{array}
```

Notice how AOE mirrors CBV and NOE mirrors CBN until our expression would require reducing under a lambda/value. In this case, both AOE and NOE continue reducing under the lambda expression while CBV and CBN halt evaluation. A table can be made to visually depict the similarities and differences among the four different evaluation strategies:

|                                          | incl. under λs | not under λs |
| ---------------------------------------- | :------------: | :----------: |
| Reduce arguments first (rightmost redex) |      AOE       |     CBV      |
| Apply function first (leftmost redex)    |      NOE       |     CBN      |

## Confluence

Notice that in our last example, all orders of evaluation reach equivalent values, though some are not yet in a normal form. That this is always the case is known as the *confluence* or *diamond* property of the lambda calculus and is formalized by the Church-Rosser Theorem. This property states that if we start with an expression $e_0$ and by applying different reductions to it we can step to two different intermediate expressions, $e_1$ and $e_2$, then eventually these diverging paths can always reduce further to a common term $e_3$.

```latex
\begin{array}{ccccc}
 & & e_0 & & \\[3pt]
 & \swarrow & & \searrow & \\[3pt]
e_1 & & & & e_2 \\[3pt]
 & {}_{*}\!\searrow & & \swarrow\!{}_{*} & \\[3pt]
 & & e_3 & &
\end{array}
```

## Termination

However, n.b. that this does not guarantee that evaluation will always terminate with a value. Let's consider an example where CBN/NOE will terminate but CBV/AOE does not.

Reduction using CBN/NOE:

```latex
\begin{array}{l}
\texttt{((\lm{} (a) (\lm{} (b) b)) $\Omega$)}\\[4pt]
\quad\rightarrow_{\beta}\;\;\texttt{(\lm{} (b) b)}
\end{array}
```

Using call by name or normal order we apply functions before reducing their arguments, so the $\Omega$ subterm is never reduced, avoiding non-termination. If we used call by value or applicative order, the $\Omega$ term would be reduced ad infinitum causing evaluation to continue without reaching a value or normal form.

Racket is a call-by-value language, so it insists on reducing the Ω argument first and never returns—watch it hit the sandbox's timeout:

``` racket run mode=expr label="CBV diverges here; CBN would not"
((λ (a) (λ (b) b))
 ((λ (u) (u u)) (λ (u) (u u))))
```

## Evaluation contexts

We can formalize a grammar for evaluation contexts ($\mathcal{E}$) to clearly specify the order in which reducible sub-expressions are simplified. Formally, an evaluation context is an expression with a "hole" ($\square$) located in the position of the current redex. Using a grammar to constrain these contexts, we can enforce different evaluation strategies; for example, CBV:

```latex
\begin{array}{rcll}
\mathcal{E} & ::= & \square & [\text{a hole---where the redex goes}]\\[3pt]
 & \vert & \texttt{(e $\mathcal{E}$)} & [\text{arg.\ position may be evaluated first}]\\[3pt]
 & \vert & \texttt{($\mathcal{E}$ v)} & [\text{func.\ position may be evaluated next}]\\[8pt]
r & ::= & \texttt{(v v)} & [\text{a $\beta$ redex is a \lm{} applied on a \lm{}}]\\[8pt]
v & ::= & \texttt{(\lm{} (x) e)} & [\text{values are \lm{}s}]
\end{array}
```

Note how this grammar enforces CBV evaluation order by ensuring that redexes are values applied on values, rightmost evaluation, and that a context may not contain a lambda. How might CBN, AOE, and NOE be formalized in this way?

When evaluating a term using a context and redex, we first (deterministically) split the term $t$ into its context and redex, $t = \mathcal{E}[r]$, β reduce the redex, $r \rightarrow_{\beta} e$, and then substitute the simplified redex ($e$) back into its context before taking another step of evaluation: $t' = \mathcal{E}[e]$. Then, if the subsequent term, $t'$, can be split into another context and redex, evaluation continues, and if not, we can terminate with a final value. For example, consider the expression:

```latex
t_0 \;=\; \texttt{((\lm{} (a) a) ((\lm{} (b) b) (\lm{} (c) c)))}
```

To take one step, we split $t_0$ into context and redex:

```latex
\begin{array}{lcl}
t_0 & = & \mathcal{E}_0[r_0]\\[3pt]
\mathcal{E}_0 & = & \texttt{((\lm{} (a) a) $\square$)}\\[3pt]
r_0 & = & \texttt{((\lm{} (b) b) (\lm{} (c) c))}
\end{array}
```

After applying β-reduction on $r_0$, we get $e_0$:

```latex
r_0 \;\rightarrow_{\beta}\; \underbrace{\texttt{(\lm{} (c) c)}}_{e_0}
```

This is then plugged back into the context:

```latex
t_1 \;=\; \mathcal{E}_0[e_0] \;=\; \texttt{((\lm{} (a) a) (\lm{} (c) c))}
```

We can then take a final step of evaluation. Now our context will be empty and the entire term is a redex:

```latex
\begin{array}{lcl}
t_1 & = & \mathcal{E}_1[r_1]\\[3pt]
\mathcal{E}_1 & = & \square\\[3pt]
r_1 & = & \texttt{((\lm{} (a) a) (\lm{} (c) c))}\\[8pt]
r_1 & \rightarrow_{\beta} & \underbrace{\texttt{(\lm{} (c) c)}}_{e_1}\\[10pt]
t_2 & = & \mathcal{E}_1[e_1] \;=\; \texttt{(\lm{} (c) c)}
\end{array}
```

Finally, since this term ($t_2$) is a value and cannot be split into context and redex, evaluation terminates. Racket performs exactly this CBV evaluation (the printed procedure is the identity function $t_2$):

``` racket run mode=expr
((λ (a) a) ((λ (b) b) (λ (c) c)))
```
