# Mathematics and Statistics

NeoGenera ships a numerical library written in Lisp: numerics, linear algebra, probability, statistics, models,
data frames, plotting and a small computer-algebra system. Everything lives in the one `COMMON-LISP` package, so the
functions are available at the Listener without any import. `(doc 'name)` shows the docstring of any of them.

## Numbers

Integers and exact ratios never overflow, and large products use Karatsuba multiplication, so `(* (expt 3 100000) (expt 7 50000))`
is fast. Floats are IEEE doubles with real infinities and NaNs: `+infinity+`, `-infinity+`, `+nan+`, and the predicates
[float-nan-p](fn:float-nan-p), [float-infinity-p](fn:float-infinity-p) and [finite-p](fn:finite-p).

Complex numbers are a library type. [complex](fn:complex) makes one and prints as `#C(re im)`. The operators `+ - * /`
stay real-only for speed; use [c+](fn:c+), [c-](fn:c-), [c*](fn:c*), [c/](fn:c/) and [cexp](fn:cexp), [clog](fn:clog), [csqrt](fn:csqrt),
[cexpt](fn:cexpt). [sqrt](fn:sqrt) and [log](fn:log) of a negative real return a complex result. [polynomial-roots](fn:polynomial-roots) finds all roots of a polynomial.

```
(sqrt -4)                        ; => #C(0.0 2.0)
(c* (complex 1 2) (complex 3 4)) ; => #C(-5 10)
(polynomial-roots '(1 -6 11 -6)) ; => (1.0 2.0 3.0)
```

Special functions: [log-gamma](fn:log-gamma), [gamma-function](fn:gamma-function), [erf](fn:erf), [erfc](fn:erfc), [beta-inc](fn:beta-inc), [gamma-p](fn:gamma-p), [choose](fn:choose),
and the hyperbolic functions.

## Random numbers

The generator is xoshiro128** seeded through splitmix64, so a seed gives the same stream on every platform.
[rng-seed](fn:rng-seed) reseeds the default generator `*rng*`; [make-rng](fn:make-rng) makes independent ones. Draw with [rand-uniform](fn:rand-uniform),
[rand-int](fn:rand-int), [rand-normal](fn:rand-normal), [rand-gamma](fn:rand-gamma), [rand-poisson](fn:rand-poisson), [rand-binomial](fn:rand-binomial) and friends; [shuffle](fn:shuffle) and
[sample](fn:sample) permute and subsample.

```
(rng-seed 2024)
(loop repeat 3 collect (rand-normal 10.0 2.0))
```

## Matrices and linear algebra

A matrix is a row-major array of floats. Build one with [mat](fn:mat) from a list of rows, read and write with [mref](fn:mref), and combine with
[mat*](fn:mat*), [mat+](fn:mat+), [matrix-transpose](fn:matrix-transpose). Solve and factor with [matrix-solve](fn:matrix-solve), [matrix-inverse](fn:matrix-inverse), [matrix-det](fn:matrix-det),
[lu-decompose](fn:lu-decompose), [qr-decompose](fn:qr-decompose) (Householder), [least-squares](fn:least-squares), [cholesky](fn:cholesky), [eigen-symmetric](fn:eigen-symmetric) (Jacobi), [eigenvalues](fn:eigenvalues)
(general, with complex pairs), [svd](fn:svd), [pseudo-inverse](fn:pseudo-inverse), [matrix-rank](fn:matrix-rank) and [condition-number](fn:condition-number).

```
(matrix-solve (mat '((4 3) (3 5))) #(1 2))
(eigenvalues (mat '((0 -1) (1 0))))   ; => (#C(0.0 -1.0) #C(0.0 1.0))
```

## Descriptive statistics and distributions

[mean](fn:mean), [variance](fn:variance), [std-dev](fn:std-dev), [median](fn:median), [quantile](fn:quantile), [skewness](fn:skewness), [kurtosis](fn:kurtosis), [correlation](fn:correlation), [spearman](fn:spearman),
[ranks](fn:ranks) and [describe-data](fn:describe-data) work on lists or vectors. Distributions follow R's naming: for each of normal, t, chi-square, F, gamma, beta,
binomial, Poisson, exponential, uniform and hypergeometric there is a density (`dnorm`), a cumulative function (`pnorm`, with `:lower nil` for the upper tail) and a quantile function (`qnorm`).
Examples: [pnorm](fn:pnorm), [qt](fn:qt), [pchisq](fn:pchisq), [pbinom](fn:pbinom).

## Hypothesis tests

Tests return an `htest` object that prints like R's output and has accessors `htest-statistic`, `htest-df`, `htest-p-value`,
`htest-estimate` and `htest-conf-int`. Available: [t-test](fn:t-test) (one sample, Welch, pooled, paired), [var-test](fn:var-test), [chisq-gof](fn:chisq-gof), [chisq-independence](fn:chisq-independence),
[anova-oneway](fn:anova-oneway), [kruskal-wallis](fn:kruskal-wallis), [mann-whitney](fn:mann-whitney) and [wilcoxon-signed-rank](fn:wilcoxon-signed-rank) (exact for small samples),
[fisher-exact](fn:fisher-exact), [binom-test](fn:binom-test), [cor-test](fn:cor-test) and [ks-test](fn:ks-test). [p-adjust](fn:p-adjust) corrects a list of p-values (Bonferroni, Holm, Hochberg, Benjamini-Hochberg,
Benjamini-Yekutieli). [bootstrap](fn:bootstrap), [bootstrap-ci](fn:bootstrap-ci) and [permutation-test](fn:permutation-test) resample.

```
(t-test '(0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0.0 2.0)
        :y '(1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4))
```

## Models

[lm](fn:lm) fits ordinary least squares by Householder QR and reports coefficients, standard errors, t and p values, R-squared and the F test.
[glm](fn:glm) fits Gaussian, binomial (logit) and Poisson (log) models by iteratively reweighted least squares. [polyfit](fn:polyfit), [lm-predict](fn:lm-predict),
[lm-confint](fn:lm-confint) and [glm-predict](fn:glm-predict) round it out. The results match R on the reference problems in the test suite
(the `women` regression, Dobson's Poisson example, NIST Wampler 1).

## Data frames

A data frame ([make-dframe](fn:make-dframe)) holds named columns. [read-csv](fn:read-csv) and [read-tsv](fn:read-tsv) infer integer, float and string columns and treat empty cells and `NA` as missing.
[df-filter](fn:df-filter), [df-select](fn:df-select), [df-sort](fn:df-sort), [df-add-column](fn:df-add-column), [df-group-by](fn:df-group-by), [df-summarize](fn:df-summarize), [df-join](fn:df-join) and [write-csv](fn:write-csv) cover everyday work.

```
(let ((d (read-csv "trial.csv")))
  (lm (df-col d "response") (df-col d "dose")))
```

## Plots

Text plots use Unicode braille and block characters: [plot-scatter](fn:plot-scatter), [plot-line](fn:plot-line), [plot-function](fn:plot-function), [plot-histogram](fn:plot-histogram), [plot-bars](fn:plot-bars),
[plot-boxplot](fn:plot-boxplot) and [plot-heatmap](fn:plot-heatmap) return strings; [show-plot](fn:show-plot) puts one in a window and `:histogram expr` plots a histogram from the Listener.
[svg-scatter](fn:svg-scatter), [svg-line](fn:svg-line), [svg-histogram](fn:svg-histogram) and [svg-heatmap](fn:svg-heatmap) produce SVG documents that [write-svg](fn:write-svg) saves.

**Graphs in pixels.** [graph-scatter](fn:graph-scatter), [graph-line](fn:graph-line), [graph-function](fn:graph-function),
[graph-histogram](fn:graph-histogram) and [graph-heatmap](fn:graph-heatmap) draw the same kinds of plot as real images (anti-aliased
lines and points, filled bars, colour scales) and return a graph. In the window system on a terminal that shows images (kitty or
Ghostty) a graph returned to the Listener, or given to [show-plot](fn:show-plot), appears as the picture with its title and ranges
around it; anywhere else it prints as the matching text plot, so code that makes graphs works on every terminal.
`(graph-function #'sin 0 10 :title "sine")` is a quick try. `graph-image` is the picture, an image you can also draw on.

## Symbolic mathematics

Expressions are ordinary Lisp lists such as `(+ (expt x 2) (* 3 x))`. [simplify](fn:simplify), [expand](fn:expand), [differentiate](fn:differentiate), [integrate](fn:integrate), [definite-integral](fn:definite-integral),
[taylor-series](fn:taylor-series), [substitute-expr](fn:substitute-expr), [evaluate-expr](fn:evaluate-expr) and [expr->string](fn:expr->string) form a small Macsyma-style layer; polynomials as coefficient lists have
[poly*](fn:poly*), [poly-divide](fn:poly-divide), [poly-gcd](fn:poly-gcd) and friends.

```
(differentiate '(* (sin x) (exp x)) 'x)
(integrate '(+ (expt x 2) 2) 'x)
```

See also [The User's Guide](topic:user-guide) and [The Documentation System](topic:documentation-system).
