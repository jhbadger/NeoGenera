# Mathematics and statistics (Phase 8)

All of Phase 8 is Lisp (`lisp/math/`) on top of three kernel changes: Karatsuba multiplication in `src/bignum.c`,
`make-string` accepting `:initial-element`, and the existing IEEE double support (the kernel produces real infinities
and NaNs; `/` by zero still signals, `%sdiv` gives the IEEE result where a library needs it).

## Layout

| File | Contents |
|---|---|
| `00-numeric.lisp` | hyperbolic functions, IEEE helpers, complex numbers (`cx` struct), `sqrt`/`log` wrappers returning complex results, polynomial roots, gamma/erf/incomplete gamma and beta |
| `10-random.lisp` | xoshiro128** generator seeded with splitmix64, uniform/normal/gamma/beta/binomial/Poisson sampling, shuffle, sample |
| `20-matrix.lisp` | row-major `matrix` struct, LU, Householder QR, least squares, Cholesky, Jacobi eigen, one-sided Jacobi SVD, shifted-QR eigenvalues |
| `30-stats.lisp` | descriptive statistics, eleven distribution families (d/p/q functions) |
| `40-tests.lisp` | `htest` results; t, F, chi-square, ANOVA, Kruskal-Wallis, Mann-Whitney, Wilcoxon, Fisher, binomial, correlation, KS tests; p-value adjustment; bootstrap and permutation |
| `50-models.lisp` | `lm` (QR) and `glm` (IRLS: gaussian, binomial, Poisson) with R-style summaries |
| `60-dataframe.lisp` | `dframe`, CSV/TSV reader with type inference, filter/sort/group/summarise/join |
| `70-plot.lisp` | braille canvas and block-character plots, SVG writers, `show-plot` |
| `80-symbolic.lisp` | simplifier, expansion, differentiation, integration, Taylor series, polynomial arithmetic |

## Design decisions

* **Complex numbers are a library type**, not part of the kernel tower. Making `+` and `*` generic over a third
  numeric type would slow every addition in the VM; `c+ c- c* c/` accept both kinds, and `sqrt` and `log` return
  complex results for negative reals.
* **Reproducible random streams.** A seed is expanded with splitmix64 into four 32-bit words, and the generator
  is xoshiro128**, which needs only 32-bit arithmetic (fixnums). `rand-uniform` uses 53 bits. The first outputs for seed 42 are
  pinned by a test, so any change to the stream is a visible, deliberate act.
* **Numerically careful kernels.** Variance uses a two-pass algorithm with a correction term; `lm` solves by Householder
  QR rather than the normal equations (the NIST Wampler 1 problem, which defeats the normal equations, is a test);
  the covariance of the coefficients comes from the R factor; p-values come from the regularised incomplete beta and gamma
  functions evaluated with continued fractions; `qnorm` is Acklam's rational approximation polished by two Halley steps and
  the other quantile functions bracket and bisect the cdf.
* **Exact rank tests.** `mann-whitney` and `wilcoxon-signed-rank` use exact null distributions for samples under 50
  without ties and the tie-corrected normal approximation otherwise, as R does.

## Validation

`tests/lisp/math-tests.lisp` checks against R output (Welch, paired and pooled t tests on the `sleep` data, one-way ANOVA
and Kruskal-Wallis on `PlantGrowth`, Fisher's tea-tasting table, the Hollander-Wolfe signed-rank example, the `women` regression,
Dobson's Poisson `glm`), NIST StRD Wampler 1, closed forms (binomial, Poisson, gamma, beta, hypergeometric), identities
(`pt(qt(p)) = p`, `A A^-1 = I`, `U S V^T = A`, `Q R = A`) and statistical properties of the random generators. Tolerances are given in each check:
about 1e-12 for special functions and 1e-6 for values quoted from R with seven significant digits.

## Deviations from the roadmap

* No complex type in the numeric tower and no BLAS/LAPACK binding; the inner loops are Lisp. Matrices are
  practical up to a few hundred rows.
* Toom-Cook is not implemented; Karatsuba switches on at 32 limbs.
* `eigenvalues` returns eigenvalues only (no eigenvectors for non-symmetric matrices).
* GLMs: gaussian, binomial and Poisson families with canonical links; no quasi-families, offsets or weights.
* The symbolic layer integrates polynomials, powers, `exp`, `sin`, `cos` and linear arguments only, and has no limits or equation systems.
* Exact distribution of `ks-test` and Shapiro-Wilk are not implemented; the KS p-value is asymptotic.
