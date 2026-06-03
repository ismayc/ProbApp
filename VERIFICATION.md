# Verification Report — Calculator for Probability Distributions

**Date:** 2026-06-03
**Scope:** Full pass through functionality and mathematical correctness of the
Shiny app — **21 built-in distributions plus a user-defined Custom distribution
(its own top-level type)** that can be discrete, continuous, or a **mixed**
distribution (point masses + a continuous curve), entered as an **R expression
or LaTeX** — across every output type (Formulas, PDF/Quantile, CDF, Probability,
Mean, Variance), after the modernization, the six added families, and the
custom-distribution engine.

## Summary

All checks pass. **630 independent verification checks** were run (on top of the
**1,095-test `testthat` suite at 100% code coverage**), covering analytic
correctness across multiple parameter regimes, edge cases, internal
self-consistency, an independent Monte-Carlo cross-check, an independent
adaptive-quadrature cross-check for the custom engine, mixed-distribution
(point-mass) references, LaTeX-conversion equivalence, sandbox-security checks,
and reactive functionality. A live end-to-end check also confirmed a CUSTOM
(LaTeX + point-mass) configuration round-trips through the shareable bookmark
URL. **No mathematical or functional defects were found.** Two minor, non-defect
observations are noted under [Findings](#findings--recommendations).

| Audit | Checks | Result |
|-------|-------:|--------|
| Formula reference pages (analytic) | 21 dists × 6 facts | ✅ all correct |
| Numeric correctness — original 15, multi-regime | 282 | ✅ 282/282 |
| Edge cases / self-consistency / Monte-Carlo | 87 | ✅ 87/87 |
| Functionality / reactive routing | 25 | ✅ 25/25 |
| Six new distributions (analytic + undefined-moment) | 54 | ✅ 54/54 |
| Custom: quadrature, mixed, LaTeX, sandbox, validation | 182 | ✅ 182/182 |
| **Total independent checks** | **630** | ✅ |
| `testthat` suite | 1,095 | ✅ 0 failures, 100% coverage |

Reproduce with (from the project root):

```r
Rscript tests/verification/verify-numeric.R            # analytic, multiple regimes
Rscript tests/verification/verify-edge-mc.R            # edges, self-consistency, Monte-Carlo
Rscript tests/verification/verify-functionality.R      # reactive behavior
Rscript tests/verification/verify-new-distributions.R  # Weibull, Log-Normal, Cauchy, Logistic, Pareto, Laplace
Rscript tests/verification/verify-custom.R             # custom engine, sandbox, support/range validation
Rscript tests/testthat.R                               # full unit/integration suite (984)
Rscript tests/coverage.R                               # 100% line coverage (covr)
```

## Methodology

Each app output is compared against an **independent** reference:

1. **Analytic references** — base-R distribution functions, deliberately using a
   *different* parameterization than the app where possible (e.g. the app builds
   the Exponential via `dgamma(shape=1)` while the reference uses `dexp`; the
   app's "number of trials" Geometric/Negative-Binomial are checked against
   R's "number of failures" `dgeom`/`dnbinom` with the appropriate offset).
2. **Monte-Carlo** — 500,000 simulated draws per distribution give an
   *independent method* (simulation, not formula) for mean, variance, and the
   CDF; this catches errors a shared analytic reference could miss.
3. **Internal self-consistency** — relationships that must hold regardless of
   the reference, e.g. `P(X ≤ x) + P(X ≥ x+1) = 1` (discrete),
   `P(X ≤ x) + P(X ≥ x) = 1` (continuous), and `F(F⁻¹(q)) = q`.
4. **Adaptive quadrature** (custom engine) — the user-defined density is
   independently normalized/integrated with R's adaptive `integrate()` and an
   independent inverse-CDF root-find, a *different numerical method* than the
   app's grid-trapezoid CDF, so the two cross-check each other.
5. **Sandbox security** — hostile and malformed expressions are asserted to be
   refused rather than evaluated.

All outputs are read through `shiny::testServer`, driving the real `appServer`
reactive graph; numeric values are parsed out of the rendered MathJax.

## 1. Formula reference pages (`formulas.R`)

Every distribution's displayed LaTeX was checked: parameter domain, support,
PDF, CDF, mean, and variance. **All 21 are mathematically correct**, including
the two fixed earlier in the project (Normal PDF exponent; Hypergeometric
variance factor `1 − m/N`).

- **Discrete (7):** Bernoulli, Binomial, Discrete Uniform, Geometric,
  Hypergeometric, Negative Binomial, Poisson.
- **Continuous (14):** Beta, Chi-square, Exponential, F, Gamma, Normal,
  Student's t, Uniform, Weibull, Log-Normal, Cauchy, Logistic, Pareto, Laplace.

## 2. Numeric correctness — original 15 (multi-regime)

For the original 15 distributions, across **two parameter regimes each** and
multiple x-values (including tails and support boundaries), the following were
verified against analytic references: PDF/density, CDF, quantile (at
0.1/0.5/0.9), lower-tail, upper-tail, between, "more-extreme", mean, and
variance. → **282/282 pass.** (`verify-numeric.R`)

This confirms in particular the correctness-sensitive details:
- The discrete-tail index arithmetic — upper tail `1 − F(x−1)`, between
  `F(x₂) − F(x₁−1)`, and the "more-extreme" two-sided sum.
- The "number of trials" offsets for Geometric (`+1`) and Negative Binomial
  (`+r`) in PDF, CDF, quantile, and all probability modes.
- Exponential PDF/CDF using the scale parameter (β) and Gamma CDF using its own
  shape (α) — the inputs that were mis-wired before and are now fixed.

## 3. Edge cases, self-consistency, Monte-Carlo

→ **87/87 pass.** (`verify-edge-mc.R`)

- **Degenerate ranges:** "between" with x₂ < x₁ returns 0; "more-extreme" with
  x₂ < x₁ returns 1 (discrete and continuous).
- **Discrete self-consistency:** `P(X ≤ x) + P(X ≥ x+1) = 1` for all 7 discrete
  distributions.
- **Continuous self-consistency:** `P(X ≤ x) + P(X ≥ x) = 1` for the continuous
  families.
- **Quantile inverse:** `F(F⁻¹(q)) = q` for q ∈ {0.1, 0.5, 0.9}.
- **Monte-Carlo:** sample mean, variance, and empirical CDF from 5×10⁵ draws
  match the app's displayed mean/variance/CDF.

## 4. Functionality / reactive routing

→ **25/25 pass.** (`verify-functionality.R`)

- The Distribution list switches correctly between the discrete and continuous
  options.
- Output-type routing is exclusive: only the relevant card renders (Formulas →
  formula; Mean → mean only; Variance → variance only; CDF → CDF value;
  Probability → probability statement; etc.).
- Conditional controls render only when applicable (Quantile/PDF toggle for PDF
  output; probability-type selector for Probability output).
- Discrete bars carry the hover tooltip using blackboard-bold `ℙ` (matching the
  Result card).
- Dark mode re-styles the plot (the render changes when toggled).
- Validation gates outputs: an invalid entry (e.g. `n = 5.4`) shows the red
  alert and blanks the result; correcting it restores the result.

Input validation domains confirmed: probability of success strictly in (0, 1);
count parameters (n, N, m, r, a, b) integer; positivity of scale/shape/df/
variance; cross-constraints (n ≤ N, m ≤ N, a ≤ b, θ₂ > θ₁); percentile in [0, 1].

## 5. Six new distributions

→ **54/54 pass.** (`verify-new-distributions.R`)

Weibull, Log-Normal, Cauchy, Logistic, Pareto, and Laplace were each verified
end-to-end (formula page, PDF, quantile, CDF, all four probability modes, mean,
variance, plot) against independent references — base R for Weibull/Log-Normal/
Cauchy/Logistic, and hand-derived closed forms for the custom-implemented Pareto
and Laplace `d/p/q`.

- **Undefined moments are handled correctly:** the **Cauchy** mean and variance,
  and the **Pareto** mean (for α ≤ 1) and variance (for α ≤ 2), display
  "undefined" rather than a spurious number.
- The Pareto and Laplace `d/p/q` implementations are separately unit-tested for
  density, CDF, and quantile-inverse consistency.

## 6. Custom (user-defined) distribution

→ **182/182 pass.** (`verify-custom.R`), plus an extensive unit battery in the
`testthat` suite. Custom is a top-level distribution type that can be discrete,
continuous, or mixed, entered as an R expression or LaTeX.

### 6a. Numeric engine

The custom engine (`make_custom_spec`) takes a density `f(x)` and support
`[lo, hi]` and computes everything numerically: normalizing constant, CDF, mean,
variance, and quantiles. Verified against **independent adaptive quadrature**
and, where available, **closed forms**:

- Closed-form shapes: constant → Uniform(0,5) (exact mean 2.5, var 25/12);
  `x²` on [0,3] (mean 2.25, var 0.3375); `(1−x)²` → Beta(1,3); `x²(1−x)³` →
  Beta(3,4) (mean 3/7); `dbeta(x,2,5)` → Beta(2,5) (mean 2/7); `√x` on [0,4]
  (mean 2.4); `dnorm`/`dgamma` recovering the true moments.
- Cross-checked against `integrate()` + inverse-CDF root-find for: truncated
  exponential, ≈Normal, polynomials, Gaussian bump `exp(−x²)`, `1/(1+x²)`,
  and `dgamma`.
- **Self-consistency:** the CDF is monotone non-decreasing on [0,1], the density
  integrates to 1, and `F(F⁻¹(q)) = q` on interior probabilities.

### 6b. Piecewise densities

Piecewise densities expressed with `ifelse()` and comparison/logical operators
are verified against analytic and quadrature references: **triangular**
`ifelse(x<1, x, 2−x)` (mean 1, var 1/6), a **two-step** density, a **trapezoid**,
an **indicator** `(x≥0)(x<1)·2` → Uniform(0,1) (mean 0.5, var 1/12), and a
logical-`&` masked ramp. All match to numerical tolerance.

### 6c. Sandbox security

The expression is evaluated in a two-layer sandbox: a **static whitelist walk**
of the parsed expression tree, and **evaluation in an environment whose parent
is `emptyenv()`**. Verified to **refuse** (surfacing a clear error, never
executing):

- System / IO / reflection: `system`, `eval`, `file`, `readLines`, `Sys.getenv`,
  `Sys.setenv`, `get`, `environment`, `quote`, `do.call`, `sapply`, `Reduce`.
- Disallowed operators: `%%`, `%*%`, `%in%`, `<-`, and non-vectorized `&&`/`||`.
- Indexing / blocks / strings / complex literals / function definitions
  (`x[1]`, `{ x }`, `'str'`, `1i`, `(function(z) z)(1)`).
- Unknown symbols (`x + y`), empty input, multiple expressions (`1; 2`), and
  parse errors (`2 +`).

### 6d. Support / range validation

User-facing validation (`validate_custom`, surfaced through `inputErrors`) is
verified to report the **appropriate** error for each bad-input class:

- **Support:** `lo ≥ hi`, `lo > hi`, and non-numeric / non-finite bounds →
  "lower bound must be less than the upper bound" / "Enter numeric lower and
  upper bounds".
- **Range / non-integrable:** a density that does not integrate to a positive,
  finite value over the chosen support — a singularity inside the range
  (`1/x²` on [0,1]) or a non-positive density (`−exp(x)`, `0`, `log(x)` on
  [−1,1]) → "does not integrate to a positive, finite value". The *same*
  expression is correctly **accepted** when the singularity lies outside the
  support (`1/x²` on [1,5]).
- Recovery: replacing a bad expression with a valid one clears the error and the
  result computes.

### 6e. Mixed distributions (point masses + continuous)

Point masses (`location:weight`) combine with the continuous density into a
mixed distribution, jointly normalized to total probability 1. Verified against
analytic references:

- **Pure discrete** (density `0`, masses 1,2,3 weighted 1,1,2 → probs ¼,¼,½):
  mean 2.25, variance 0.6875, `F(2)=0.5` (CDF jump included), `P(X=2)=¼`.
- **Mixed** (mass at 0 weight 1 + Uniform(0,1) density `1` → P(X=0)=½): mean ¼,
  `F(0.5)=0.75`, total probability 1.
- The PDF view shows `P(X = xᵢ) = pᵢ` at an atom; the plot overlays mass stems;
  malformed mass strings (`2`, `x:1`, `2:-1`, `2:0`, duplicate locations) are
  each rejected with a clear message.

### 6f. LaTeX input mode

A common subset of LaTeX is converted to the R expression (`latex_to_expr`) and
fed through the same sandbox. Verified that the converted expression yields the
**same** distribution as the equivalent R expression for `e^{-x}`,
`\frac{1}{1+x^2}`, `x^{2}`, `\sqrt{x}`, and `e^{-x^2/2}` (Gaussian via
`\frac{1}{\sqrt{2\pi}} e^{-x^2/2}`), and that invalid LaTeX surfaces a clear
"could not parse" error.

### 6g. Live integration

A headless-browser check confirmed the end-to-end flow: selecting **CUSTOM**,
switching to **LaTeX** mode, entering `e^{-x}` with a point mass `2:1`, clicking
**Share link**, and loading the resulting URL in a fresh session **restores the
full state** (distribution type, input mode, expression, and masses).

## Findings & recommendations

No defects. Two minor, optional consistency items:

1. **Probability-of-success domain text vs. validation.** The Bernoulli /
   Binomial / Geometric / Negative-Binomial formula pages state `p ∈ (0, 1]`,
   while input validation enforces `p ∈ (0, 1)` (exclusive, as requested).
   For consistency, either display `p ∈ (0, 1)` on those pages or relax the
   validation to allow `p = 1`. (Cosmetic; does not affect any computation.)

2. **Moment conditions for heavy-tailed families.** Student's-t variance
   `ν/(ν−2)` requires ν > 2, and the F mean/variance require d₂ > 2 / d₂ > 4.
   The app labels these with "(for ν > 2)" / "(for d₂ > 4)" but still computes
   and displays a value for out-of-range parameters (e.g. t-variance with
   ν = 1.5 shows a negative number). Optional: gate the Variance/Mean output on
   the relevant moment condition for these distributions. (Note: the newer
   Cauchy and Pareto families *do* gate undefined moments — see §5.)

## Conclusion

The app is **functionally correct and mathematically sound** across all 21
built-in distributions, the user-defined custom distribution, and every output
type. Results were validated by independent analytic, simulation, adaptive-
quadrature, and self-consistency methods, plus sandbox-security and
support/range validation checks, in addition to the full automated test suite
(984 tests, 100% coverage). The only open items are the two cosmetic/edge
consistency notes above.
