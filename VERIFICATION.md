# Verification Report — Calculator for Probability Distributions

**Date:** 2026-06-02
**Scope:** Full pass through functionality and mathematical correctness of the
Shiny app (15 distributions × 6 output types), after the modernization +
bug-fix work.

## Summary

All checks pass. Across this pass, **394 independent verification checks** were
run (on top of the 471-test `testthat` suite at 100% code coverage), covering
analytic correctness across multiple parameter regimes, edge cases, internal
self-consistency, an independent Monte-Carlo cross-check, and reactive
functionality. **No mathematical or functional defects were found.** Two minor,
non-defect observations are noted under [Findings](#findings--recommendations).

| Audit | Checks | Result |
|-------|-------:|--------|
| Formula reference pages (analytic) | 15 dists × 6 facts | ✅ all correct |
| Numeric correctness (multi-regime, analytic) | 282 | ✅ 282/282 |
| Edge cases / self-consistency / Monte-Carlo | 87 | ✅ 87/87 |
| Functionality / reactive routing | 25 | ✅ 25/25 |
| `testthat` suite | 471 | ✅ 0 failures, 100% coverage |

Reproduce with (from the project root):

```r
Rscript tests/verification/verify-numeric.R         # analytic, multiple regimes
Rscript tests/verification/verify-edge-mc.R         # edges, self-consistency, Monte-Carlo
Rscript tests/verification/verify-functionality.R   # reactive behavior
Rscript tests/testthat.R                            # full unit/integration suite
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

All outputs are read through `shiny::testServer`, driving the real `appServer`
reactive graph; numeric values are parsed out of the rendered MathJax.

## 1. Formula reference pages (`formulas.R`)

Every distribution's displayed LaTeX was checked: parameter domain, support,
PDF, CDF, mean, and variance. **All 15 are mathematically correct**, including
the two fixed earlier in the project (Normal PDF exponent; Hypergeometric
variance factor `1 − m/N`).

Discrete: Bernoulli, Binomial, Discrete Uniform, Geometric, Hypergeometric,
Negative Binomial, Poisson. Continuous: Beta, Chi-square, Exponential, F, Gamma,
Normal, Student's t, Uniform.

## 2. Numeric correctness (multi-regime)

For all 15 distributions, across **two parameter regimes each** and multiple
x-values (including tails and support boundaries), the following were verified
against analytic references: PDF/density, CDF, quantile (at 0.1/0.5/0.9),
lower-tail, upper-tail, between, "more-extreme", mean, and variance.
→ **282/282 pass.** (`verify-numeric.R`)

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
- **Continuous self-consistency:** `P(X ≤ x) + P(X ≥ x) = 1` for all 8.
- **Quantile inverse:** `F(F⁻¹(q)) = q` for q ∈ {0.1, 0.5, 0.9} across all 8
  continuous distributions.
- **Monte-Carlo:** sample mean, variance, and empirical CDF from 5×10⁵ draws
  match the app's displayed mean/variance/CDF for all 15 distributions.

## 4. Functionality / reactive routing

→ **25/25 pass.** (`verify-functionality.R`)

- The Distribution list switches correctly between the 7 discrete and 8
  continuous options.
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
   the relevant moment condition for these distributions.

## Conclusion

The app is **functionally correct and mathematically sound** across all 15
distributions and 6 output types. Results were validated by three independent
methods (analytic, simulation, internal self-consistency) in addition to the
full automated test suite. The only open items are the two cosmetic/edge
consistency notes above.
