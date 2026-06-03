# Calculator for Probability Distributions

An interactive [Shiny](https://shiny.posit.co/) app for exploring 21 built-in
probability distributions — plus a **custom distribution you define yourself**.
For any distribution you can view its **formulas**, its **PDF/PMF** and **CDF**
(with shaded interactive plots), compute **probabilities** (lower-tail,
upper-tail, between, and "more extreme"), find **quantiles**, and read off the
**mean** and **variance** — each shown as rendered LaTeX.

Originally created by Dr. Chester Ismay and Logan Soich; modernized with
[bslib](https://rstudio.github.io/bslib/) (light/dark themes), interactive
[Plotly](https://plotly.com/r/) plots with hover tooltips and click/drag
interaction, input validation, shareable-link bookmarking, and a sandboxed
custom-distribution builder. See [`NEWS.md`](NEWS.md) for the full changelog.

## Distributions

- **Discrete:** Bernoulli, Binomial, Discrete Uniform, Geometric,
  Hypergeometric, Negative Binomial, Poisson
- **Continuous:** Beta, Chi-square, Exponential, F, Gamma, Normal, Student's t,
  Uniform, Weibull, Log-Normal, Cauchy, Logistic, Pareto, Laplace
- **Custom** (its own top-level distribution **type**): define your own density
  `f(x)` over a support `[lo, hi]`, optional **point masses** (`location:weight`),
  or both — a discrete, continuous, or **mixed** distribution, jointly normalized
  to total probability 1. Enter the density as an **R expression or LaTeX**
  (toggle). The expression is parsed in a sandbox (whitelisted math only — no
  file/network/`eval` access) and everything (normalization, CDF with jumps at
  masses, mean, variance, quantiles) is computed numerically. **Piecewise**
  densities are supported via `ifelse()` and comparison/logical operators, e.g.
  `ifelse(x < 1, x, 2 - x)`.

The Cauchy distribution's mean and variance are reported as *undefined*; Pareto
and Laplace are implemented with custom `d/p/q` functions (`functions.R`).

## Run it

From R, in the project root:

```r
shiny::runApp()
```

Dependencies are pinned with [renv](https://rstudio.github.io/renv/); the
project `.Rprofile` activates the library automatically. To install them the
first time:

```r
renv::restore()
```

## Parameterization conventions

A few choices differ from base R's defaults — worth knowing:

| Distribution | App parameterization | Notes |
|---|---|---|
| **Geometric** | "number of **trials**", support {1, 2, …} | PMF `p(1−p)^{x−1}`. Base R's `dgeom` counts *failures* (support {0, 1, …}); the app uses `dgeom(x − 1, p)`. |
| **Negative Binomial** | "number of **trials** to get *r* successes", support {r, r+1, …} | Base R's `dnbinom` counts *failures*; the app uses `dnbinom(x − r, r, p)`. |
| **Exponential** | **scale** β (mean = β) | `f(x) = e^{−x/β}/β`. Not the rate `λ`. |
| **Gamma** | **shape** α, **scale** β (mean = αβ) | Scale, not rate. |
| **Normal** | mean μ, **variance** σ² | The input is the variance, not the standard deviation. |
| **Beta / Gamma** | shapes entered as α and β | Beta uses shape1 = α, shape2 = β. |

Probability of success `p` is validated to be strictly in (0, 1); count
parameters (n, N, m, r, a, b) must be whole numbers; scale/shape/df/variance
must be positive; cross-constraints (n ≤ N, m ≤ N, a ≤ b, θ₂ > θ₁) are enforced.

## Develop & test

```r
Rscript tests/testthat.R     # full unit/integration suite (testthat)
Rscript tests/coverage.R     # code-coverage report (covr)
```

Independent correctness audits (analytic, Monte-Carlo, self-consistency,
functionality) live in `tests/verification/` and are summarized in
[`VERIFICATION.md`](VERIFICATION.md):

```r
Rscript tests/verification/verify-numeric.R
Rscript tests/verification/verify-edge-mc.R
Rscript tests/verification/verify-functionality.R
Rscript tests/verification/verify-new-distributions.R
Rscript tests/verification/verify-custom.R
```

## Deploy

The app deploys to [shinyapps.io](https://www.shinyapps.io/) (or any Shiny
Server / Posit Connect):

```r
rsconnect::deployApp()
```

**Continuous deployment.** Pushing to the default branch automatically deploys
to <https://ismay.shinyapps.io/ProbApp> via GitHub Actions
(`.github/workflows/deploy.yaml`). It requires two repository secrets —
`SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` (from your shinyapps.io account →
Tokens). The workflow can also be triggered manually from the Actions tab.

## Project layout

| File | Purpose |
|---|---|
| `ui.R` | bslib `page_sidebar` UI: controls in the sidebar, results in cards |
| `server.R` | reactive logic: validation, plots, and all distribution math |
| `functions.R` | plotting helpers, the plot theme/palette, `fmtp`, the custom-distribution sandbox/engine, small distribution utilities |
| `formulas.R` | the MathJax formula reference pages |
| `global.R` | enables URL bookmarking before the UI/server load |
| `tests/` | testthat suite, coverage harness, and verification audits |
| [`NEWS.md`](NEWS.md) | changelog of all releases |
