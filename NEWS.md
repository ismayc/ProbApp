# NEWS

All notable changes to the **Calculator for Probability Distributions** are
documented here. The format follows [Keep a Changelog](https://keepachangelog.com/),
and versions use a `MAJOR.MINOR.PATCH` scheme. `0.1.0` is the original
decade-old application; each subsequent minor version is one milestone of the
2026 modernization. Newest first.

---

## 0.10.0 — Mobile-friendly + browser-only (shinylive) deployment (2026-06-04)

### Added
- **Second, server-free deployment**: the app is now also published as a fully
  static, in-browser build (R compiled to WebAssembly via webR/**shinylive**) on
  GitHub Pages at <https://ismayc.github.io/ProbApp/>. It runs entirely
  client-side — no server — and is built from the same source by a new GitHub
  Actions workflow on every push (the browser tab is titled the same as the app).
  The existing shinyapps.io server deployment continues alongside it.
- **Loading overlay**: a full-screen spinner ("Loading calculator…") shown on
  startup and faded out once the app finishes its first render, in light or dark
  to match the browser. Especially helpful for the heavier first boot of the
  browser-only build.

### Changed
- **Mobile-friendly layout** on phones: the header wraps its controls instead of
  overflowing, the custom-distribution card grows to fit instead of clipping, the
  plot is responsive, the Plotly toolbar is hidden where it overlapped the title,
  long plot titles are scaled to fit, and crowded discrete x-axis labels are
  thinned (every other label kept).
- The "Inter" font now loads from the Google Fonts CDN — required for the
  browser-only build and visually identical in the server build.
- **"Share link" appears in the server build only**: under webR the app runs in
  an iframe where URL bookmarking can't reach the address bar, so the button (and
  bookmarking) are omitted from the browser-only build.

### Fixed
- Silenced harmless startup console noise: the package "Attaching / object is
  masked" notices and the deferred Plotly "event … not registered" warnings (the
  test suite now reports **zero** warnings).

### Verified
- Both deployments confirmed live; the browser-only build exercised in a real
  browser (webR boots and the full app — sidebar, Plotly plot, MathJax — renders
  with no errors). Suite green at 1,095 tests / 100% coverage, now 0 warnings.

## 0.9.0 — Custom as its own type: mixed distributions + LaTeX input (2026-06-03)

### Added
- **Custom is now a top-level Distribution Type** (Discrete / Continuous /
  CUSTOM), since a custom distribution can be discrete, continuous, or a mix.
- **Mixed distributions**: a new "Point masses" field (`location:weight`,
  comma-separated) adds atoms alongside the continuous density. The whole
  distribution (curve + masses) is normalized to total probability 1, with a
  CDF that jumps at the masses, correct mean/variance/quantiles, `P(X = x_i)`
  shown at atoms, and the masses drawn as stems on the plot. A density of `0`
  with only masses gives a purely discrete custom distribution.
- **LaTeX input mode**: a toggle switches the density field between
  "R expression" and "LaTeX"; a common subset (`\frac`, `^{}`, `e^{}`,
  `\sqrt`, `\left/\right`, `\cdot`, `\pi`, `\ln`/trig, `|..|`, implicit
  multiplication) is converted to the R expression and fed through the same
  sandbox.
- **Auto-generated density LaTeX in the Formulas tab**: the custom density is
  shown as nicely formatted LaTeX with its numeric normalizing constant, a
  `\begin{cases}` breakdown for piecewise densities, and `P(X = x_i) = p_i`
  rows for point masses.

### Changed
- The custom-distribution inputs moved from the narrow sidebar into a roomy
  "Custom distribution" card above the plot. The density field's label tracks
  the input mode ("in x" vs "as LaTeX"), and full syntax / point-mass guidance
  moved into a "Syntax & point-mass help" modal so the card stays compact.
- The sidebar "Parameters:" heading is hidden for CUSTOM under Mean/Variance
  (the custom inputs live in the card, so there is nothing to head there).

### Verified
- Live bookmark round-trip: a CUSTOM (LaTeX + point-mass) configuration restores
  in full from a shared URL. Suite at 1,095 tests / 100% coverage; 630
  independent verification checks across the five audits.

## 0.8.0 — Piecewise custom densities (2026-06-03)

### Added
- The custom-distribution sandbox now permits the vectorized conditional
  `ifelse()` together with comparison (`< > <= >= == !=`) and logical
  (`& | !`) operators, so **piecewise densities** can be entered directly, e.g.
  `ifelse(x < 1, x, 2 - x)` (triangular) or `(x >= 0) * (x < 1) * 2` (indicator).
- Verification audit expanded with five piecewise families (triangular, step,
  trapezoid, indicator, logical-mask), each cross-checked against analytic and
  adaptive-quadrature references.
- A much broader custom-distribution test battery: dozens of valid densities
  (polynomials, trig, exponential/Gaussian, density-function mixtures, integrable
  singularities) plus invalid cases asserting the right errors for bad **support**
  (lo ≥ hi, non-numeric bounds), **range** (singularities/non-positive densities),
  and **sandbox** violations.

### Security
- Non-vectorized `&&` / `||` remain rejected (they would error on vector input);
  the two-layer sandbox (static whitelist walk + `emptyenv`-parented evaluation)
  is unchanged.

---

## 0.7.0 — Custom user-defined distribution (2026-06-03)

### Added
- A **"Custom (define your own)"** distribution. Enter a density `f(x)` in `x`,
  a finite support `[lo, hi]`, and an optional LaTeX string for display.
- Everything is computed numerically: the normalizing constant (`integrate()`),
  the CDF (fine grid-trapezoid), the mean and variance
  (`integrate(x·f)` / `integrate((x-μ)²·f)`), and quantiles (inverse-CDF
  interpolation). The expression need not be normalized.
- The custom density flows through every output type (PDF/quantile, CDF, all
  four probability types, mean, variance) and the plot, via the shared
  continuous engine.

### Security
- Expressions are evaluated in a **sandbox**, never with raw `eval`:
  1. the parsed expression tree is statically rejected unless every node is a
     whitelisted math function, the symbols `x`/`pi`, or a number; and
  2. evaluation happens in an environment whose parent is `emptyenv()`, so
     `system`, `file`, `eval`, `Sys.*`, etc. are unreachable.
  Public deployments must keep this sandbox.

---

## 0.6.0 — Shareable links (2026-06-02)

### Added
- **URL bookmarking**: a "Share link" button encodes the full configuration
  (distribution, parameters, output type) into the address bar so a specific
  example can be copied and shared. Restoring a link rebuilds the dynamic
  controls and all inputs.

---

## 0.5.0 — Six new distributions (2026-06-02)

### Added
- **Weibull, Log-Normal, Cauchy, Logistic, Pareto, and Laplace** distributions
  (21 built-in distributions total). Pareto and Laplace ship with custom
  `d`/`p`/`q` implementations; the others use base R.
- Undefined moments are reported honestly (e.g. the Cauchy mean/variance and the
  Pareto conditions on the shape parameter display "undefined" rather than a
  number).
- Each new family is defined once via a `contSpec()` registry that the generic
  continuous output handlers consume.

---

## 0.4.0 — Interactive plots (2026-06-02)

### Changed
- Plots migrated to **Plotly** (`ggplotly`) for richer interaction.

### Added
- **Hover tooltips** on both discrete bars and continuous curves, showing the
  PMF/density/CDF with the blackboard-bold `ℙ` matching the result text.
- **Click-to-set** a value and **drag-to-brush** a region directly on the plot,
  which updates the relevant inputs.
- **Dark-mode-aware plot theming** and a built-in **PNG download** of the plot.

---

## 0.3.0 — Polish & infrastructure (2026-06-02)

### Added
- **README** covering what the app does, how to run it, how to test and measure
  coverage, how to deploy, and the parameterization conventions.
- **Smarter number formatting**: very small probabilities render in scientific
  notation (e.g. `3.052 × 10⁻⁵`) instead of `0.0000`.
- **Reset to defaults** button and a **Help / About** dialog explaining usage
  and the parameterization notes that commonly trip people up.
- **Continuous integration** workflow running the full test suite.

### Changed
- Accessibility: the teal/slate palette and link colors verified against
  WCAG AA contrast on light and dark backgrounds.

---

## 0.2.0 — Modernized interface & verified correctness (2026-06-02)

### Changed
- Complete UI rebuild on **bslib (Bootstrap 5)**: a `page_sidebar` layout with
  card-based results, an "Inter" type / calm-teal "clean academic" theme, and a
  **light/dark mode toggle**.
- Result text rendering fixed so inline math (`X`) stays on the line and empty
  outputs no longer leave large vertical gaps.
- Dependency management moved from **packrat to renv**; the server adopts the
  modern named-function form (replacing the deprecated `shinyServer()`).
- Contact email updated to `chester.ismay@gmail.com`.

### Added
- **Comprehensive input validation** with clear inline errors: whole-number
  counts (n, N, m, r, a, b), probability of success strictly in (0, 1), positive
  scale/shape/rate parameters, and cross-constraints (e.g. `n ≤ N`, `a ≤ b`,
  `θ₁ < θ₂`).
- **Hover tooltips** on discrete bars showing each probability.
- A **100% test-coverage** suite (`testServer`-driven) plus standalone numeric,
  edge-case/Monte-Carlo, and functionality **verification audits**.

### Fixed
- Five statistical correctness bugs: Beta variance numerator (`αβ`, not `α+β`);
  Exponential PDF/CDF using the wrong scale input; Gamma CDF using the wrong
  shape input; Negative-Binomial "more extreme" missing the success offset.
- Two formula-display typos: the Normal PDF exponent and the Hypergeometric
  variance factor (`1 − m/N`).
- An output/input ID collision on `percentile` (the output slot was renamed).

---

## 0.1.0 — Baseline (original)

- The original calculator by Dr. Chester Ismay and Logan Soich: 15 probability
  distributions (7 discrete, 8 continuous) with formula pages and
  PDF/quantile, CDF, probability, mean, and variance outputs, packaged as a
  two-file `ui.R` / `server.R` Shiny app with packrat.
