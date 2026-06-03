# Small distribution utilities and the shared plot-styling helpers.

test_that("discrete uniform density (dunifdisc) is correct", {
  expect_equal(dunifdisc(3, 1, 6), 1 / 6)
  expect_equal(dunifdisc(1, 1, 6), 1 / 6)
  expect_equal(dunifdisc(6, 1, 6), 1 / 6)
  expect_equal(dunifdisc(0, 1, 6), 0)   # below support
  expect_equal(dunifdisc(7, 1, 6), 0)   # above support
  expect_equal(dunifdisc(2.5, 1, 6), 0) # non-integer
})

test_that("discrete uniform CDF (punifdisc) is correct", {
  expect_equal(punifdisc(0, 1, 6), 0)
  expect_equal(punifdisc(3, 1, 6), 3 / 6)
  expect_equal(punifdisc(6, 1, 6), 1)
  expect_equal(punifdisc(10, 1, 6), 1)
})

test_that("discrete uniform quantile (qunifdisc) is correct", {
  expect_equal(qunifdisc(0.5, 1, 6), floor(0.5 * 6))
  expect_equal(qunifdisc(1.0, 1, 6), 6)
})

test_that("runifdisc returns values within support", {
  set.seed(1)
  draws <- runifdisc(50, 1, 6)
  expect_length(draws, 50)
  expect_true(all(draws >= 1 & draws <= 6))
})

test_that("Pareto d/p/q functions are correct", {
  expect_equal(dpareto(2, 1, 3), 3 * 1^3 / 2^4)     # 0.1875
  expect_equal(dpareto(0.5, 1, 3), 0)               # below the minimum x_m
  expect_equal(ppareto(2, 1, 3), 1 - (1/2)^3)       # 0.875
  expect_equal(ppareto(0.5, 1, 3), 0)
  expect_equal(qpareto(0.5, 1, 3), 1 / (0.5)^(1/3))
  expect_equal(ppareto(qpareto(0.7, 1, 3), 1, 3), 0.7)   # inverse consistency
})

test_that("Laplace d/p/q functions are correct", {
  expect_equal(dlaplace(0, 0, 1), 0.5)
  expect_equal(dlaplace(2, 0, 1), exp(-2) / 2)
  expect_equal(plaplace(0, 0, 1), 0.5)
  expect_equal(plaplace(-1, 0, 1), 0.5 * exp(-1))
  expect_equal(plaplace(1, 0, 1), 1 - 0.5 * exp(-1))
  expect_equal(qlaplace(0.5, 0, 1), 0)
  expect_equal(qlaplace(0.25, 0, 1), log(0.5))
  expect_equal(plaplace(qlaplace(0.8, 2, 3), 2, 3), 0.8)  # inverse consistency
})

# ---- Custom (user-defined) distribution: sandbox + numeric engine ----------
test_that("safe_pdf accepts whitelisted math and evaluates correctly", {
  f <- safe_pdf("exp(-x)")
  expect_equal(f(0), 1)
  expect_equal(f(1), exp(-1))
  expect_equal(safe_pdf("dnorm(x, 0, 1)")(0), dnorm(0))
  expect_equal(safe_pdf("x^2 + 2*x + 1")(3), 16)
  expect_equal(safe_pdf("pi * x")(2), 2 * pi)          # pi constant allowed
  expect_equal(safe_pdf("1")(7), 1)                    # constant
})

test_that("safe_pdf blocks anything outside the math whitelist", {
  expect_error(safe_pdf("system('ls')"), "not an allowed function")
  expect_error(safe_pdf("eval(1)"), "not an allowed function")
  expect_error(safe_pdf("file('x')"), "not an allowed function")
  expect_error(safe_pdf("Sys.getenv('HOME')"), "not an allowed function")
  expect_error(safe_pdf("x + y"), "not allowed")        # unknown symbol
  expect_error(safe_pdf("{ x }"), "not an allowed function")  # block
  expect_error(safe_pdf("x[1]"), "not an allowed function")   # indexing
  expect_error(safe_pdf("(function(z) z)(1)"), "simple math function calls")  # IIFE: head is a call
  expect_error(safe_pdf("'a string'"), "text strings are not allowed")
  expect_error(safe_pdf("y <- x"), "not an allowed function")        # assignment
  expect_error(safe_pdf(""), "enter an expression")
  expect_error(safe_pdf("   "), "enter an expression")
  expect_error(safe_pdf("1; 2"), "single expression")
  expect_error(safe_pdf("2 +"), "could not parse")
})

test_that("custom_check_node rejects disallowed calls and bare names", {
  expect_error(custom_check_node(quote(paste(x))), "not an allowed function")
  expect_error(custom_check_node(quote(foo)), "not allowed")
  expect_error(custom_check_node(quote(1i)), "unsupported token")   # complex literal
  expect_error(safe_pdf("1i * x"), "unsupported token")
  expect_silent(custom_check_node(quote(exp(x) + pi)))
  expect_silent(custom_check_node(42))                  # numeric literal node
  expect_silent(custom_check_node(TRUE))                # logical literal node
})

# ---- Custom: LaTeX -> R expression converter -------------------------------
test_that("latex_to_expr converts a common subset of LaTeX to R", {
  expect_equal(latex_to_expr("e^{-x}"), "exp(-x)")
  expect_equal(latex_to_expr("x^{2}"), "x^(2)")
  expect_equal(latex_to_expr("\\sqrt{x}"), "sqrt(x)")
  expect_equal(latex_to_expr("\\frac{1}{1+x^2}"), "((1)/(1+x^2))")
  expect_equal(latex_to_expr("\\left( x - 1 \\right)^{2}"), "(x-1)^(2)")
  expect_equal(latex_to_expr("2x"), "2*x")                       # implicit multiplication
  expect_equal(latex_to_expr("|x|"), "abs(x)")
  expect_equal(latex_to_expr("\\pi x"), "pi*x")
  expect_equal(latex_to_expr("a \\cdot b"), "a*b")
  expect_equal(latex_to_expr("\\ln(x)"), "log(x)")
  expect_equal(latex_to_expr("f(x) = e^{-x}"), "exp(-x)")        # strips a leading "f(x) ="
  # nested braces resolve innermost-first (multiple fixpoint iterations)
  expect_equal(latex_to_expr("\\frac{1}{\\sqrt{2\\pi}}"), "((1)/(sqrt(2*pi)))")
  expect_equal(latex_to_expr(NULL), "")
  expect_equal(latex_to_expr(""), "")
  # the converted expression feeds the existing pipeline
  expect_silent(make_custom_spec(latex_to_expr("e^{-x^2/2}"), -5, 5))
})

# ---- Custom: point masses + mixed distributions ----------------------------
test_that("parse_masses parses 'loc:wt' lists and rejects malformed entries", {
  expect_null(parse_masses(""))
  expect_null(parse_masses("   "))
  expect_null(parse_masses(NULL))
  m <- parse_masses("2:0.3, 5:0.7")
  expect_equal(m$loc, c(2, 5)); expect_equal(m$wt, c(0.3, 0.7))
  expect_equal(parse_masses("1:1 , , 3:2")$loc, c(1, 3))   # skips empty entries
  expect_null(parse_masses(", ,"))                          # all-empty entries -> NULL
  expect_error(parse_masses("2"), "location:weight")
  expect_error(parse_masses("x:1"), "must be numbers")
  expect_error(parse_masses("2:0"), "greater than 0")
  expect_error(parse_masses("2:-1"), "greater than 0")
  expect_error(parse_masses("2:1, 2:3"), "distinct")
})

test_that("make_custom_spec mixes point masses with a continuous density", {
  # Pure discrete: density 0, masses 1,2,3 with weights 1,1,2 -> probs .25,.25,.5
  d <- make_custom_spec("0", 0, 3, parse_masses("1:1, 2:1, 3:2"))
  expect_equal(d$mean, 2.25, tolerance = 1e-6)
  expect_equal(d$var, 0.6875, tolerance = 1e-6)
  expect_equal(d$p(2), 0.5, tolerance = 1e-9)        # CDF jump included
  expect_equal(d$p(2.5), 0.5, tolerance = 1e-9)
  expect_equal(d$q(0.5), 2)                          # smallest x with F >= 0.5
  expect_equal(d$q(0.6), 3)
  expect_equal(d$pmass(2), 0.25); expect_equal(d$pmass(3), 0.5); expect_equal(d$pmass(1.5), 0)
  expect_equal(d$contMass, 0)
  expect_equal(nrow(d$atoms), 3L)
  expect_equal(d$q(1.5), 3)                          # p beyond the CDF max -> largest support point

  # A non-integrable continuous part is treated as zero mass when masses carry it.
  big <- make_custom_spec("1/x^2", 0, 1, parse_masses("5:1"))
  expect_equal(big$contMass, 0)                      # divergent continuous integral -> 0
  expect_equal(big$pmass(5), 1, tolerance = 1e-9)

  # Mixed: mass at 0 (weight 1) + Uniform(0,1) density "1" -> P(X=0)=1/2
  m <- make_custom_spec("1", 0, 1, parse_masses("0:1"))
  expect_equal(m$pmass(0), 0.5, tolerance = 1e-9)
  expect_equal(m$d(0.5), 0.5, tolerance = 1e-6)      # continuous density scaled by total
  expect_equal(m$p(0), 0.5, tolerance = 1e-3)
  expect_equal(m$p(0.5), 0.75, tolerance = 1e-3)
  expect_equal(m$mean, 0.25, tolerance = 1e-3)
  expect_equal(m$var, 0.104167, tolerance = 1e-3)
  expect_equal(m$contMass, 0.5, tolerance = 1e-6)
  expect_equal(m$q(0.5), 0, tolerance = 1e-6)        # quantile lands on the atom
})

# ---- Custom: expression -> LaTeX converter ---------------------------------
test_that("expr_to_latex renders the whitelisted sublanguage", {
  expect_equal(expr_to_latex("x"), "x")
  expect_equal(expr_to_latex("pi"), "\\pi")
  expect_equal(expr_to_latex("2"), "2")
  expect_equal(expr_to_latex("x^2"), "x^{2}")
  expect_equal(expr_to_latex("(x - 1)^2"), "\\left(x - 1\\right)^{2}")   # base needs parens
  expect_equal(expr_to_latex("1/x"), "\\frac{1}{x}")
  expect_equal(expr_to_latex("2*x"), "2 x")                              # number*var -> juxtapose
  expect_equal(expr_to_latex("x*x"), "x \\cdot x")
  expect_equal(expr_to_latex("-x"), "-x")
  expect_equal(expr_to_latex("!x"), "\\lnot x")
  expect_equal(expr_to_latex("(x + 1)*2"), "\\left(x + 1\\right) \\cdot 2")  # grouping transparent
  expect_equal(expr_to_latex("sqrt(x)"), "\\sqrt{x}")
  expect_equal(expr_to_latex("exp(-x)"), "e^{-x}")
  expect_equal(expr_to_latex("abs(sin(x))"), "\\left|\\sin\\!\\left(x\\right)\\right|")
  expect_equal(expr_to_latex("log(1 + x)"), "\\ln\\!\\left(1 + x\\right)")
  expect_match(expr_to_latex("x > 0 & x < 1"), "\\text{and}", fixed = TRUE)
  expect_match(expr_to_latex("x <= 1 | x >= 2"), "\\text{or}", fixed = TRUE)
  expect_match(expr_to_latex("x >= 1"), "\\ge", fixed = TRUE)
  expect_equal(expr_to_latex("choose(5, x)"), "\\binom{5}{x}")
  expect_match(expr_to_latex("beta(x, 2)"), "B\\!\\left(x, 2\\right)", fixed = TRUE)
  expect_match(expr_to_latex("lbeta(x, 2)"), "\\ln B", fixed = TRUE)
  expect_match(expr_to_latex("dnorm(x, 0, 1)"), "\\operatorname{dnorm}", fixed = TRUE)
  expect_match(expr_to_latex("pmax(0, x)"), "\\operatorname{max}", fixed = TRUE)   # pmax -> max
  expect_match(expr_to_latex("ifelse(x < 1, x, 2)"), "\\begin{cases}", fixed = TRUE)  # nested
  expect_equal(expr_to_latex(quote(TRUE)), "1")                          # logical literal
  expect_equal(expr_to_latex(quote(1i)), "?")                            # unsupported node type
  # operands whose own precedence is queried (*, ^, !, and function/`/` nodes)
  expect_match(expr_to_latex("x*x*2"), "\\cdot", fixed = TRUE)           # chained * -> prec(*)
  expect_equal(expr_to_latex("x^2 + 1"), "x^{2} + 1")                    # ^ as operand -> prec(^)
  expect_match(expr_to_latex("(x > 1) & !(x < 0)"), "\\lnot", fixed = TRUE)  # ! as operand -> prec(!)
  expect_equal(expr_to_latex("sqrt(x) + 1"), "\\sqrt{x} + 1")            # function node -> default prec
})

# ---- Custom: full density LaTeX (constant + piecewise breakdown) ------------
test_that("custom_density_latex shows the normalizing constant and support", {
  s <- custom_density_latex("exp(-x)", 0, 5)
  expect_match(s, "f(x) = ", fixed = TRUE)
  expect_match(s, "e^{-x}", fixed = TRUE)
  expect_match(s, "\\le x \\le", fixed = TRUE)
  expect_match(s, "\\,", fixed = TRUE)                                   # a normalizing constant is shown
  # An already-normalized density omits the redundant "1 *" factor.
  tri <- custom_density_latex("ifelse(x < 1, x, 2 - x)", 0, 2)
  expect_match(tri, "\\begin{cases}", fixed = TRUE)
  expect_false(grepl("1\\,x", tri))                                      # no leading "1 *"
  expect_match(tri, "\\text{otherwise}", fixed = TRUE)
})

test_that("custom_density_latex breaks piecewise densities into cases and folds constants", {
  s <- custom_density_latex("ifelse(x < 1, 2, 1)", 0, 3)                 # C = 1/4
  expect_match(s, "\\begin{cases}", fixed = TRUE)
  expect_match(s, "0.5, &", fixed = TRUE)                                # 0.25 * 2 folded -> 0.5
  expect_match(s, "0.25, &", fixed = TRUE)                               # 0.25 * 1 folded -> 0.25
  s2 <- custom_density_latex("ifelse(x < 1, x, ifelse(x < 2, 1, 3 - x))", 0, 3)  # nested else
  expect_equal(lengths(regmatches(s2, gregexpr("&", s2))), 3L)           # three case rows
})

test_that("custom_density_latex renders point masses and mixed distributions", {
  # pure discrete: only P(X = x_i) = p_i rows, no continuous density
  d <- custom_density_latex("0", 0, 3, parse_masses("1:1, 2:1, 3:2"))
  expect_match(d, "\\mathbb{P}(X = 1) = 0.25", fixed = TRUE)
  expect_match(d, "\\mathbb{P}(X = 3) = 0.5", fixed = TRUE)
  expect_false(grepl("f(x)", d, fixed = TRUE))            # no continuous component
  # mixed: a mass plus the continuous density
  m <- custom_density_latex("exp(-x)", 0, 5, parse_masses("2:1"))
  expect_match(m, "\\mathbb{P}(X = 2)", fixed = TRUE)
  expect_match(m, "f(x) = ", fixed = TRUE)
  expect_match(m, "e^{-x}", fixed = TRUE)
  # mixed with a piecewise continuous part
  mp <- custom_density_latex("ifelse(x < 1, x, 2 - x)", 0, 2, parse_masses("0:1"))
  expect_match(mp, "\\mathbb{P}(X = 0)", fixed = TRUE)
  expect_match(mp, "\\begin{cases}", fixed = TRUE)
})

test_that("custom_density_latex rejects an invalid support", {
  expect_error(custom_density_latex("x", 5, 5), "invalid support")
  expect_error(custom_density_latex("x", NA_real_, 1), "invalid support")
  expect_error(custom_density_latex("1/x^2", 0, 1), "positive, finite")  # non-integrable
})

# ---- Custom: the Formulas-page wrapper -------------------------------------
test_that("customForm auto-formats, falls back to a LaTeX override, then to a prompt", {
  # auto-generated path
  f1 <- customForm("exp(-x)", "", 0, 5)
  expect_match(as.character(f1), "Custom Distribution", fixed = TRUE)
  expect_match(as.character(f1), "e^{-x}", fixed = TRUE)
  expect_match(as.character(f1), "normalizing constant", fixed = TRUE)
  # override path: invalid expression but a user LaTeX string is provided
  f2 <- customForm("1/x^2", "g(x)", 0, 1)
  expect_match(as.character(f2), "override", fixed = TRUE)
  expect_match(as.character(f2), "g(x)", fixed = TRUE)
  # override with missing bounds -> symbolic \ell / u
  f3 <- customForm("system('x')", "h(x)", NULL, NULL)
  expect_match(as.character(f3), "\\ell", fixed = TRUE)
  # nothing usable -> a prompt
  f4 <- customForm("system('x')", "", 0, 5)
  expect_match(as.character(f4), "Enter a valid density", fixed = TRUE)
  # point masses flow through to the rendered LaTeX
  f5 <- customForm("0", "", 0, 3, "2:0.3, 5:0.7")
  expect_match(as.character(f5), "\\mathbb{P}(X = 2)", fixed = TRUE)
})

test_that("custom_cumtrapz integrates and handles degenerate input", {
  x <- seq(0, 1, length.out = 5)
  expect_equal(custom_cumtrapz(x, rep(1, 5))[5], 1)     # area under y=1 over [0,1]
  expect_equal(custom_cumtrapz(x, x)[5], 0.5)           # area under y=x over [0,1]
  expect_equal(custom_cumtrapz(numeric(0), numeric(0)), numeric(0))
  expect_equal(custom_cumtrapz(1, 1), 0)                # single point -> 0
})

test_that("make_custom_spec normalizes and matches known truncated densities", {
  # Constant density on [0, 5] is Uniform(0, 5).
  s <- make_custom_spec("1", 0, 5)
  expect_equal(s$mean, 2.5, tolerance = 1e-6)
  expect_equal(s$var, 25 / 12, tolerance = 1e-4)
  expect_equal(s$d(2), 0.2, tolerance = 1e-6)
  expect_equal(s$p(2.5), 0.5, tolerance = 1e-4)
  expect_equal(s$q(0.5), 2.5, tolerance = 1e-2)
  expect_equal(s$d(-1), 0)                              # outside support
  expect_equal(s$d(6), 0)

  # dnorm(x,0,1) truncated to [-6, 6] is essentially N(0, 1).
  sn <- make_custom_spec("dnorm(x, 0, 1)", -6, 6)
  expect_equal(sn$mean, 0, tolerance = 1e-6)
  expect_equal(sn$var, 1, tolerance = 1e-3)
  expect_equal(sn$p(0), 0.5, tolerance = 1e-4)
  expect_equal(sn$q(0.975), qnorm(0.975), tolerance = 1e-2)

  # p and q are monotone inverses on a non-trivial density.
  se <- make_custom_spec("exp(-x)", 0, 5)
  expect_equal(se$p(se$q(0.3)), 0.3, tolerance = 1e-3)
  expect_true(se$p(1) > se$p(0.5))
})

test_that("safe_pdf supports piecewise via ifelse and comparison/logical operators", {
  expect_equal(safe_pdf("ifelse(x < 1, x, 2 - x)")(c(0.5, 1.5)), c(0.5, 0.5))
  expect_equal(safe_pdf("(x >= 0) * (x < 1) * 2")(c(-1, 0.5, 2)), c(0, 2, 0))  # raw, pre-normalization
  expect_equal(safe_pdf("(x > 0 & x < 2) * x")(c(-1, 1, 3)), c(0, 1, 0))
  expect_equal(safe_pdf("(x < 0 | x > 1) * 1")(c(-1, 0.5, 2)), c(1, 0, 1))
  expect_equal(safe_pdf("ifelse(x == 1, 5, 0)")(c(1, 2)), c(5, 0))
  # Non-vectorized && / || are rejected (they would error on vector x anyway).
  expect_error(safe_pdf("x > 0 && x < 1"), "not an allowed function")
  expect_error(safe_pdf("x > 0 || x < 1"), "not an allowed function")
})

test_that("make_custom_spec is exact for the triangular piecewise density", {
  # Tri(0, 2, 1): f(x) = ifelse(x<1, x, 2-x); already integrates to 1.
  s <- make_custom_spec("ifelse(x < 1, x, 2 - x)", 0, 2)
  expect_equal(s$mean, 1, tolerance = 1e-6)
  expect_equal(s$var, 1 / 6, tolerance = 1e-4)
  expect_equal(s$d(0.5), 0.5, tolerance = 1e-6)
  expect_equal(s$d(1), 1, tolerance = 1e-6)
  expect_equal(s$p(1), 0.5, tolerance = 1e-3)
  expect_equal(s$q(0.5), 1, tolerance = 1e-2)

  # Indicator (x>=0)*(x<1)*2 on [-1, 2] is Uniform(0, 1).
  u <- make_custom_spec("(x >= 0) * (x < 1) * 2", -1, 2)
  expect_equal(u$mean, 0.5, tolerance = 1e-4)
  expect_equal(u$var, 1 / 12, tolerance = 1e-4)
  expect_equal(u$d(1.5), 0)
})

test_that("make_custom_spec rejects non-integrable / non-positive densities", {
  expect_error(make_custom_spec("x - x", 0, 5), "positive, finite")   # integrates to 0
  expect_error(make_custom_spec("0", 0, 5), "positive, finite")
})

test_that("validate_custom returns NULL when valid and a message otherwise", {
  expect_null(validate_custom("exp(-x)", 0, 5))
  expect_match(validate_custom("1", 5, 5), "lower bound .* less than")
  expect_match(validate_custom("1", NULL, 5), "numeric lower and upper")
  expect_match(validate_custom("1", 0, NA_real_), "numeric lower and upper")
  expect_match(validate_custom("system('ls')", 0, 5), "not an allowed function")
  expect_match(validate_custom("0", 0, 5), "positive, finite")
})

# ---- Custom: a broad battery of VALID densities ----------------------------
test_that("make_custom_spec builds, normalizes, and gives monotone CDFs for many densities", {
  valid_cases <- list(
    list("x^2", 0, 3), list("x^3", 0, 2), list("(1-x)^2", 0, 1),
    list("x^2 * (1-x)^3", 0, 1), list("sqrt(x)", 0, 4), list("1/sqrt(x)", 0, 1),
    list("exp(-x^2)", -3, 3), list("exp(-abs(x))", -5, 5), list("x*exp(-x)", 0, 10),
    list("1 + sin(x)", 0, 2*pi), list("cos(x)^2", 0, pi), list("abs(sin(x))", 0, pi),
    list("dnorm(x, 0, 1)", -5, 5), list("dnorm(x,-2,1) + dnorm(x,2,1)", -6, 6),
    list("dgamma(x, 2, 1)", 0, 15), list("dbeta(x, 2, 5)", 0, 1), list("dexp(x, 1)", 0, 10),
    list("dweibull(x, 2, 1)", 0, 5), list("1/(1+x^2)", -10, 10), list("1/(1+x)", 0, 5),
    list("log(1+x)", 0, 5), list("gamma(x)", 1, 3), list("2", 0, 1),
    list("ifelse(x < 0, 0, x)", -2, 2), list("1/x", 1, 5), list("tan(x)", 0, 1.5),
    list("pmax(0, 1 - abs(x))", -2, 2)   # tent function via pmax
  )
  for (cse in valid_cases) {
    expr <- cse[[1]]; lo <- cse[[2]]; hi <- cse[[3]]
    info <- sprintf("%s on [%g, %g]", expr, lo, hi)
    s <- make_custom_spec(expr, lo, hi)
    expect_null(validate_custom(expr, lo, hi), info = info)
    # density integrates to 1
    expect_equal(integrate(s$d, lo, hi)$value, 1, tolerance = 5e-3, info = info)
    # CDF runs 0 -> 1 and is non-decreasing
    g <- seq(lo, hi, length.out = 150); cc <- s$p(g)
    expect_true(all(diff(cc) >= -1e-8), info = info)
    expect_equal(cc[1], 0, tolerance = 1e-6, info = info)
    expect_equal(cc[length(cc)], 1, tolerance = 1e-3, info = info)
    # p and q are inverses on interior probabilities
    pr <- c(0.2, 0.5, 0.8)
    expect_equal(s$p(s$q(pr)), pr, tolerance = 5e-3, info = info)
    # finite moments
    expect_true(is.finite(s$mean) && is.finite(s$var) && s$var >= 0, info = info)
  }
})

test_that("make_custom_spec moments match closed forms for known shapes", {
  expect_equal(make_custom_spec("x^2", 0, 3)$mean, 2.25, tolerance = 1e-4)
  expect_equal(make_custom_spec("x^2", 0, 3)$var, 0.3375, tolerance = 1e-3)
  expect_equal(make_custom_spec("(1-x)^2", 0, 1)$mean, 0.25, tolerance = 1e-4)       # Beta(1,3)
  expect_equal(make_custom_spec("(1-x)^2", 0, 1)$var, 3/80, tolerance = 1e-4)
  expect_equal(make_custom_spec("x^2 * (1-x)^3", 0, 1)$mean, 3/7, tolerance = 1e-3)  # Beta(3,4)
  expect_equal(make_custom_spec("x^2 * (1-x)^3", 0, 1)$var, 12/392, tolerance = 1e-3)
  expect_equal(make_custom_spec("dbeta(x, 2, 5)", 0, 1)$mean, 2/7, tolerance = 1e-3) # Beta(2,5)
  expect_equal(make_custom_spec("dbeta(x, 2, 5)", 0, 1)$var, 10/392, tolerance = 1e-3)
  expect_equal(make_custom_spec("2", 0, 1)$mean, 0.5, tolerance = 1e-4)              # U(0,1)
  expect_equal(make_custom_spec("2", 0, 1)$var, 1/12, tolerance = 1e-4)
  expect_equal(make_custom_spec("sqrt(x)", 0, 4)$mean, 2.4, tolerance = 1e-3)
  expect_equal(make_custom_spec("dnorm(x, 0, 1)", -6, 6)$var, 1, tolerance = 1e-3)
  expect_equal(make_custom_spec("exp(-x^2)", -4, 4)$mean, 0, tolerance = 1e-6)       # symmetric
})

# ---- Custom: INVALID densities with the appropriate error messages ---------
test_that("invalid support / range is rejected with a clear message", {
  # lo >= hi (validate checks bounds before building)
  expect_match(validate_custom("exp(-x)", 5, 5), "lower bound .* less than")
  expect_match(validate_custom("exp(-x)", 5, 2), "lower bound .* less than")
  expect_match(validate_custom("x^2", 3, 3), "lower bound .* less than")
  # non-numeric / missing bounds
  expect_match(validate_custom("1", NA_real_, 3), "numeric lower and upper")
  expect_match(validate_custom("1", 0, NULL), "numeric lower and upper")
  expect_match(validate_custom("1", Inf, 3), "numeric lower and upper")     # non-finite
})

test_that("non-integrable densities over the given support are rejected", {
  # singularity inside the range makes the integral diverge
  expect_error(make_custom_spec("1/x^2", 0, 1), "positive, finite")
  expect_error(make_custom_spec("1/x^2", -1, 1), "positive, finite")
  expect_match(validate_custom("1/(x^2)", 0, 5), "positive, finite")
  # same expression is FINE once the singularity is outside the support
  expect_null(validate_custom("1/x^2", 1, 5))
  expect_null(validate_custom("1/x", 1, 5))
})

test_that("non-positive densities over the range are rejected", {
  for (e in c("0", "x - x", "-1", "-exp(x)", "-(x^2)")) {
    expect_error(make_custom_spec(e, 0, 5), "positive, finite", info = e)
    expect_match(validate_custom(e, 0, 5), "positive, finite", info = e)
  }
  # log(x) is negative/NaN across [-1, 1], so it clamps to 0 everywhere -> rejected
  expect_match(validate_custom("log(x)", -1, 1), "positive, finite")
})

test_that("disallowed operators and helpers are blocked by the sandbox", {
  for (e in c("x %% 2", "x %*% 2", "y <- x", "x && 1", "x || 1",
              "sapply(x, sqrt)", "Reduce(sum, x)", "do.call(sum, x)",
              "get('x')", "environment()", "quote(x)", "x %in% 1")) {
    expect_error(safe_pdf(e), "not an allowed", info = e)
    expect_match(validate_custom(e, 0, 5), "not an allowed", info = e)
  }
})

test_that("prob_breaks chooses unit vs scaled spacing by range width", {
  expect_equal(prob_breaks(c(0, 10)), seq(0, 10, 1))            # narrow -> unit
  wide <- prob_breaks(c(0, 60))
  expect_true(length(wide) <= 16)                               # wide -> coarser
  expect_equal(wide, seq(0, 60, ceiling(60 / 15)))
})

test_that("theme_prob returns a ggplot theme", {
  th <- theme_prob()
  expect_s3_class(th, "theme")
})

test_that("palette constants are defined hex colours", {
  expect_match(prob_hl, "^#[0-9a-fA-F]{6}$")
  expect_match(prob_base, "^#[0-9a-fA-F]{6}$")
  expect_match(prob_line, "^#[0-9a-fA-F]{6}$")
})

test_that("fmtp uses fixed decimals normally and scientific for tiny values", {
  expect_equal(fmtp(0.5), "0.5000")
  expect_equal(fmtp(0.0176), "0.0176")
  expect_equal(fmtp(7.5), "7.5000")
  expect_equal(fmtp(0), "0.0000")
  expect_equal(fmtp(1e-4), "0.0001")                     # boundary: not yet scientific
  expect_equal(fmtp(3.48678e-11), "3.487 \\times 10^{-11}")
  expect_match(fmtp(0.5 * 1e-6), "\\\\times 10\\^\\{-7\\}")
  expect_equal(fmtp(NA_real_), "NA")                     # non-finite passthrough
})
