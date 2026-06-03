# Server logic exercised through shiny::testServer(appServer, ...).
# Covers every distribution across every output type, verifies a sample of the
# numeric results, and flips the edge-case branches in the calculation code.

# Strip HTML tags so rendered MathJax text can be matched.
strip <- function(x) paste(gsub("<[^>]+>", " ", as.character(x)), collapse = " ")

# A rendered plot should be non-NULL. Rendering some distributions emits a
# benign ggplot "removed rows ... outside the scale range" clipping warning
# (the app sets explicit scale_x_continuous() limits); suppress only that here.
# Errors are NOT suppressed, so a broken plot still fails the test.
plot_ok <- function(p) expect_false(is.null(suppressWarnings(p)))

# ---- The dynamic control renderers ----------------------------------------
test_that("distName, percentile and probTypeSelect render per selection", {
  testServer(appServer, {
    session$setInputs(distType = "Discrete", outType = "Formulas")
    expect_false(is.null(output$distName))
    session$setInputs(distType = "Continuous")
    expect_false(is.null(output$distName))

    session$setInputs(distrib = "norm", outType = "PDF")
    expect_false(is.null(output$percentileUI))      # Quantile/PDF radio
    session$setInputs(outType = "Probability")
    expect_false(is.null(output$probTypeSelect))  # probability-type select
  })
})

# ---- Every distribution x every output type --------------------------------
for (nm in names(dist_configs)) {
  test_that(paste0("all output types render for '", nm, "'"), {
    testServer(appServer, {
      do.call(session$setInputs, inputs_for(nm))

      session$setInputs(outType = "Formulas")
      expect_false(is.null(output$formulas))

      session$setInputs(outType = "PDF", percentile = "pdf")
      plot_ok(output$distribPlot)
      expect_false(is.null(output$distribCalc))

      session$setInputs(percentile = "quant")
      plot_ok(output$distribPlot)
      expect_false(is.null(output$percentCalc))

      session$setInputs(outType = "CDF")
      plot_ok(output$distribPlot)
      expect_false(is.null(output$distribCalc))

      for (pt in c("lowerTail", "between", "upperTail", "extreme")) {
        session$setInputs(outType = "Probability", probType = pt)
        plot_ok(output$distribPlot)
        expect_false(is.null(output$probCalc))
      }

      session$setInputs(outType = "Mean")
      expect_false(is.null(output$meanCalc))
      session$setInputs(outType = "Variance")
      expect_false(is.null(output$varCalc))
    })
  })
}

# ---- Numeric correctness (math must be unchanged by the revamp) ------------
test_that("Binomial numeric results match base R", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("bin"))
    session$setInputs(outType = "CDF", xFixedL = 3)
    expect_match(strip(output$distribCalc), sprintf("%.04f", pbinom(3, 15, 0.5)), fixed = TRUE)
    session$setInputs(outType = "PDF", percentile = "pdf", xFixedPC = 7)
    expect_match(strip(output$distribCalc), sprintf("%.04f", dbinom(7, 15, 0.5)), fixed = TRUE)
    session$setInputs(outType = "Mean")
    expect_match(strip(output$meanCalc), sprintf("%.04f", 15 * 0.5), fixed = TRUE)
    session$setInputs(outType = "Variance")
    expect_match(strip(output$varCalc), sprintf("%.04f", 15 * 0.5 * 0.5), fixed = TRUE)
  })
})

test_that("Normal numeric results match base R", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("norm"))
    session$setInputs(outType = "CDF", xFixedL = 1)
    expect_match(strip(output$distribCalc), sprintf("%.04f", pnorm(1, 0, 1)), fixed = TRUE)
    session$setInputs(outType = "Probability", probType = "between", x1 = -1, x2 = 1)
    expect_match(strip(output$probCalc),
                 sprintf("%.04f", pnorm(1, 0, 1) - pnorm(-1, 0, 1)), fixed = TRUE)
    session$setInputs(probType = "upperTail", xFixedU = 1)
    expect_match(strip(output$probCalc),
                 sprintf("%.04f", 1 - pnorm(1, 0, 1)), fixed = TRUE)
  })
})

test_that("Poisson and quantile results match base R", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("poi"))
    session$setInputs(outType = "PDF", percentile = "pdf", xFixedPC = 2)
    expect_match(strip(output$distribCalc), sprintf("%.04f", dpois(2, 4)), fixed = TRUE)
    session$setInputs(percentile = "quant", quantile = 0.5)
    expect_match(strip(output$percentCalc), sprintf("%.04f", qpois(0.5, 4)), fixed = TRUE)
  })
})

# ---- Edge-case branches in the probability calculations --------------------
test_that("between/extreme handle x2 <= x1 (discrete and continuous)", {
  testServer(appServer, {
    # Discrete between with x2 <= x1 -> probability 0
    do.call(session$setInputs, inputs_for("bin"))
    session$setInputs(outType = "Probability", probType = "between", x1 = 6, x2 = 2)
    expect_match(strip(output$probCalc), sprintf("%.04f", 0), fixed = TRUE)
    # Discrete extreme with x2 <= x1 -> probability 1
    session$setInputs(probType = "extreme", x1 = 6, x2 = 2)
    expect_match(strip(output$probCalc), sprintf("%.04f", 1), fixed = TRUE)
    # Continuous extreme with x2 <= x1 -> probability 1
    do.call(session$setInputs, inputs_for("norm"))
    session$setInputs(outType = "Probability", probType = "extreme", x1 = 1, x2 = -1)
    expect_match(strip(output$probCalc), sprintf("%.04f", 1), fixed = TRUE)
  })
})

test_that("quantile colour branch flips when the whole range is highlighted", {
  testServer(appServer, {
    # quantile = 1 makes qbinom == n, exercising the alternate fill ordering
    do.call(session$setInputs, inputs_for("bin"))
    session$setInputs(outType = "PDF", percentile = "quant", quantile = 1)
    plot_ok(output$distribPlot)
    # discrete uniform and poisson alternate branches too
    do.call(session$setInputs, inputs_for("dunif"))
    session$setInputs(outType = "PDF", percentile = "quant", quantile = 1)
    plot_ok(output$distribPlot)
    do.call(session$setInputs, inputs_for("poi"))
    session$setInputs(outType = "PDF", percentile = "quant", quantile = 1)
    plot_ok(output$distribPlot)
  })
})

# ---- Regression tests for statistical bugs fixed during the revamp --------
# Each of these produced a wrong number in the original app.
test_that("Exponential PDF and CDF use the scale parameter (betaE), not betaG", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("exp"))  # betaE = 5
    session$setInputs(outType = "PDF", percentile = "pdf", xFixedPC = 3)
    expect_match(strip(output$distribCalc), sprintf("%.04f", dexp(3, 1 / 5)), fixed = TRUE)
    session$setInputs(outType = "CDF", xFixedL = 3)
    expect_match(strip(output$distribCalc), sprintf("%.04f", pexp(3, 1 / 5)), fixed = TRUE)
  })
})

test_that("Gamma CDF uses its own shape parameter (alphaG)", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("gam"))  # alphaG = 3, betaG = 6
    session$setInputs(outType = "CDF", xFixedL = 15)
    expect_match(strip(output$distribCalc),
                 sprintf("%.04f", pgamma(15, shape = 3, scale = 6)), fixed = TRUE)
  })
})

test_that("Negative Binomial 'extreme' applies the success offset to x1", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("nbin"))  # r = 2, p = 0.5
    session$setInputs(outType = "Probability", probType = "extreme", x1 = 3, x2 = 6)
    ref <- pnbinom(3 - 2, 2, 0.5) + (1 - pnbinom(6 - 2 - 1, 2, 0.5))  # 0.6875
    expect_match(strip(output$probCalc), sprintf("%.04f", ref), fixed = TRUE)
  })
})

test_that("Beta variance numerator is alpha*beta (not alpha+beta)", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("beta"))  # alpha = 2, beta = 5
    session$setInputs(outType = "Variance")
    ref <- 2 * 5 / ((2 + 5)^2 * (2 + 5 + 1))        # 0.0255
    expect_match(strip(output$varCalc), sprintf("%.04f", ref), fixed = TRUE)
  })
})

test_that("Custom distribution matches its numeric spec across output types", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("custom"))   # exp(-x) on [0, 5]
    spec <- make_custom_spec("exp(-x)", 0, 5)
    session$setInputs(outType = "CDF", xFixedL = 1)
    expect_match(strip(output$distribCalc), sprintf("%.04f", spec$p(1)), fixed = TRUE)
    session$setInputs(outType = "PDF", percentile = "pdf", xFixedPC = 1)
    expect_match(strip(output$distribCalc), sprintf("%.04f", spec$d(1)), fixed = TRUE)
    session$setInputs(outType = "Mean")
    expect_match(strip(output$meanCalc), sprintf("%.04f", spec$mean), fixed = TRUE)
    session$setInputs(outType = "Variance")
    expect_match(strip(output$varCalc), sprintf("%.04f", spec$var), fixed = TRUE)
  })
})

test_that("Custom distribution surfaces a validation error for a unsafe/invalid expression", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("custom"))
    session$setInputs(customExpr = "system('rm -rf /')")
    expect_true(length(inputErrors()) >= 1)
    expect_match(inputErrors()[1], "not an allowed function")
    # An unsafe expression blanks the result: req() short-circuits, so reading
    # the output raises shiny's silent validation error rather than computing.
    session$setInputs(outType = "Mean")
    expect_error(output$meanCalc, class = "shiny.silent.error")
    # lo >= hi is also rejected.
    session$setInputs(customExpr = "exp(-x)", customLo = 5, customHi = 5)
    expect_match(inputErrors()[1], "lower bound")
  })
})

test_that("Custom distribution reports support / range / non-integrable errors to the user", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("custom"))
    # Support error: lower bound not below upper bound.
    session$setInputs(customExpr = "x^2", customLo = 2, customHi = 2)
    expect_match(inputErrors()[1], "lower bound .* less than")
    session$setInputs(customLo = 4, customHi = 1)
    expect_match(inputErrors()[1], "lower bound .* less than")
    # Range error: density not positive/finite over the support (singularity in range).
    session$setInputs(customLo = 0, customHi = 1, customExpr = "1/x^2")
    expect_match(inputErrors()[1], "positive, finite")
    # Non-positive density across the range.
    session$setInputs(customExpr = "-exp(x)", customLo = 0, customHi = 5)
    expect_match(inputErrors()[1], "positive, finite")
    # Disallowed helper.
    session$setInputs(customExpr = "sapply(x, sqrt)")
    expect_match(inputErrors()[1], "not an allowed")
    # Recovering with a valid expression clears the error and computes.
    session$setInputs(customExpr = "x^2", customLo = 0, customHi = 3, outType = "Mean")
    expect_length(inputErrors(), 0L)
    expect_match(strip(output$meanCalc), "2.2500", fixed = TRUE)
  })
})

test_that("applyBookmarkUrl (Share link handler) writes the query string without error", {
  testServer(appServer, {
    # The onBookmarked handler: pushes the encoded state into the address bar
    # and shows a confirmation. Returns the showNotification id (non-NULL).
    nid <- applyBookmarkUrl("?_inputs_&distrib=%22bin%22")
    expect_false(is.null(nid))
  })
})

test_that("probability lowerTail/upperTail full-coverage fill branches build", {
  testServer(appServer, {
    do.call(session$setInputs, inputs_for("bin"))
    # lowerTail at n -> CDF == 1 (alternate fill branch)
    session$setInputs(outType = "Probability", probType = "lowerTail", xFixedL = 15)
    plot_ok(output$distribPlot)
    # upperTail at 0 -> upper CDF == 1 (alternate fill branch)
    session$setInputs(probType = "upperTail", xFixedU = 0)
    plot_ok(output$distribPlot)
  })
})
