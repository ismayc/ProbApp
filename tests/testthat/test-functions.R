# Every plotting helper should build a valid plot without error.

# A plot is a "prob_plot": plotly.js traces plus a layout. Check its shape, that
# every trace has matching x / y lengths, and that it survives the light and
# dark finishing step and Shiny's JSON serialization.
builds <- function(p) {
  expect_s3_class(p, "prob_plot")
  expect_gt(length(p$data), 0)
  for (tr in p$data) expect_equal(length(tr$x), length(tr$y))
  for (dark in c(FALSE, TRUE)) {
    out <- finish_prob_plot(p, dark = dark)
    expect_named(out, c("data", "layout", "config"))
    expect_no_error(shiny:::toJSON(out))
  }
}

test_that("distribPlot builds for every discrete distribution", {
  builds(distribPlot(func = pbinom, range = 0:1, args = c(1, 0.5),
                     inputValue = 1, distribName = "Bernoulli"))
  builds(distribPlot(range = 0:15, args = c(15, 0.5), inputValue = 3,
                     distribName = "Binomial"))
  builds(distribPlot(func = dunifdisc, range = 1:6, args = c(1, 6),
                     inputValue = 3, distribName = "Discrete Uniform"))
  builds(distribPlot(func = dgeom, range = 1:20, args = c(0.5), inputValue = 2,
                     numArgs = 1, paramAdjust = 1, distribName = "Geometric"))
  builds(distribPlot(func = dhyper, range = 0:3, args = c(3, 7, 5),
                     inputValue = 2, numArgs = 3, distribName = "Hypergeometric"))
  builds(distribPlot(func = dnbinom, range = 2:20, args = c(2, 0.5),
                     inputValue = 3, paramAdjust = 2, distribName = "Negative Binomial"))
  builds(distribPlot(func = dpois, range = 0:12, args = c(4), inputValue = 3,
                     numArgs = 1, distribName = "Poisson"))
})

test_that("distribPlot guards against missing inputs", {
  expect_null(distribPlot(inputValue = NULL))
  expect_null(distribPlot(numArgs = 1, args = NULL, inputValue = 0))
})

test_that("CDF-tooltip branch builds and each bar carries its tooltip", {
  p <- distribPlot(func = pbinom, range = 0:5, args = c(5, 0.5), inputValue = 3,
                   plotType = "Cumulative", mainLabel = "Cumulative Distribution Function")
  builds(p)
  expect_match(p$data[[1]]$text[4], "ℙ(X ≤ 3) = 0.8125", fixed = TRUE)
  expect_equal(p$layout$yaxis$title$text, "<b>Cumulative Probability</b>")
  p <- distribPlot(range = 0:5, args = c(5, 0.5), inputValue = 3, distribName = "Binomial")
  expect_match(p$data[[1]]$text[4], "ℙ(X = 3) = 0.3125", fixed = TRUE)
  expect_equal(p$layout$title$text, "<b>Binomial Probability Mass Function</b>")
  expect_equal(p$layout$yaxis$title$text, "<b>Probability</b>")
})

test_that("distribPlot highlights only the bar at the input value", {
  p <- distribPlot(range = 0:5, args = c(5, 0.5), inputValue = 3, distribName = "Binomial")
  bars <- p$data[[1]]
  expect_equal(bars$type, "bar")
  expect_equal(as.character(bars$x), as.character(0:5))   # the bar's x is its value
  expect_equal(as.character(bars$marker$color),
               ifelse(0:5 == 3, prob_hl, prob_base))
  expect_equal(p$layout$xaxis$type, "category")
  expect_equal(p$layout$xaxis$tickangle, 0)
})

test_that("pmf_plot colors the bars in the region and handles a missing region", {
  p <- pmf_plot(0:5, dbinom(0:5, 5, 0.5), xlab = "x", ylab = "Probability",
                main = "Test", fill = 0:5 <= 2)
  builds(p)
  expect_equal(as.character(p$data[[1]]$marker$color), rep(c(prob_hl, prob_base), each = 3))
  # every bar in the region
  all_in <- pmf_plot(0:5, dbinom(0:5, 5, 0.5), fill = rep(TRUE, 6))
  expect_true(all(all_in$data[[1]]$marker$color == prob_hl))
  # fill = NULL default, and an NA in the region test: no highlight
  none <- pmf_plot(0:5, dbinom(0:5, 5, 0.5))
  builds(none)
  expect_true(all(none$data[[1]]$marker$color == prob_base))
  expect_equal(as.character(pmf_plot(0:1, c(0.5, 0.5), fill = c(NA, TRUE))$data[[1]]$marker$color),
               c(prob_base, prob_hl))
  # a region bound that has not arrived yet (zero-length fill): no plot
  expect_null(pmf_plot(0:5, dbinom(0:5, 5, 0.5), fill = logical(0)))
})

test_that("a shaded density plot has the region, the curve, and a padded window", {
  p <- normal_prob_area_plot(-1, 1, 0, 1)
  expect_length(p$data, 2)                                   # one region + the curve
  expect_equal(range(p$data[[1]]$x), c(-1, 1))
  expect_equal(p$data[[1]]$fill, "tozeroy")
  expect_equal(as.numeric(p$layout$xaxis$range), c(-4.4, 4.4))
  expect_equal(as.numeric(p$layout$xaxis$tickvals), -4:4)
  # the curve is sampled at 100 points, so its peak is just under dnorm(0)
  expect_equal(as.numeric(p$layout$yaxis$range), c(-0.05, 1.05) * dnorm(0), tolerance = 1e-3)
  tails <- normal_prob_area_plot(-1, 1, 0, 1, extreme = TRUE)
  expect_length(tails$data, 3)                               # two tails + the curve
  expect_equal(range(tails$data[[1]]$x), c(-4, -1))
  expect_equal(range(tails$data[[2]]$x), c(1, 4))
})

test_that("an infinite density becomes a gap and stays out of the y-range", {
  p <- chisq_prob_area_plot(0, 1, df = 1)                    # dchisq(0, 1) is Inf
  curve <- p$data[[length(p$data)]]
  expect_true(is.na(curve$y[1]))
  expect_true(all(is.finite(as.numeric(p$layout$yaxis$range))))
  builds(p)
  # nothing finite to scale by: leave the y-axis to plotly.js
  flat <- cont_area_plot(0, 1, function(x) rep(NaN, length(x)), c(0, 1), "No density")
  expect_null(flat$layout$yaxis$range)
})

test_that("point masses add stems and widen the window", {
  base <- cont_area_plot(0, 1, dexp, c(0, 5), "Custom Probability Density Function")
  expect_identical(add_point_masses(base, data.frame(loc = numeric(0), prob = numeric(0)), c(0, 5)), base)
  p <- add_point_masses(base, data.frame(loc = c(2, 7), prob = c(1.5, 0.2)), c(0, 5))
  builds(p)
  expect_length(p$data, length(base$data) + 2)               # stems + their heads
  expect_equal(as.numeric(p$data[[length(p$data)]]$x), c(2, 7))
  expect_gt(p$layout$xaxis$range[2], 7)                      # the mass at 7 is in view
  expect_equal(as.numeric(p$layout$yaxis$range), c(-0.05, 1.05) * 1.5)   # tallest stem fits
})

test_that("finish_prob_plot applies the light and dark colors and the config", {
  p <- distribPlot(range = 0:5, args = c(5, 0.5), inputValue = 3, distribName = "Binomial")
  light <- finish_prob_plot(p); dark <- finish_prob_plot(p, dark = TRUE)
  expect_equal(light$layout$paper_bgcolor, "white")
  expect_equal(dark$layout$paper_bgcolor, "#1f2937")
  expect_equal(dark$layout$title$font$color, "#f8fafc")
  expect_equal(dark$layout$xaxis$tickfont$color, "#cbd5e1")
  expect_equal(light$layout$dragmode, "select")
  expect_false(light$config$displaylogo)
  expect_true(light$config$responsive)
  expect_identical(light$data, dark$data)
})

test_that("continuous density-area plots build (incl. extreme tails)", {
  builds(beta_prob_area_plot(0, 0.5, 2, 5))
  builds(beta_prob_area_plot(0.2, 0.7, 2, 5, extreme = TRUE))
  builds(chisq_prob_area_plot(0, 8, 10))
  builds(chisq_prob_area_plot(5, 15, 10, extreme = TRUE))
  builds(exp_prob_area_plot(0, 5, scale = 5))
  builds(exp_prob_area_plot(2, 8, scale = 5, extreme = TRUE))
  builds(f_prob_area_plot(0, 3, 5, 10))
  builds(f_prob_area_plot(0.5, 3, 5, 10, extreme = TRUE))
  builds(gamma_prob_area_plot(0, 10, 3, 6))
  builds(gamma_prob_area_plot(10, 25, 3, 6, extreme = TRUE))
  builds(normal_prob_area_plot(-1, 1, 0, 1))
  builds(normal_prob_area_plot(-1, 1, 0, 1, extreme = TRUE))
  builds(t_prob_area_plot(-2, 2, 10))
  builds(t_prob_area_plot(-2, 2, 10, extreme = TRUE))
  builds(uniform_prob_area_plot(0, 3, 0, 5))
  builds(uniform_prob_area_plot(1, 4, 0, 5, extreme = TRUE))
})

test_that("continuous CDF-area plots build", {
  builds(beta_prob_CDF_plot(0, 0.5, 2, 5))
  builds(chisq_prob_CDF_plot(0, 8, 10))
  builds(exp_prob_CDF_plot(0, 5, scale = 5))
  builds(f_prob_CDF_plot(0, 3, 5, 10))
  builds(gamma_prob_CDF_plot(0, 10, 3, 6))
  builds(normal_prob_CDF_plot(-4, 1, 0, 1))
  builds(t_prob_CDF_plot(-3, 1, 10))
  builds(uniform_prob_CDF_plot(0, 3, 0, 5))
})

test_that("distribPlot guards the 2- and 3-argument paths", {
  expect_null(distribPlot(numArgs = 2, args = NULL, inputValue = 0))
  expect_null(distribPlot(numArgs = 3, args = NULL, inputValue = 0))
})

test_that("generic continuous helpers build for the new families", {
  builds(cont_area_plot(0, 1, function(x) dnorm(x), c(-3, 3), "Normal PDF"))
  builds(cont_area_plot(-1, 1, function(x) dnorm(x), c(-3, 3), "Normal PDF", extreme = TRUE))
  builds(cont_cdf_plot(0, 1, function(x) pnorm(x), c(-3, 3), "Normal CDF"))
  builds(cont_area_plot(1, 4, function(x) dweibull(x, 2, 3), c(0, 10), "Weibull PDF"))
  builds(cont_area_plot(1.5, 4, function(x) dpareto(x, 1, 3), c(1, 8), "Pareto PDF"))
  builds(cont_cdf_plot(-1, 1, function(x) plaplace(x, 0, 1), c(-5, 5), "Laplace CDF"))
})

test_that("generic continuous helpers guard against non-finite limits", {
  expect_null(cont_area_plot(0, 1, function(x) dnorm(x), c(NA, NA), "x"))
  expect_null(cont_cdf_plot(0, 1, function(x) pnorm(x), c(NA, NA), "x"))
})

test_that("continuous plot helpers guard against NULL limits / params", {
  expect_null(normal_prob_area_plot(-1, 1, 0, 1, limits = c(NULL, NULL)))
  expect_null(normal_prob_CDF_plot(-1, 1, 0, 1, limits = c(NULL, NULL)))
  expect_null(chisq_prob_area_plot(0, 8, 10, limits = c(NULL, NULL)))
  expect_null(t_prob_area_plot(-2, 2, 10, limits = c(NULL, NULL)))
  expect_null(uniform_prob_area_plot(0, 3, 0, 5, limits = c(NULL, NULL)))
  expect_null(gamma_prob_area_plot(0, 10, 3, 6, limits = c(NULL, NULL)))
  expect_null(exp_prob_area_plot(0, 5, scale = 5, limits = c(NULL, NULL)))
  expect_null(f_prob_area_plot(0, 3, 5, 10, limits = c(NULL, NULL)))
  expect_null(beta_prob_area_plot(0, 0.5, NULL, NULL))  # shape1/shape2 guard
  # CDF helpers (explicit NULL limits so the default isn't evaluated first)
  expect_null(beta_prob_CDF_plot(0, 0.5, 2, 5, limits = c(NULL, NULL)))
  expect_null(chisq_prob_CDF_plot(0, 8, 10, limits = c(NULL, NULL)))
  expect_null(exp_prob_CDF_plot(0, 5, scale = 5, limits = c(NULL, NULL)))
  expect_null(f_prob_CDF_plot(0, 3, 5, 10, limits = c(NULL, NULL)))
  expect_null(gamma_prob_CDF_plot(0, 10, 3, 6, limits = c(NULL, NULL)))
  expect_null(t_prob_CDF_plot(-3, 1, 10, limits = c(NULL, NULL)))
  expect_null(uniform_prob_CDF_plot(0, 3, 0, 5, limits = c(NULL, NULL)))
})
