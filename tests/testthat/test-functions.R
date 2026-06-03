# Every plotting helper should build into a valid ggplot without error.

# Force a ggplot object through the full build pipeline so that any lazy
# evaluation error surfaces; return TRUE on success.
builds <- function(p) {
  expect_s3_class(p, "ggplot")
  # suppress the benign "Ignoring unknown aesthetics: text" (the `text` tooltip
  # aesthetic is used by plotly, not by ggplot's own renderer)
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
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

test_that("CDF-tooltip branch builds and the bar carries a tooltip aesthetic", {
  builds(distribPlot(func = pbinom, range = 0:5, args = c(5, 0.5), inputValue = 3,
                     plotType = "Cumulative", mainLabel = "Cumulative Distribution Function"))
  p <- distribPlot(range = 0:5, args = c(5, 0.5), inputValue = 3, distribName = "Binomial")
  expect_true("text" %in% names(p$mapping))   # tooltip text flows through to plotly
})

test_that("the qplot shim builds a bar chart and handles NULL fill", {
  builds(qplot(factor(0:5), dbinom(0:5, 5, 0.5), xlab = "x", ylab = "Probability",
               main = "Test\n", fill = 0:5 <= 2))
  builds(qplot(factor(0:5), dbinom(0:5, 5, 0.5)))  # fill = NULL default
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
