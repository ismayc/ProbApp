# Shared setup for the test suite.
# Loads the app's source so that the plotting helpers, the formula objects and
# the named server function (appServer) are available to every test file.
# Sourcing server.R also sources functions.R and formulas.R (via relative
# paths), so the working directory is set to the project root first.

library(shiny)
library(ggplot2)
library(bslib)

app_root <- normalizePath(file.path("..", ".."))
setwd(app_root)

source("server.R")                       # defines appServer + plotting helpers + formulas
ui_factory <- source("ui.R", local = new.env())$value  # ui.R now returns function(request)
ui <- ui_factory(list())                 # build the page (request unused for the static UI)

# A full set of parameter inputs (one per numericInput in the UI). Tests set
# this superset for every distribution so that any input referenced by a given
# output branch is always populated; only the inputs relevant to the selected
# distribution actually affect its result.
base_params <- list(
  pBG = 0.5, p = 0.5, numBinTrials = 15, a = 1, b = 6,
  numEvents = 10, numTrials = 5, favBalls = 3, pNeg = 0.5,
  numSuccesses = 2, lambda = 4,
  alpha = 2, beta = 5, alphaG = 3, betaG = 6, betaE = 5,
  df = 10, df1 = 5, df2 = 10,
  normMean = 0, normVar = 1, theta1 = 0, theta2 = 5,
  weibShape = 2, weibScale = 3, lnMeanlog = 0, lnSdlog = 1,
  cauchyLoc = 0, cauchyScale = 1, logisLoc = 0, logisScale = 2,
  paretoScale = 1, paretoShape = 3, laplaceLoc = 0, laplaceScale = 1,
  customExpr = "exp(-x)", customLo = 0, customHi = 5, customMasses = "", customMode = "expr"
)

# Per-distribution x / value inputs chosen to sit inside each support.
dist_configs <- list(
  bern  = list(type = "Discrete",   x = list(xFixedPC = 1,  xFixedL = 0,  xFixedU = 1,  x1 = 0,   x2 = 1,   quantile = 0.5)),
  bin   = list(type = "Discrete",   x = list(xFixedPC = 3,  xFixedL = 3,  xFixedU = 10, x1 = 2,   x2 = 6,   quantile = 0.5)),
  dunif = list(type = "Discrete",   x = list(xFixedPC = 3,  xFixedL = 3,  xFixedU = 4,  x1 = 2,   x2 = 5,   quantile = 0.5)),
  geom  = list(type = "Discrete",   x = list(xFixedPC = 2,  xFixedL = 2,  xFixedU = 3,  x1 = 1,   x2 = 4,   quantile = 0.5)),
  hgeom = list(type = "Discrete",   x = list(xFixedPC = 2,  xFixedL = 2,  xFixedU = 2,  x1 = 1,   x2 = 3,   quantile = 0.5)),
  nbin  = list(type = "Discrete",   x = list(xFixedPC = 3,  xFixedL = 3,  xFixedU = 5,  x1 = 2,   x2 = 6,   quantile = 0.5)),
  poi   = list(type = "Discrete",   x = list(xFixedPC = 3,  xFixedL = 3,  xFixedU = 6,  x1 = 2,   x2 = 6,   quantile = 0.5)),
  beta  = list(type = "Continuous", x = list(xFixedPC = 0.5, xFixedL = 0.5, xFixedU = 0.5, x1 = 0.2, x2 = 0.7, quantile = 0.5)),
  chisq = list(type = "Continuous", x = list(xFixedPC = 8,  xFixedL = 8,  xFixedU = 12, x1 = 5,   x2 = 15,  quantile = 0.5)),
  exp   = list(type = "Continuous", x = list(xFixedPC = 3,  xFixedL = 3,  xFixedU = 6,  x1 = 2,   x2 = 8,   quantile = 0.5)),
  f     = list(type = "Continuous", x = list(xFixedPC = 1,  xFixedL = 1,  xFixedU = 2,  x1 = 0.5, x2 = 3,   quantile = 0.5)),
  gam   = list(type = "Continuous", x = list(xFixedPC = 15, xFixedL = 15, xFixedU = 20, x1 = 10,  x2 = 25,  quantile = 0.5)),
  norm  = list(type = "Continuous", x = list(xFixedPC = 1,  xFixedL = 1,  xFixedU = 1,  x1 = -1,  x2 = 1,   quantile = 0.5)),
  t     = list(type = "Continuous", x = list(xFixedPC = 1,  xFixedL = 1,  xFixedU = 1,  x1 = -2,  x2 = 2,   quantile = 0.5)),
  unif  = list(type = "Continuous", x = list(xFixedPC = 2,  xFixedL = 2,  xFixedU = 3,  x1 = 1,   x2 = 4,   quantile = 0.5)),
  weib   = list(type = "Continuous", x = list(xFixedPC = 2.5, xFixedL = 2.5, xFixedU = 3, x1 = 1,   x2 = 4,  quantile = 0.5)),
  lnorm  = list(type = "Continuous", x = list(xFixedPC = 1.5, xFixedL = 1.5, xFixedU = 2, x1 = 0.5, x2 = 3,  quantile = 0.5)),
  cauchy = list(type = "Continuous", x = list(xFixedPC = 0.5, xFixedL = 0.5, xFixedU = 1, x1 = -1,  x2 = 1,  quantile = 0.5)),
  logis  = list(type = "Continuous", x = list(xFixedPC = 1,   xFixedL = 1,   xFixedU = 2, x1 = -2,  x2 = 2,  quantile = 0.5)),
  pareto = list(type = "Continuous", x = list(xFixedPC = 2,   xFixedL = 2,   xFixedU = 3, x1 = 1.5, x2 = 4,  quantile = 0.5)),
  laplace= list(type = "Continuous", x = list(xFixedPC = 0.5, xFixedL = 0.5, xFixedU = 1, x1 = -1,  x2 = 1,  quantile = 0.5)),
  custom = list(type = "CUSTOM",     x = list(xFixedPC = 1,   xFixedL = 1,   xFixedU = 1, x1 = 0.5, x2 = 2,  quantile = 0.5))
)

# Build the full input list for a given distribution.
inputs_for <- function(name, outType = "Probability",
                       percentile = "pdf", probType = "lowerTail") {
  cfg <- dist_configs[[name]]
  c(list(distType = cfg$type, distrib = name, outType = outType,
         percentile = percentile, probType = probType),
    base_params, cfg$x)
}
