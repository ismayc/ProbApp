# The UI should build into a bslib page that contains every control and card.

test_that("ui is a bslib page", {
  expect_s3_class(ui, "bslib_page")
})

test_that("ui html exposes all control input ids", {
  html <- as.character(ui)
  for (id in c("distType", "outType", "pBG", "p", "numBinTrials", "a", "b",
               "numEvents", "numTrials", "favBalls", "pNeg", "numSuccesses",
               "lambda", "beta", "alpha", "betaG", "alphaG", "df", "betaE",
               "df1", "df2", "normMean", "normVar", "theta1", "theta2",
               "weibShape", "weibScale", "lnMeanlog", "lnSdlog",
               "cauchyLoc", "cauchyScale", "logisLoc", "logisScale",
               "paretoScale", "paretoShape", "laplaceLoc", "laplaceScale",
               "customExpr", "customLo", "customHi",
               "xFixedPC", "quantile", "x1", "x2", "xFixedL", "xFixedU")) {
    expect_match(html, id, fixed = TRUE, info = id)
  }
})

test_that("ui html exposes the server-rendered output slots and cards", {
  html <- as.character(ui)
  for (out in c("distName", "percentileUI", "probTypeSelect", "formulas",
                "distribPlot", "meanCalc", "varCalc", "percentCalc",
                "distribCalc", "probCalc")) {
    expect_match(html, out, fixed = TRUE, info = out)
  }
  expect_match(html, "Formula", fixed = TRUE)
  expect_match(html, "Plot", fixed = TRUE)
  expect_match(html, "Result", fixed = TRUE)
})

test_that("ui includes a dark-mode toggle", {
  html <- as.character(ui)
  expect_match(html, "dark-mode", fixed = TRUE)
})
