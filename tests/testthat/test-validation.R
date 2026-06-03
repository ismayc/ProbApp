# Input validation: invalid parameter entries should produce a clear error
# (inputErrors) and surface the alert via output$inputError.

test_that("integer-only parameters must be whole numbers", {
  testServer(appServer, {
    session$setInputs(distType = "Discrete", distrib = "bin", numBinTrials = 5.4, p = 0.5)
    expect_true(any(grepl("whole number", inputErrors())))
    expect_false(is.null(output$inputError))     # red alert is shown
    session$setInputs(numBinTrials = 15)
    expect_length(inputErrors(), 0)
    expect_null(output$inputError)               # alert cleared
  })
})

test_that("probability of success must be strictly between 0 and 1", {
  testServer(appServer, {
    session$setInputs(distType = "Discrete", distrib = "bin", numBinTrials = 15, p = 1.5)
    expect_true(any(grepl("between 0 and 1, exclusive", inputErrors())))
    session$setInputs(p = 0)                      # boundary is invalid (exclusive)
    expect_true(any(grepl("exclusive", inputErrors())))
    session$setInputs(p = 1)
    expect_true(any(grepl("exclusive", inputErrors())))
    session$setInputs(p = 0.3)
    expect_length(inputErrors(), 0)
  })
})

test_that("cross-parameter domain constraints are enforced", {
  testServer(appServer, {
    # hypergeometric: n and m cannot exceed N
    session$setInputs(distType = "Discrete", distrib = "hgeom",
                      numEvents = 10, numTrials = 12, favBalls = 3)
    expect_true(any(grepl("cannot exceed", inputErrors())))
    # discrete uniform: a cannot exceed b
    session$setInputs(distrib = "dunif", a = 6, b = 2)
    expect_true(any(grepl("cannot exceed", inputErrors())))
    # continuous uniform: theta2 must be > theta1
    session$setInputs(distType = "Continuous", distrib = "unif", theta1 = 5, theta2 = 2)
    expect_true(any(grepl("greater than", inputErrors())))
  })
})

test_that("scale/shape/df/variance parameters must be positive", {
  testServer(appServer, {
    session$setInputs(distType = "Continuous", distrib = "norm", normMean = 0, normVar = -1)
    expect_true(any(grepl("greater than 0", inputErrors())))
    session$setInputs(distrib = "gam", alphaG = 0, betaG = 6)
    expect_true(any(grepl("greater than 0", inputErrors())))
    session$setInputs(distrib = "t", df = -2)
    expect_true(any(grepl("greater than 0", inputErrors())))
  })
})

test_that("percentile must be a valid probability", {
  testServer(appServer, {
    session$setInputs(distType = "Continuous", distrib = "norm",
                      normMean = 0, normVar = 1, quantile = 1.5)
    expect_true(any(grepl("Percentile", inputErrors())))
    session$setInputs(quantile = 0.5)
    expect_length(inputErrors(), 0)
  })
})

test_that("valid default inputs produce no errors for any distribution", {
  for (nm in names(dist_configs)) {
    testServer(appServer, {
      do.call(session$setInputs, inputs_for(nm))
      expect_length(inputErrors(), 0)
    })
  }
})
