# Functionality / reactive-routing verification.
suppressMessages({library(shiny); library(ggplot2); library(bslib); library(plotly)})
source("server.R")
strip <- function(x) paste(gsub("<[^>]+>", " ", as.character(x)), collapse = " ")
# Safe read: a req()-gated/empty output returns "" instead of raising.
S <- function(out) tryCatch({ v <- strip(out); if (length(v) == 0) "" else v }, error = function(e) "")
P <- 0L; Fails <- character(0)
ok <- function(lbl, cond) { if (isTRUE(cond)) P <<- P + 1L else Fails <<- c(Fails, lbl) }
bp <- list(pBG=.5,p=.5,numBinTrials=15,a=1,b=6,numEvents=10,numTrials=5,favBalls=3,pNeg=.5,
  numSuccesses=2,lambda=4,alpha=2,beta=5,alphaG=3,betaG=6,betaE=5,df=10,df1=5,df2=10,
  normMean=0,normVar=1,theta1=0,theta2=5,xFixedPC=3,xFixedL=3,xFixedU=10,x1=2,x2=6,quantile=.5)

# (1) Distribution list switches with distribution type
testServer(appServer, {
  session$setInputs(distType="Discrete", outType="Formulas")
  h <- S(output$distName)
  ok("Discrete list has all 7", all(sapply(c("Bernoulli","Binomial","Discrete Uniform","Geometric","Hypergeometric","Negative Binomial","Poisson"), grepl, h)))
  ok("Discrete list excludes Beta", !grepl("Beta", h))
  session$setInputs(distType="Continuous")
  h <- S(output$distName)
  ok("Continuous list has named dists", all(sapply(c("Beta","Chi-square","Exponential","Gamma","Normal","Uniform"), grepl, h)))
  ok("Continuous list excludes Poisson", !grepl("Poisson", h))
})

# (2) Output-type routing: only the relevant output renders
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", probType="lowerTail", percentile="pdf"), bp))
  session$setInputs(outType="Formulas")
  ok("Formulas: formulas shown",  grepl("Binomial", S(output$formulas)))
  ok("Formulas: meanCalc empty",  S(output$meanCalc) == "")
  session$setInputs(outType="Mean")
  ok("Mean: meanCalc shown",      grepl("7.5000", S(output$meanCalc)))
  ok("Mean: varCalc empty",       S(output$varCalc) == "")
  ok("Mean: probCalc empty",      S(output$probCalc) == "")
  session$setInputs(outType="Variance")
  ok("Variance: varCalc shown",   grepl("3.7500", S(output$varCalc)))
  ok("Variance: meanCalc empty",  S(output$meanCalc) == "")
  session$setInputs(outType="CDF")
  ok("CDF: distribCalc shown",    grepl("leq|F\\(", S(output$distribCalc)))
  session$setInputs(outType="Probability", probType="between")
  ok("Probability: probCalc shown", grepl("probability that", S(output$probCalc)))
  ok("Probability: distribCalc empty", S(output$distribCalc) == "")
})

# (3) Conditional control rendering: percentile only for PDF; probTypeSelect only for Probability
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", probType="lowerTail", percentile="pdf"), bp))
  session$setInputs(outType="PDF")
  ok("percentile shown for PDF",       S(output$percentileUI) != "")
  session$setInputs(outType="CDF")
  ok("percentile hidden for CDF",      S(output$percentileUI) == "")
  session$setInputs(outType="Probability")
  ok("probTypeSelect shown for Prob",  S(output$probTypeSelect) != "")
  session$setInputs(outType="Mean")
  ok("probTypeSelect hidden for Mean", S(output$probTypeSelect) == "")
})

# (4) Discrete bars carry the blackboard-P tooltip; (5) dark mode restyles plot
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="PDF", percentile="pdf", probType="lowerTail", dark_mode="light"), bp))
  j_light <- tryCatch(as.character(output$distribPlot), error = function(e) "")
  ok("tooltip uses blackboard P",  grepl("ℙ", j_light, fixed=TRUE) && grepl("0.1964", j_light))  # dbinom(7,15,.5) tooltip
  session$setInputs(dark_mode="dark")
  j_dark <- tryCatch(as.character(output$distribPlot), error = function(e) "")
  ok("plot renders (non-trivial SVG)", nchar(j_light) > 1000 && nchar(j_dark) > 1000)
  ok("dark mode re-styles the plot",   j_dark != j_light)   # overlay changes the render
})

# (6) Validation gates outputs and shows alert
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="Mean", percentile="pdf", probType="lowerTail"), bp))
  session$setInputs(numBinTrials=5.4)
  ok("invalid: alert shown",     grepl("whole number", S(output$inputError)))
  ok("invalid: meanCalc gated",  S(output$meanCalc) == "")
  session$setInputs(numBinTrials=15)
  ok("valid: alert cleared",     S(output$inputError) == "")
  ok("valid: meanCalc restored", grepl("7.5000", S(output$meanCalc)))
})

cat(sprintf("\n==== FUNCTIONALITY AUDIT: %d passed, %d FAILED ====\n", P, length(Fails)))
if (length(Fails)) cat(paste0("  FAIL: ", Fails, collapse="\n"), "\n")
