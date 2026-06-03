# Edge-case, internal-self-consistency, and Monte-Carlo verification of the app.
# Complements the analytic multi-regime audit (verify_stats2.R).
suppressMessages({library(shiny); library(ggplot2); library(bslib); library(plotly)})
source("server.R")

strip <- function(x) paste(gsub("<[^>]+>", " ", as.character(x)), collapse = " ")
nums  <- function(s) as.numeric(regmatches(s, gregexpr("-?[0-9]+\\.?[0-9]*", s))[[1]])
# lastn understands both plain "0.0176" and scientific "3.487 \times 10^{-11}".
lastn <- function(s) {
  sci <- regmatches(s, gregexpr("(-?[0-9.]+)\\s*\\\\times\\s*10\\^\\{(-?[0-9]+)\\}", s, perl = TRUE))[[1]]
  if (length(sci)) {
    z <- sci[length(sci)]
    as.numeric(sub("^\\s*(-?[0-9.]+).*", "\\1", z)) *
      10^as.numeric(sub(".*\\{(-?[0-9]+)\\}.*", "\\1", z))
  } else { v <- nums(s); v[length(v)] }
}
valof <- function(s) as.numeric(sub(".*value of (-?[0-9.]+).*", "\\1", s))
P <- 0L; A <- 0L; Fails <- character(0)
cmp <- function(lbl, app, ref, tol = 2e-3) {
  A <<- A + 1L
  if (length(app) != 1 || length(ref) != 1) { Fails <<- c(Fails, sprintf("%-40s NO-VALUE (app len %d)", lbl, length(app))); return(invisible()) }
  ok <- isTRUE(is.finite(app) && is.finite(ref) && abs(app - ref) <= tol)
  if (ok) P <<- P + 1L
  else Fails <<- c(Fails, sprintf("%-40s app=%-11s ref=%-11s", lbl,
                   formatC(app, 5, format = "g"), formatC(ref, 5, format = "g")))
}
bp <- list(pBG=.5,p=.5,numBinTrials=15,a=1,b=6,numEvents=10,numTrials=5,favBalls=3,pNeg=.5,
  numSuccesses=2,lambda=4,alpha=2,beta=5,alphaG=3,betaG=6,betaE=5,df=10,df1=5,df2=10,
  normMean=0,normVar=1,theta1=0,theta2=5,
  xFixedPC=1)  # always present in the live app (default 1.0); distribCalc's outer guard needs it

cat("== (A) Degenerate ranges: between(x2<x1)=0, extreme(x2<x1)=1 ==\n")
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin"), bp))
  session$setInputs(outType="Probability", probType="between", x1=8, x2=3)
  cmp("bin between x2<x1 -> 0", lastn(strip(output$probCalc)), 0)
  session$setInputs(probType="extreme", x1=8, x2=3)
  cmp("bin extreme x2<x1 -> 1", lastn(strip(output$probCalc)), 1)
  do.call(session$setInputs, c(list(distType="Continuous", distrib="norm"), bp))
  session$setInputs(outType="Probability", probType="extreme", x1=1, x2=-1)
  cmp("norm extreme x2<x1 -> 1", lastn(strip(output$probCalc)), 1)
})

cat("== (B) Discrete self-consistency: P(X<=x) + P(X>=x+1) = 1 ==\n")
xint <- list(bern=0, bin=7, dunif=3, geom=2, hgeom=2, nbin=4, poi=4)
for (d in names(xint)) {
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Discrete", distrib=d), bp))
    session$setInputs(outType="Probability", probType="lowerTail", xFixedL=xint[[d]])
    lo <- lastn(strip(output$probCalc))
    session$setInputs(probType="upperTail", xFixedU=xint[[d]] + 1)
    up <- lastn(strip(output$probCalc))
    cmp(paste0(d, ": lower(x)+upper(x+1)=1"), lo + up, 1)
  })
}

cat("== (C) Continuous self-consistency: P(X<=x) + P(X>=x) = 1 ==\n")
xcont <- list(beta=.4, chisq=8, exp=3, f=1.2, gam=15, norm=0.7, t=0.5, unif=2)
for (d in names(xcont)) {
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Continuous", distrib=d), bp))
    session$setInputs(outType="Probability", probType="lowerTail", xFixedL=xcont[[d]])
    lo <- lastn(strip(output$probCalc))
    session$setInputs(probType="upperTail", xFixedU=xcont[[d]])
    up <- lastn(strip(output$probCalc))
    cmp(paste0(d, ": lower(x)+upper(x)=1"), lo + up, 1)
  })
}

cat("== (D) Quantile-inverse consistency: F(F^{-1}(q)) = q (continuous) ==\n")
for (d in c("beta","chisq","exp","f","gam","norm","t","unif")) {
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Continuous", distrib=d), bp))
    for (q in c(.1, .5, .9)) {
      session$setInputs(outType="PDF", percentile="quant", quantile=q)
      xq <- valof(strip(output$percentCalc))
      session$setInputs(outType="CDF", xFixedL=xq)
      cmp(sprintf("%s: F(F^-1(%.1f))", d, q), lastn(strip(output$distribCalc)), q, tol=1e-2)
    }
  })
}

cat("== (E) Monte-Carlo cross-check (independent method): mean, variance, CDF ==\n")
mcset <- list(
  bin  =list(t="Discrete",  rng=function(n) rbinom(n,15,.5),     x=7),
  poi  =list(t="Discrete",  rng=function(n) rpois(n,4),          x=4),
  geom =list(t="Discrete",  rng=function(n) rgeom(n,.5)+1,       x=3),   # +1: trials parameterization
  nbin =list(t="Discrete",  rng=function(n) rnbinom(n,2,.5)+2,   x=5),   # +r: trials parameterization
  hgeom=list(t="Discrete",  rng=function(n) rhyper(n,3,7,5),     x=2),
  dunif=list(t="Discrete",  rng=function(n) sample(1:6,n,TRUE),  x=3),
  bern =list(t="Discrete",  rng=function(n) rbinom(n,1,.5),      x=0),
  norm =list(t="Continuous",rng=function(n) rnorm(n,0,1),        x=1),
  exp  =list(t="Continuous",rng=function(n) rexp(n,1/5),         x=3),
  gam  =list(t="Continuous",rng=function(n) rgamma(n,3,scale=6), x=15),
  beta =list(t="Continuous",rng=function(n) rbeta(n,2,5),        x=.5),
  unif =list(t="Continuous",rng=function(n) runif(n,0,5),        x=2),
  chisq=list(t="Continuous",rng=function(n) rchisq(n,10),        x=8),
  t    =list(t="Continuous",rng=function(n) rt(n,10),            x=1),
  f    =list(t="Continuous",rng=function(n) rf(n,5,10),          x=1))
N <- 5e5
for (d in names(mcset)) {
  C <- mcset[[d]]; set.seed(2024); draws <- C$rng(N)
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType=C$t, distrib=d), bp))
    session$setInputs(outType="Mean")
    cmp(paste0(d, ": MC mean"), lastn(strip(output$meanCalc)), mean(draws), tol=0.05)
    session$setInputs(outType="Variance")
    cmp(paste0(d, ": MC variance"), lastn(strip(output$varCalc)), var(draws),
        tol=max(0.25, 0.08 * var(draws)))
    session$setInputs(outType="CDF", xFixedL=C$x)
    cmp(paste0(d, ": MC CDF(", C$x, ")"), lastn(strip(output$distribCalc)),
        mean(draws <= C$x), tol=0.01)
  })
}

cat(sprintf("\n==== EDGE/SELF-CONSISTENCY/MC AUDIT: %d attempted, %d passed, %d issues ====\n", A, P, length(Fails)))
if (length(Fails)) cat(paste0("  ", Fails, collapse="\n"), "\n")
