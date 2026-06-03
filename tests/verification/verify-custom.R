# Verification audit for the Custom (user-defined) distribution.
# Cross-checks the app's numeric engine against INDEPENDENT references:
#   * closed-form truncated densities (Uniform, truncated Exponential, ~Normal,
#     a polynomial), and
#   * an independent normalize-by-integrate reference (adaptive quadrature),
#     which is a different method from the app's grid-trapezoid CDF/quantile.
# Also confirms the sandbox refuses unsafe / invalid expressions (the app
# surfaces a validation error and blanks the result rather than evaluating).
suppressMessages({library(shiny);library(ggplot2);library(bslib);library(plotly)})
source("server.R")
strip <- function(x) paste(gsub("<[^>]+>"," ",as.character(x)),collapse=" ")
S <- function(o) tryCatch({v<-strip(o); if(length(v)==0)"" else v}, error=function(e) paste("ERR:",conditionMessage(e)))
nums <- function(s){ v<-as.numeric(regmatches(s,gregexpr("-?[0-9]+\\.?[0-9]*",s))[[1]]); v[length(v)] }
qval <- function(s) as.numeric(sub(".*value of (-?[0-9.eE+-]+).*","\\1", s))
P<-0L; Fl<-0L
chk <- function(l,a,b,tol=3e-3){ ok<-isTRUE(is.finite(a)&&is.finite(b)&&abs(a-b)<=tol)
  if(ok)P<<-P+1L else {Fl<<-Fl+1L; cat(sprintf("  FAIL %s app=%s ref=%s\n",l,format(a),format(b)))} }
chkT<-function(l,cond){ if(isTRUE(cond))P<<-P+1L else {Fl<<-Fl+1L; cat(sprintf("  FAIL %s\n",l))} }

# Independent reference built by adaptive quadrature (NOT the app's method).
ref_spec <- function(f, lo, hi) {
  Z <- integrate(f, lo, hi)$value
  d <- function(x) f(x)/Z
  p <- function(x) vapply(x, function(xi) if(xi<=lo)0 else if(xi>=hi)1 else integrate(f,lo,xi)$value/Z, numeric(1))
  q <- function(pr) vapply(pr, function(pp) uniroot(function(t) p(t)-pp, c(lo,hi))$root, numeric(1))
  mu <- integrate(function(x) x*d(x), lo, hi)$value
  v2 <- integrate(function(x) (x-mu)^2*d(x), lo, hi)$value
  list(d=d,p=p,q=q,mean=mu,var=v2)
}

# (expr-as-typed, equivalent R function, support, evaluation x)
cases <- list(
  list(name="uniform",  expr="1",            f=function(x) rep(1,length(x)), lo=0,  hi=5, x=2),
  list(name="trunc-exp",expr="exp(-x)",      f=function(x) exp(-x),          lo=0,  hi=5, x=1),
  list(name="approx-N", expr="dnorm(x, 0, 1)",f=function(x) dnorm(x,0,1),    lo=-6, hi=6, x=0.5),
  list(name="poly-x^2", expr="x^2",          f=function(x) x^2,              lo=0,  hi=3, x=2),
  # --- piecewise densities (ifelse / comparisons / logical operators) ---
  list(name="pw-triangle", expr="ifelse(x < 1, x, 2 - x)",
       f=function(x) ifelse(x < 1, x, 2 - x),                                lo=0,  hi=2, x=0.5),
  list(name="pw-step",     expr="ifelse(x < 1, 2, 1)",
       f=function(x) ifelse(x < 1, 2, 1),                                    lo=0,  hi=3, x=2),
  list(name="pw-trapez",   expr="ifelse(x < 1, x, ifelse(x < 2, 1, 3 - x))",
       f=function(x) ifelse(x < 1, x, ifelse(x < 2, 1, 3 - x)),             lo=0,  hi=3, x=1.5),
  list(name="pw-indicator",expr="(x >= 0) * (x < 1) * 2",
       f=function(x) (x >= 0) * (x < 1) * 2,                                 lo=-1, hi=2, x=0.5),
  list(name="pw-and",      expr="(x > 0 & x < 2) * x",
       f=function(x) (x > 0 & x < 2) * x,                                    lo=-1, hi=3, x=1),
  # --- more analytic shapes cross-checked against adaptive quadrature ---
  list(name="beta-poly",   expr="x^2 * (1 - x)^3",
       f=function(x) x^2 * (1 - x)^3,                                        lo=0,  hi=1, x=0.4),
  list(name="dbeta-2-5",   expr="dbeta(x, 2, 5)",
       f=function(x) dbeta(x, 2, 5),                                         lo=0,  hi=1, x=0.3),
  list(name="gauss-bump",  expr="exp(-x^2)",
       f=function(x) exp(-x^2),                                              lo=-4, hi=4, x=0.5),
  list(name="recip-quad",  expr="1/(1 + x^2)",
       f=function(x) 1/(1 + x^2),                                           lo=-8, hi=8, x=1),
  list(name="dgamma-2-1",  expr="dgamma(x, 2, 1)",
       f=function(x) dgamma(x, 2, 1),                                        lo=0,  hi=15, x=2)
)

for (cs in cases) {
  R <- ref_spec(cs$f, cs$lo, cs$hi); x <- cs$x
  testServer(appServer, {
    do.call(session$setInputs, list(distType="CUSTOM", distrib="custom",
      customExpr=cs$expr, customLatex="f", customLo=cs$lo, customHi=cs$hi,
      percentile="pdf", probType="lowerTail",
      xFixedPC=x, xFixedL=x, xFixedU=x, x1=cs$lo+(cs$hi-cs$lo)*0.25, x2=cs$lo+(cs$hi-cs$lo)*0.5, quantile=0.5))
    x1<-cs$lo+(cs$hi-cs$lo)*0.25; x2<-cs$lo+(cs$hi-cs$lo)*0.5
    session$setInputs(outType="Formulas"); chkT(paste0(cs$name,":formula"), nchar(S(output$formulas))>20)
    session$setInputs(outType="PDF", percentile="pdf"); chk(paste0(cs$name,":pdf"), nums(S(output$distribCalc)), R$d(x))
    plok <- !inherits(try(force(output$distribPlot), silent=TRUE), "try-error")   # plot in a plotting mode
    session$setInputs(percentile="quant", quantile=0.5); chk(paste0(cs$name,":median"), qval(S(output$percentCalc)), R$q(0.5), tol=1e-2)
    session$setInputs(percentile="quant", quantile=0.9); chk(paste0(cs$name,":q90"), qval(S(output$percentCalc)), R$q(0.9), tol=1e-2)
    session$setInputs(outType="CDF"); chk(paste0(cs$name,":cdf"), nums(S(output$distribCalc)), R$p(x))
    session$setInputs(outType="Probability", probType="lowerTail"); chk(paste0(cs$name,":lower"), nums(S(output$probCalc)), R$p(x))
    session$setInputs(probType="upperTail"); chk(paste0(cs$name,":upper"), nums(S(output$probCalc)), 1-R$p(x))
    session$setInputs(probType="between"); chk(paste0(cs$name,":between"), nums(S(output$probCalc)), R$p(x2)-R$p(x1))
    session$setInputs(outType="Mean"); chk(paste0(cs$name,":mean"), nums(S(output$meanCalc)), R$mean)
    session$setInputs(outType="Variance"); chk(paste0(cs$name,":var"), nums(S(output$varCalc)), R$var)
    cat(sprintf("%-10s pdf/cdf/mean/var cross-checked, plot:%s\n", cs$name, plok))
  })
}

# ---- closed-form spot checks (independent of any integration) --------------
testServer(appServer, {   # Uniform(0,5): exact values
  do.call(session$setInputs, list(distType="CUSTOM", distrib="custom", customExpr="1",
    customLatex="1", customLo=0, customHi=5, percentile="pdf", probType="lowerTail",
    xFixedPC=2, xFixedL=2, xFixedU=2, x1=1, x2=3, quantile=0.5))
  session$setInputs(outType="PDF", percentile="pdf"); chk("exact:U(0,5) pdf", nums(S(output$distribCalc)), 0.2, tol=1e-4)
  session$setInputs(outType="CDF"); chk("exact:U(0,5) cdf(2)", nums(S(output$distribCalc)), 0.4, tol=2e-3)
  session$setInputs(outType="Mean"); chk("exact:U(0,5) mean", nums(S(output$meanCalc)), 2.5, tol=1e-4)
  session$setInputs(outType="Variance"); chk("exact:U(0,5) var", nums(S(output$varCalc)), 25/12, tol=1e-3)
})

# ---- sandbox + validation: unsafe / invalid expressions are refused --------
bad <- list(
  "system('ls')", "eval(parse(text='1'))", "file('/etc/passwd')",
  "Sys.setenv(X=1)", "x[1]", "readLines('x')", "x + secret", "{ x }", "2 +", "0", "-1"
)
for (b in bad) {
  testServer(appServer, {
    do.call(session$setInputs, list(distType="CUSTOM", distrib="custom", customExpr=b,
      customLatex="f", customLo=0, customHi=5, percentile="pdf", probType="lowerTail",
      xFixedPC=1, xFixedL=1, xFixedU=1, x1=0, x2=2, quantile=0.5))
    chkT(paste0("refuse: ", b), length(inputErrors()) >= 1)
    session$setInputs(outType="Mean")
    chkT(paste0("blank-on-bad: ", b), inherits(try(force(output$meanCalc), silent=TRUE), "try-error"))
  })
}
# lo >= hi is rejected
testServer(appServer, {
  do.call(session$setInputs, list(distType="CUSTOM", distrib="custom", customExpr="exp(-x)",
    customLatex="f", customLo=5, customHi=5, percentile="pdf", probType="lowerTail",
    xFixedPC=1, xFixedL=1, xFixedU=1, x1=0, x2=2, quantile=0.5))
  chkT("refuse: lo>=hi", any(grepl("lower bound", inputErrors())))
})

# ---- self-consistency: CDF monotone in [0,1]; q is p's inverse -------------
s <- make_custom_spec("exp(-x) * (1 + sin(x)^2)", 0, 6)   # a wiggly but valid density
grid <- seq(0, 6, length.out=200); cc <- s$p(grid)
chkT("custom CDF monotone non-decreasing", all(diff(cc) >= -1e-9))
chkT("custom CDF in [0,1]", min(cc) >= -1e-9 && max(cc) <= 1+1e-9)
chkT("custom p(q) inverse", max(abs(s$p(s$q(c(.1,.3,.5,.7,.9))) - c(.1,.3,.5,.7,.9))) < 5e-3)
chkT("custom density integrates to 1", abs(integrate(s$d, 0, 6)$value - 1) < 1e-3)

# ---- mixed distributions (point masses + continuous), analytic references --
# Pure discrete: masses 1,2,3 with weights 1,1,2 -> probs .25,.25,.5
testServer(appServer, {
  do.call(session$setInputs, list(distType="CUSTOM", distrib="custom", customExpr="0",
    customLatex="", customMode="expr", customMasses="1:1, 2:1, 3:2", customLo=0, customHi=3,
    percentile="pdf", probType="lowerTail", xFixedPC=2, xFixedL=2, xFixedU=2, x1=1, x2=3, quantile=0.5))
  session$setInputs(outType="Mean");     chk("mix-discrete:mean", nums(S(output$meanCalc)), 2.25)
  session$setInputs(outType="Variance"); chk("mix-discrete:var",  nums(S(output$varCalc)), 0.6875)
  session$setInputs(outType="CDF", xFixedL=2); chk("mix-discrete:cdf(2)", nums(S(output$distribCalc)), 0.5)
})
# Mixed: mass at 0 (weight 1) + Uniform(0,1) density "1" -> P(X=0)=1/2, mean 1/4
testServer(appServer, {
  do.call(session$setInputs, list(distType="CUSTOM", distrib="custom", customExpr="1",
    customLatex="", customMode="expr", customMasses="0:1", customLo=0, customHi=1,
    percentile="pdf", probType="lowerTail", xFixedPC=0, xFixedL=0, xFixedU=0, x1=0, x2=1, quantile=0.5))
  session$setInputs(outType="Mean");     chk("mix:mean", nums(S(output$meanCalc)), 0.25)
  session$setInputs(outType="CDF", xFixedL=0.5); chk("mix:cdf(0.5)", nums(S(output$distribCalc)), 0.75)
})
chkT("mixed self-consistency: total prob 1",
     { m <- make_custom_spec("1", 0, 1, parse_masses("0:1")); abs(m$p(1) - 1) < 1e-6 })

# ---- LaTeX input mode: conversion matches the equivalent R expression ------
ltx <- list(c("e^{-x}", "exp(-x)"), c("\\frac{1}{1+x^2}", "1/(1+x^2)"),
            c("x^{2}", "x^2"), c("\\sqrt{x}", "sqrt(x)"), c("e^{-x^2/2}", "exp(-x^2/2)"))
for (pr in ltx) {
  a <- make_custom_spec(latex_to_expr(pr[1]), 0.01, 4)$mean
  b <- make_custom_spec(pr[2], 0.01, 4)$mean
  chk(paste0("latex:", pr[1]), a, b, tol = 1e-6)
}

cat(sprintf("\n==== CUSTOM-DIST AUDIT: %d passed, %d FAILED ====\n", P, Fl))
