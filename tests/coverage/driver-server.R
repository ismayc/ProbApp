# Coverage driver for server.R. Drives appServer across every distribution,
# output type, probability type, the edge/colour branches, input validation,
# and dark mode. The plot-BUILDING logic is exercised via the plotObj() reactive
# (a plain ggplot); the renderGirafe wrapper renders an actual SVG only a few
# times (rendering many SVGs under covr instrumentation is slow/unstable).
suppressWarnings(suppressMessages(library(testthat)))

bp <- list(pBG=.5,p=.5,numBinTrials=15,a=1,b=6,numEvents=10,numTrials=5,favBalls=3,
  pNeg=.5,numSuccesses=2,lambda=4,alpha=2,beta=5,alphaG=3,betaG=6,betaE=5,df=10,df1=5,df2=10,
  normMean=0,normVar=1,theta1=0,theta2=5,
  weibShape=2,weibScale=3,lnMeanlog=0,lnSdlog=1,cauchyLoc=0,cauchyScale=1,
  logisLoc=0,logisScale=2,paretoScale=1,paretoShape=3,laplaceLoc=0,laplaceScale=1,
  customExpr="exp(-x)",customLo=0,customHi=5,customMasses="",customMode="expr")
cfg <- list(
  bern =list(t="Discrete",  x=list(xFixedPC=1, xFixedL=0, xFixedU=1, x1=0,  x2=1,  quantile=.5)),
  bin  =list(t="Discrete",  x=list(xFixedPC=3, xFixedL=3, xFixedU=10,x1=2,  x2=6,  quantile=.5)),
  dunif=list(t="Discrete",  x=list(xFixedPC=3, xFixedL=3, xFixedU=4, x1=2,  x2=5,  quantile=.5)),
  geom =list(t="Discrete",  x=list(xFixedPC=2, xFixedL=2, xFixedU=3, x1=1,  x2=4,  quantile=.5)),
  hgeom=list(t="Discrete",  x=list(xFixedPC=2, xFixedL=2, xFixedU=2, x1=1,  x2=3,  quantile=.5)),
  nbin =list(t="Discrete",  x=list(xFixedPC=3, xFixedL=3, xFixedU=5, x1=3,  x2=6,  quantile=.5)),
  poi  =list(t="Discrete",  x=list(xFixedPC=3, xFixedL=3, xFixedU=6, x1=2,  x2=6,  quantile=.5)),
  beta =list(t="Continuous",x=list(xFixedPC=.5,xFixedL=.5,xFixedU=.6,x1=.2, x2=.7, quantile=.5)),
  chisq=list(t="Continuous",x=list(xFixedPC=8, xFixedL=8, xFixedU=12,x1=5,  x2=15, quantile=.5)),
  exp  =list(t="Continuous",x=list(xFixedPC=3, xFixedL=3, xFixedU=6, x1=2,  x2=8,  quantile=.5)),
  f    =list(t="Continuous",x=list(xFixedPC=1, xFixedL=1, xFixedU=2, x1=.5, x2=3,  quantile=.5)),
  gam  =list(t="Continuous",x=list(xFixedPC=15,xFixedL=15,xFixedU=20,x1=10, x2=25, quantile=.5)),
  norm =list(t="Continuous",x=list(xFixedPC=1, xFixedL=1, xFixedU=1, x1=-1, x2=1,  quantile=.5)),
  t    =list(t="Continuous",x=list(xFixedPC=1, xFixedL=1, xFixedU=1, x1=-2, x2=2,  quantile=.5)),
  unif =list(t="Continuous",x=list(xFixedPC=2, xFixedL=2, xFixedU=3, x1=1,  x2=4,  quantile=.5)),
  weib  =list(t="Continuous",x=list(xFixedPC=2.5,xFixedL=2.5,xFixedU=3, x1=1,  x2=4,  quantile=.5)),
  lnorm =list(t="Continuous",x=list(xFixedPC=1.5,xFixedL=1.5,xFixedU=2, x1=.5, x2=3,  quantile=.5)),
  cauchy=list(t="Continuous",x=list(xFixedPC=.5, xFixedL=.5, xFixedU=1, x1=-1, x2=1,  quantile=.5)),
  logis =list(t="Continuous",x=list(xFixedPC=1,  xFixedL=1,  xFixedU=2, x1=-2, x2=2,  quantile=.5)),
  pareto=list(t="Continuous",x=list(xFixedPC=2,  xFixedL=2,  xFixedU=3, x1=1.5,x2=4,  quantile=.5)),
  laplace=list(t="Continuous",x=list(xFixedPC=.5,xFixedL=.5, xFixedU=1, x1=-1, x2=1,  quantile=.5)),
  custom=list(t="CUSTOM",    x=list(xFixedPC=1,  xFixedL=1,  xFixedU=1, x1=.5, x2=2,  quantile=.5)))

touch <- function(x) invisible(suppressWarnings(try(force(x), silent = TRUE)))

# ---- plot-building branches (via plotObj) + calc outputs --------------------
for (nm in names(cfg)) {
  C <- cfg[[nm]]
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType=C$t, distrib=nm, percentile="pdf", probType="lowerTail"), bp, C$x))
    session$setInputs(outType="Formulas"); touch(output$formulas)
    session$setInputs(outType="PDF", percentile="pdf"); touch(plotObj()); touch(output$distribCalc)
    session$setInputs(percentile="quant"); touch(plotObj()); touch(output$percentCalc)
    if (C$t == "Discrete") {
      session$setInputs(percentile="quant", quantile=1); touch(plotObj())   # flip colour branch
      session$setInputs(percentile="quant", quantile=0); touch(plotObj())
      session$setInputs(percentile="quant", quantile=.5)
    }
    session$setInputs(outType="CDF"); touch(plotObj()); touch(output$distribCalc)
    for (pt in c("lowerTail","between","upperTail","extreme")) {
      session$setInputs(outType="Probability", probType=pt); touch(plotObj()); touch(output$probCalc)
    }
    # edge branches: x2 <= x1 (calc only)
    session$setInputs(outType="Probability", probType="between", x1=C$x$x2, x2=C$x$x1); touch(output$probCalc)
    session$setInputs(probType="extreme", x1=C$x$x2, x2=C$x$x1); touch(output$probCalc)
    session$setInputs(probType="lowerTail", x1=C$x$x1, x2=C$x$x2)
    if (C$t == "Discrete") {  # flip the alternate fill-colour-ordering branches
      session$setInputs(outType="Probability", probType="lowerTail", xFixedL=9999); touch(plotObj())
      session$setInputs(probType="upperTail", xFixedU=-9999); touch(plotObj())
      session$setInputs(probType="between", x1=-9999, x2=9999); touch(plotObj())
      session$setInputs(probType="extreme", x1=0, x2=1); touch(plotObj())
      session$setInputs(probType="lowerTail", xFixedL=C$x$xFixedL, x1=C$x$x1, x2=C$x$x2)
    }
    session$setInputs(outType="Mean"); touch(output$meanCalc)
    session$setInputs(outType="Variance"); touch(output$varCalc)
  })
}

# ---- renderGirafe wrapper: a few real SVG renders, light and dark -----------
for (dm in c("light", "dark")) {
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="PDF",
                                      percentile="pdf", probType="lowerTail", dark_mode=dm), bp, cfg$bin$x))
    touch(output$distribPlot)
  })
}
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Continuous", distrib="norm", outType="Probability",
                                    probType="between", dark_mode="dark"), bp, cfg$norm$x))
  touch(output$distribPlot)
})

# ---- input validation: trigger every distribution's error branches ---------
inval <- list(
  bern=list(pBG=1.5), bin=list(numBinTrials=5.4, p=1.5), dunif=list(a=6, b=2),
  geom=list(pBG=0), hgeom=list(numEvents=10, numTrials=12, favBalls=12),
  nbin=list(pNeg=1.2, numSuccesses=0), poi=list(lambda=0),
  beta=list(alpha=0, beta=-1), chisq=list(df=0), exp=list(betaE=0),
  f=list(df1=0, df2=-1), gam=list(alphaG=0, betaG=-2), norm=list(normVar=-1),
  t=list(df=0), unif=list(theta1=5, theta2=2),
  weib=list(weibScale=0), lnorm=list(lnSdlog=-1), cauchy=list(cauchyScale=0),
  logis=list(logisScale=-1), pareto=list(paretoShape=-1), laplace=list(laplaceScale=0),
  custom=list(customExpr="system('ls')"))
for (nm in names(inval)) {
  C <- cfg[[nm]]
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType=C$t, distrib=nm, outType="Mean",
                                      percentile="pdf", probType="lowerTail"),
                                 modifyList(bp, inval[[nm]]), C$x))
    touch(output$inputError)
    touch(output$meanCalc); touch(output$varCalc); touch(output$distribCalc)
    touch(output$probCalc); touch(output$percentCalc); touch(output$distribPlot)
  })
}
testServer(appServer, {   # percentile out of range
  do.call(session$setInputs, c(list(distType="Continuous", distrib="norm", outType="PDF",
                                    percentile="quant"), bp,
                               modifyList(cfg$norm$x, list(quantile=1.5))))
  touch(output$inputError)
})

# ---- reachable defensive guards (the relevant x-input left NULL) -----------
testServer(appServer, {   # PDF/pdf without xFixedPC
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="PDF",
                                    percentile="pdf", probType="lowerTail"), bp))
  touch(plotObj()); touch(output$distribCalc)
})
testServer(appServer, {   # CDF without xFixedL (xFixedPC set so distribCalc's outer guard passes)
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="CDF",
                                    percentile="pdf", probType="lowerTail", xFixedPC=3), bp))
  touch(plotObj()); touch(output$distribCalc)
})
testServer(appServer, {   # discrete 'extreme' without x1/x2
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="Probability",
                                    probType="extreme", percentile="pdf"), bp))
  touch(output$probCalc)
})
testServer(appServer, {   # continuous 'extreme' without x1/x2
  do.call(session$setInputs, c(list(distType="Continuous", distrib="norm", outType="Probability",
                                    probType="extreme", percentile="pdf"), bp))
  touch(output$probCalc)
})

# ---- dynamic control renderers + distType switch ---------------------------
testServer(appServer, {
  session$setInputs(distType="Discrete", outType="Formulas"); touch(output$distName)
  session$setInputs(distType="Continuous"); touch(output$distName)
  session$setInputs(distType="CUSTOM"); touch(output$distName)   # hidden distrib='custom'
  session$setInputs(distrib="norm", outType="PDF"); touch(output$percentileUI)
  session$setInputs(outType="Probability"); touch(output$probTypeSelect)
})

# ---- defensive early-return guards -----------------------------------------
for (nm in names(cfg)) {
  testServer(appServer, {
    session$setInputs(distType=cfg[[nm]]$t, distrib=nm, outType="PDF",
                      percentile="pdf", probType="lowerTail")
    touch(plotObj())
  })
}
for (nm in c("bern","bin","dunif","geom","hgeom","nbin","poi")) {
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Discrete", distrib=nm, outType="PDF",
                                      percentile="quant", probType="lowerTail"), bp))
    touch(plotObj())
  })
}
testServer(appServer, { touch(output$distName) })
# custom in LaTeX input mode: exercises customRExpr() conversion, the mode-label
# observer (both branches via transitions), and the formula fallback
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="CUSTOM", distrib="custom", outType="Formulas",
                                    percentile="pdf", probType="lowerTail"),
                               modifyList(bp, list(customLo=-4, customHi=4)), cfg$custom$x))
  session$setInputs(customMode="latex", customExpr="e^{-x^2/2}")   # -> latex label branch
  touch(output$formulas); touch(output$meanCalc)
  session$setInputs(outType="PDF", percentile="pdf"); touch(output$distribCalc); touch(plotObj())
  session$setInputs(outType="Formulas", customExpr="\\foo{x}")     # bad LaTeX -> formula fallback
  touch(output$formulas); touch(output$inputError)
  session$setInputs(customMode="expr")                            # -> expr label branch
})
# mixed custom (point masses): exercises the atom-stem plot overlay + atom PDF
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="CUSTOM", distrib="custom", outType="PDF",
                                    percentile="pdf", probType="lowerTail"),
                               modifyList(bp, list(customMasses="2:1, 6:1")), cfg$custom$x))
  touch(output$distribCalc); touch(output$distribPlot)        # PDF: P(X=x) atom + stems
  session$setInputs(outType="Probability", probType="between", x1=1, x2=4)
  touch(output$distribPlot)                                    # Probability: stems
  session$setInputs(outType="CDF"); touch(output$distribPlot)  # CDF: jumps (no stems)
  session$setInputs(outType="Mean"); touch(output$meanCalc)
  session$setInputs(outType="Variance"); touch(output$varCalc)
  session$setInputs(outType="Formulas"); touch(output$formulas)
})

# ---- reset + Help/About observers ------------------------------------------
testServer(appServer, {
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin"), bp, cfg$bin$x))
  session$setInputs(reset = 1)   # fires the reset observer (updates every input)
  session$setInputs(about = 1)   # fires the Help/About modal observer
  session$setInputs(customHelp = 1)   # fires the Custom syntax-help modal observer
  applyBookmarkUrl("?_inputs_&distrib=%22bin%22")   # the onBookmarked "Share link" handler
})

# ---- plot click / drag-select observers ------------------------------------
testServer(appServer, {
  # pretend the plot has rendered so event_data() reads the event inputs
  session$userData$plotlyShinyEventIDs <- c("plotly_click-distribPlot", "plotly_selected-distribPlot")
  do.call(session$setInputs, c(list(distType="Discrete", distrib="bin", outType="PDF",
                                    percentile="pdf", probType="lowerTail"), bp, cfg$bin$x))
  session$setInputs(`plotly_click-distribPlot` = '[{"x":7,"y":0.2}]')             # PDF -> xFixedPC
  session$setInputs(outType="CDF")
  session$setInputs(`plotly_click-distribPlot` = '[{"x":5,"y":0.1}]')             # CDF -> xFixedL
  session$setInputs(outType="Probability", probType="lowerTail")
  session$setInputs(`plotly_click-distribPlot` = '[{"x":4}]')                     # lowerTail -> xFixedL
  session$setInputs(probType="upperTail")
  session$setInputs(`plotly_click-distribPlot` = '[{"x":9}]')                     # upperTail -> xFixedU
  session$setInputs(probType="between", x1=2, x2=6)
  session$setInputs(`plotly_click-distribPlot` = '[{"x":3}]')                     # between -> nearer endpoint
  session$setInputs(`plotly_selected-distribPlot` = '[{"x":2},{"x":6}]')          # drag -> x1/x2
  session$setInputs(outType="PDF", percentile="quant")
  session$setInputs(`plotly_click-distribPlot` = '[{"x":5}]')                     # quant: no-op branch
  # continuous + non-finite/empty guards
  session$setInputs(distType="Continuous", distrib="norm", outType="Probability", probType="extreme", x1=-1, x2=1)
  session$setInputs(`plotly_selected-distribPlot` = '[{"x":-1.5},{"x":1.5}]')
  session$setInputs(`plotly_click-distribPlot` = '[{"x":null}]')                  # non-finite guard
  session$setInputs(`plotly_selected-distribPlot` = '[]')                         # empty-selection guard
  session$setInputs(`plotly_click-distribPlot` = '[{"y":0.5}]')                   # ev$x NULL guard
  session$setInputs(distType="Discrete", distrib="bin", outType="Probability", probType="between", x1=2, x2=6)
  session$setInputs(`plotly_click-distribPlot` = '[{"x":5}]')                     # click nearer upper bound -> x2
  session$setInputs(`plotly_selected-distribPlot` = '[{"x":null}]')               # all-NA selection guard
})
