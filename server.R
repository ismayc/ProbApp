
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

suppressMessages({
  library(shiny)
  library(ggplot2)
  library(bslib)
  library(plotly)
})

source("functions.R")
source("formulas.R")

discreteDists <- c("bern", "bin", "dunif", "geom", "hgeom", "nbin", "poi")

# Called by Shiny when "Share link" is clicked: write the encoded state into the
# address bar (so the link can be copied/shared directly) and confirm it. Kept
# at top level — rather than as an inline onBookmarked() closure — so it can be
# exercised directly by the test suite (MockShinySession$doBookmark() does not
# fire onBookmarked callbacks).
applyBookmarkUrl <- function(url) {
  updateQueryString(url)
  showNotification("This configuration is now saved in the page URL — copy the address bar to share it.",
                   duration = 6, type = "message")
}


# The server logic is defined as a named function (appServer) and returned as
# the last expression in this file. shiny's ui.R/server.R loader uses that
# returned value as the server function (the modern replacement for the
# deprecated shinyServer()), and the test suite drives it via
# testServer(appServer).
appServer <- function(input, output, session) {

  # Keep transient / preference / event inputs out of the bookmark URL.
  setBookmarkExclude(c("reset", "about", "dark_mode",
                       "plotly_click-distribPlot", "plotly_selected-distribPlot"))
  # On "Share link", push the encoded state into the address bar (see helper).
  onBookmarked(applyBookmarkUrl)

  # Effective R-expression for the custom density: the density field is read as
  # LaTeX (and converted) when the input mode is "latex", otherwise verbatim.
  customRExpr <- reactive({
    if (identical(input$customMode, "latex")) latex_to_expr(input$customExpr) else input$customExpr
  })
  # Relabel the single density field to match the chosen input mode, so it is
  # clear the LaTeX goes in that field (not in a separate box).
  observeEvent(input$customMode, {
    if (identical(input$customMode, "latex")) {
      updateTextInput(session, "customExpr", label = "Density f(x), as LaTeX:",
                      placeholder = "e.g.  \\frac{1}{\\sqrt{2\\pi}} e^{-x^2/2}")
    } else {
      updateTextInput(session, "customExpr", label = "Density f(x) in x:",
                      placeholder = "e.g.  exp(-x)")
    }
  }, ignoreInit = TRUE)

  ##########################################
  # Input validation                       #
  ##########################################
  # Validate every parameter against its valid domain so a bad entry produces a
  # clear error instead of a silently-wrong or broken result. Each helper
  # returns an error string (or NULL when the value is missing/valid).
  inputErrors <- reactive({
    has   <- function(v) !is.null(v) && is.finite(v)
    whole <- function(v, l) if (has(v) && v != round(v)) sprintf("%s must be a whole number (you entered %s).", l, format(v)) else NULL
    prob  <- function(v, l) if (has(v) && (v <= 0 || v >= 1)) sprintf("%s must be between 0 and 1, exclusive (you entered %s).", l, format(v)) else NULL
    pos   <- function(v, l) if (has(v) && v <= 0) sprintf("%s must be greater than 0 (you entered %s).", l, format(v)) else NULL
    atLeast <- function(v, m, l) if (has(v) && v < m) sprintf("%s must be at least %s (you entered %s).", l, m, format(v)) else NULL
    le    <- function(v, w, vl, wl) if (has(v) && has(w) && v > w) sprintf("%s cannot exceed the %s.", vl, wl) else NULL
    lt    <- function(v, w, msg) if (has(v) && has(w) && v >= w) msg else NULL
    rng01 <- function(v, l) if (has(v) && (v < 0 || v > 1)) sprintf("%s must be between 0 and 1 (you entered %s).", l, format(v)) else NULL

    if (is.null(input$distrib)) return(character(0))
    e <- switch(input$distrib,
      bern  = list(prob(input$pBG, "Probability of success (p)")),
      bin   = list(prob(input$p, "Probability of success (p)"),
                   whole(input$numBinTrials, "Number of trials (n)"),
                   atLeast(input$numBinTrials, 1, "Number of trials (n)")),
      dunif = list(whole(input$a, "Lower bound (a)"),
                   whole(input$b, "Upper bound (b)"),
                   le(input$a, input$b, "lower bound (a)", "upper bound (b)")),
      geom  = list(prob(input$pBG, "Probability of success (p)")),
      hgeom = list(whole(input$numEvents, "Total number of events (N)"), atLeast(input$numEvents, 1, "Total number of events (N)"),
                   whole(input$numTrials, "Number of trials (n)"),       atLeast(input$numTrials, 0, "Number of trials (n)"),
                   whole(input$favBalls,  "Number of favorable events (m)"), atLeast(input$favBalls, 0, "Number of favorable events (m)"),
                   le(input$numTrials, input$numEvents, "number of trials (n)", "total number of events (N)"),
                   le(input$favBalls,  input$numEvents, "number of favorable events (m)", "total number of events (N)")),
      nbin  = list(prob(input$pNeg, "Probability of success (p)"),
                   whole(input$numSuccesses, "Number of successes (r)"),
                   atLeast(input$numSuccesses, 1, "Number of successes (r)")),
      poi   = list(pos(input$lambda, "Rate parameter (λ)")),
      beta  = list(pos(input$alpha, "Shape parameter (α)"), pos(input$beta, "Shape parameter (β)")),
      chisq = list(pos(input$df, "Degrees of freedom (ν)")),
      exp   = list(pos(input$betaE, "Scale parameter (β)")),
      f     = list(pos(input$df1, "Numerator degrees of freedom (d₁)"), pos(input$df2, "Denominator degrees of freedom (d₂)")),
      gam   = list(pos(input$alphaG, "Shape parameter (α)"), pos(input$betaG, "Scale parameter (β)")),
      norm  = list(pos(input$normVar, "Variance (σ²)")),
      t     = list(pos(input$df, "Degrees of freedom (ν)")),
      unif  = list(lt(input$theta1, input$theta2, "Upper bound (θ₂) must be greater than the lower bound (θ₁).")),
      weib  = list(pos(input$weibShape, "Shape parameter (k)"), pos(input$weibScale, "Scale parameter (λ)")),
      lnorm = list(pos(input$lnSdlog, "Standard deviation of log (σ)")),
      cauchy = list(pos(input$cauchyScale, "Scale parameter (γ)")),
      logis = list(pos(input$logisScale, "Scale parameter (s)")),
      pareto = list(pos(input$paretoScale, "Scale parameter (x_m)"), pos(input$paretoShape, "Shape parameter (α)")),
      laplace = list(pos(input$laplaceScale, "Scale parameter (b)")),
      custom = list(validate_custom(customRExpr(), input$customLo, input$customHi, input$customMasses)),
      list()
    )
    # Percentile (used in PDF/Quantile mode) must be a valid probability.
    e <- c(e, list(rng01(input$quantile, "Percentile")))
    unlist(Filter(Negate(is.null), e))
  })

  # Red alert shown above the result cards when any input is invalid.
  output$inputError <- renderUI({
    e <- inputErrors()
    if (length(e) == 0) return(NULL)
    div(class = "alert alert-danger", role = "alert",
        tags$strong(if (length(e) == 1) "Invalid input" else "Invalid inputs"),
        tags$br(),
        HTML(paste(e, collapse = "<br/>")))
  })

  # Reset every control back to its documented default.
  observeEvent(input$reset, {
    updateRadioButtons(session, "distType", selected = "Discrete")
    updateRadioButtons(session, "outType", selected = "Probability")
    defaults <- list(pBG = 0.5, p = 0.5, numBinTrials = 15, a = 1, b = 6,
                     numEvents = 10, numTrials = 5, favBalls = 3, pNeg = 0.5,
                     numSuccesses = 2, lambda = 4, beta = 5, alpha = 2,
                     betaG = 6, alphaG = 3, df = 10, betaE = 5, df1 = 5, df2 = 10,
                     normMean = 0, normVar = 1, theta1 = 0, theta2 = 5,
                     xFixedPC = 1, quantile = 0.5, x1 = 0, x2 = 1,
                     xFixedL = 0, xFixedU = 1,
                     weibShape = 2, weibScale = 1, lnMeanlog = 0, lnSdlog = 1,
                     cauchyLoc = 0, cauchyScale = 1, logisLoc = 0, logisScale = 1,
                     paretoScale = 1, paretoShape = 3, laplaceLoc = 0, laplaceScale = 1,
                     customLo = 0, customHi = 5)
    for (id in names(defaults)) updateNumericInput(session, id, value = defaults[[id]])
    updateRadioButtons(session, "customMode", selected = "expr")
    updateTextInput(session, "customExpr", value = "exp(-x)")
    updateTextInput(session, "customMasses", value = "")
  })

  # Custom-distribution syntax help (opened from the "Syntax & point-mass help"
  # link in the Custom card) — the full guidance expands in a modal.
  observeEvent(input$customHelp, {
    showModal(modalDialog(
      title = "Custom distribution — syntax help",
      easyClose = TRUE, size = "xl", footer = modalButton("Close"),
      HTML(paste0(
        "<p>Define a <strong>density</strong> over a support <code>[&#8467;, u]</code>, optional ",
        "<strong>point masses</strong>, or both (a mixed distribution). The whole distribution ",
        "(curve + masses) is rescaled to total probability 1 — your density need not be normalized.</p>",
        "<h5>R expression mode</h5>",
        "<p>Use <code>x</code>, <code>pi</code>, numbers, and elementary math: ",
        "<code>+ - * / ^</code>, <code>exp</code>, <code>log</code>, <code>sqrt</code>, ",
        "<code>sin</code>/<code>cos</code>/<code>tan</code>, <code>abs</code>, <code>gamma</code>, ",
        "and density functions like <code>dnorm</code>, <code>dgamma</code>, <code>dbeta</code>.</p>",
        "<p><strong>Piecewise</strong> densities use <code>ifelse()</code> and comparisons, e.g. ",
        "<code>ifelse(x &lt; 1, x, 2 - x)</code> (triangular) or ",
        "<code>(x &gt;= 0) * (x &lt; 1) * 2</code> (an indicator).</p>",
        "<h5>LaTeX mode</h5>",
        "<p>Enter standard LaTeX math in <code>x</code>, e.g. ",
        "<code>\\frac{1}{\\sqrt{2\\pi}} e^{-x^2/2}</code>. Supported: <code>\\frac{a}{b}</code>, ",
        "<code>x^{n}</code>, <code>e^{…}</code>, <code>\\sqrt{…}</code>, <code>\\left(\\right)</code>, ",
        "<code>\\cdot</code>, <code>\\pi</code>, <code>\\ln</code>, <code>\\sin/\\cos/\\tan</code>, ",
        "<code>|x|</code>, and implicit multiplication (<code>2x</code>, <code>\\pi x</code>). ",
        "Only <code>x</code>, <code>\\pi</code> and numbers may appear as symbols.</p>",
        "<h5>Point masses</h5>",
        "<p>Enter <code>location:weight</code> pairs separated by commas, e.g. ",
        "<code>2:0.3, 5:0.7</code>. Each adds an atom with probability proportional to its weight. ",
        "A density of <code>0</code> with only masses gives a purely discrete distribution; a positive ",
        "density plus masses gives a mixed distribution (the CDF jumps at each mass).</p>",
        "<p class='text-muted'><small>For security, expressions are evaluated in a sandbox: only the ",
        "whitelisted math above is allowed — no file, network, or system access.</small></p>"
      ))
    ))
  })

  # Help / About dialog.
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "Help / About",
      easyClose = TRUE, size = "l", footer = modalButton("Close"),
      withMathJax(),
      HTML(paste0(
        "<p>Pick a <strong>distribution type</strong> and <strong>distribution</strong> in the sidebar, ",
        "choose an <strong>output type</strong> (Formulas, PDF/Quantile, CDF, Probability, Mean, or ",
        "Variance), and enter the parameters. Results render as formulas and an interactive plot; ",
        "hover the discrete bars to read each probability.</p>",
        "<p><strong>Parameterization notes</strong> (these differ from base R's defaults):</p>",
        "<ul>",
        "<li><strong>Geometric / Negative Binomial</strong> count the <em>number of trials</em> ",
        "(support starts at 1 and at r, respectively), not the number of failures.</li>",
        "<li><strong>Exponential</strong> and <strong>Gamma</strong> use the <em>scale</em> parameter ",
        "\\(\\beta\\) (mean \\(=\\beta\\) and \\(=\\alpha\\beta\\)), not the rate.</li>",
        "<li><strong>Normal</strong> takes the <em>variance</em> \\(\\sigma^2\\), not the standard deviation.</li>",
        "</ul>",
        "<p>Counts (n, N, m, r, a, b) must be whole numbers and the probability of success ",
        "must be strictly between 0 and 1; invalid entries show a red alert.</p>",
        "<p>Developed by Dr. Chester Ismay and Logan Soich.</p>"
      ))
    ))
  })

  # Specification for the continuous distributions added beyond the originals
  # (Weibull, Log-Normal, Cauchy, Logistic, Pareto, Laplace). Reads the current
  # parameter inputs and returns density/CDF/quantile closures, the mean and
  # variance (NA where undefined), a plotting x-window [lo, hi], and a display
  # name. The generic switch-default handlers below use this, so each new family
  # is defined in exactly one place.
  contSpec <- function(d) {
    n <- function(id) as.numeric(input[[id]])
    switch(d,
      weib = { k <- n("weibShape"); lam <- n("weibScale")
        list(name = "Weibull",
             d = function(x) dweibull(x, k, lam), p = function(x) pweibull(x, k, lam), q = function(p) qweibull(p, k, lam),
             mean = lam * gamma(1 + 1/k), var = lam^2 * (gamma(1 + 2/k) - gamma(1 + 1/k)^2),
             lo = 0, hi = qweibull(0.999, k, lam)) },
      lnorm = { mu <- n("lnMeanlog"); sg <- n("lnSdlog")
        list(name = "Log-Normal",
             d = function(x) dlnorm(x, mu, sg), p = function(x) plnorm(x, mu, sg), q = function(p) qlnorm(p, mu, sg),
             mean = exp(mu + sg^2/2), var = (exp(sg^2) - 1) * exp(2*mu + sg^2),
             lo = 0, hi = qlnorm(0.999, mu, sg)) },
      cauchy = { x0 <- n("cauchyLoc"); g <- n("cauchyScale")
        list(name = "Cauchy",
             d = function(x) dcauchy(x, x0, g), p = function(x) pcauchy(x, x0, g), q = function(p) qcauchy(p, x0, g),
             mean = NA_real_, var = NA_real_,
             lo = qcauchy(0.02, x0, g), hi = qcauchy(0.98, x0, g)) },
      logis = { mu <- n("logisLoc"); s <- n("logisScale")
        list(name = "Logistic",
             d = function(x) dlogis(x, mu, s), p = function(x) plogis(x, mu, s), q = function(p) qlogis(p, mu, s),
             mean = mu, var = s^2 * pi^2 / 3,
             lo = qlogis(0.001, mu, s), hi = qlogis(0.999, mu, s)) },
      pareto = { xm <- n("paretoScale"); a <- n("paretoShape")
        list(name = "Pareto",
             d = function(x) dpareto(x, xm, a), p = function(x) ppareto(x, xm, a), q = function(p) qpareto(p, xm, a),
             mean = if (is.finite(a) && a > 1) a * xm / (a - 1) else NA_real_,
             var  = if (is.finite(a) && a > 2) a * xm^2 / ((a - 1)^2 * (a - 2)) else NA_real_,
             lo = xm, hi = qpareto(0.99, xm, a)) },
      laplace = { mu <- n("laplaceLoc"); b <- n("laplaceScale")
        list(name = "Laplace",
             d = function(x) dlaplace(x, mu, b), p = function(x) plaplace(x, mu, b), q = function(p) qlaplace(p, mu, b),
             mean = mu, var = 2 * b^2,
             lo = qlaplace(0.001, mu, b), hi = qlaplace(0.999, mu, b)) },
      # User-defined density: parsed in a sandbox and integrated numerically
      # (see make_custom_spec / safe_pdf in functions.R). inputErrors() blocks
      # every output while the expression/support is invalid, so this is only
      # reached with a buildable spec.
      custom = make_custom_spec(customRExpr(), n("customLo"), n("customHi"), parse_masses(input$customMasses))
    )
  }
  # The continuous families that go through contSpec() (vs the bespoke originals).
  specDists <- c("weib", "lnorm", "cauchy", "logis", "pareto", "laplace", "custom")

  #Select distribution after choosing Discrete or Continuous type
  output$distName <- renderUI({
    if(is.null(input$distType)) return ()
    if(input$distType=="Discrete"){
      radioButtons("distrib",
                   strong("Distribution:"),
                   selected = restoreInput(id = "distrib", default = "bin"),
                   list("Bernoulli"="bern",
                        "Binomial"="bin",
                        "Discrete Uniform"="dunif",
                        "Geometric"="geom",
                        "Hypergeometric"="hgeom",
                        "Negative Binomial"="nbin",
                        "Poisson"="poi"),
                   #inline=TRUE
      )
    } else if(input$distType=="Continuous"){
      radioButtons("distrib",
                   strong("Distribution:"),
                   selected = restoreInput(id = "distrib", default = "beta"),
                   list("Beta"="beta",
                        #"Cauchy"="cauchy",
                        "Chi-square"="chisq",
                        "Exponential"="exp",
                        "F"="f",
                        "Gamma"="gam",
                        #"Laplace"="lap",
                        #"Logistic"="logi",
                        #"Log-Normal"="lognorm",
                        "Normal"="norm",
                        #"Pareto"="pareto",
                        "t"="t",
                        "Uniform"="unif",
                        "Cauchy"="cauchy",
                        "Laplace"="laplace",
                        "Logistic"="logis",
                        "Log-Normal"="lnorm",
                        "Pareto"="pareto",
                        "Weibull"="weib"),
                   #"Weibull"="weib")
                   # inline=TRUE
      )
    } else if(input$distType=="CUSTOM"){
      # Custom is its own distribution type (it can be discrete, continuous, or
      # a mix of point masses and a curve), so there is no sub-distribution to
      # pick — fix the distrib id to "custom" via a hidden control.
      div(style = "display:none;",
          radioButtons("distrib", NULL, c("Custom" = "custom"),
                       selected = restoreInput(id = "distrib", default = "custom")))
    }
  })
  
  output$probTypeSelect <- renderUI({
    if(input$outType == "Probability"){
      selectInput("probType", label = h5("Select probability type:"), 
                  selected = restoreInput(id = "probType", default = "lowerTail"),
                  list("Less Than or Equal To" = "lowerTail",
                       "Between Values (Inclusive)" = "between",
                       "Greater Than or Equal To" = "upperTail",
                       "More Extreme" = "extreme")
      )
    }
  })
  
  ##########################################
  #Formulas for the distributions          #
  ##########################################
  #Formulas defined in formulas.R file
  output$formulas <- renderUI({  
    if(is.null(input$distrib)) return ()
    if(input$outType == "Formulas"){
      switch(input$distrib,
             #Discrete
             bern = bernForm,
             bin = binForm,
             dunif = discUnifForm,
             geom = geomForm,
             hgeom = hyperGeomForm,
             nbin = negBinForm,
             poi = poiForm,
             #Continuous
             beta = betaForm,
             chisq = chisqForm,
             exp = expForm,
             f = fForm,
             gam = gamForm,
             norm = normForm,
             t = tForm,
             unif = unifForm,
             weib = weibForm,
             lnorm = lnormForm,
             cauchy = cauchyForm,
             logis = logisForm,
             pareto = paretoForm,
             laplace = laplaceForm,
             custom = customForm(customRExpr(),
                                 if (identical(input$customMode, "latex")) input$customExpr else NULL,
                                 as.numeric(input$customLo), as.numeric(input$customHi), input$customMasses),
      )
    }
  })
  
  # Build the ggplot object (rendered interactively below via renderGirafe).
  plotObj <- reactive({
    #To allow for the parameters to fill before calculating/plotting
    if(is.null(input$distrib)) return ()
    if(is.null(input$probType)) return()
    if(input$distrib == "bern" && is.null(input$pBG)) return ()
    else if(input$distrib == "bin" && (is.null(input$numBinTrials) || is.null(input$p))) return ()
    else if(input$distrib == "dunif" && (is.null(input$a) || is.null(input$b))) return ()
    else if(input$distrib == "geom" && is.null(input$pBG)) return ()
    else if(input$distrib == "hgeom" && (is.null(input$numTrials) || is.null(input$numEvents) || is.null(input$favBalls))) return ()
    else if(input$distrib == "nbin" && (is.null(input$pNeg) || is.null(input$numSuccesses))) return ()
    else if(input$distrib == "poi" && is.null(input$lambda)) return ()
    else if(input$distrib == "exp" && is.null(input$betaE)) return ()
    else if(input$distrib == "gam" && (is.null(input$betaG) || is.null(input$alphaG))) return ()
    else if(input$distrib == "beta" && (is.null(input$beta) || is.null(input$alpha))) return ()
    else if(input$distrib == "norm" && (is.null(input$normMean) || is.null(input$normVar))) return ()
    else if(input$distrib == "unif" && (is.null(input$theta1) || is.null(input$theta2))) return ()
    else if(input$distrib == "t" && is.null(input$df)) return ()
    else if(input$distrib == "chisq" && is.null(input$df)) return ()
    else if(input$distrib == "f" && (is.null(input$df1) || is.null(input$df2))) return ()
    else if(input$distrib == "custom" && (is.null(input$customExpr) || is.null(input$customLo) || is.null(input$customHi))) return ()

    if(input$outType == "PDF" && input$percentile == "pdf" && !is.null(input$percentile)){
      if(is.null(input$xFixedPC)) return ()
      
      #Plot PDF function using the distribPlot function in functions.R with shading of parameter value
      switch(input$distrib, 
             "bern" = distribPlot(func = dbinom, args = c(1, input$pBG), inputValue = input$xFixedPC),
             
             "bin" = distribPlot(range = 0:input$numBinTrials, args = c(input$numBinTrials, input$p), 
                                 inputValue = input$xFixedPC, distribName = "Binomial"),
             
             "dunif" = distribPlot(func = dunifdisc, range = input$a:input$b, args = c(input$a, input$b),
                                   inputValue = input$xFixedPC, xlabel = "Discrete Values",
                                   distribName = "Discrete Uniform"),
             
             "geom" = distribPlot(func = dgeom, range = 1:ceiling(qgeom(0.9999, prob=input$pBG)), args = c(input$pBG),
                                  inputValue = input$xFixedPC, xlabel = "Number of Trials",
                                  distribName = "Geometric", numArgs = 1, paramAdjust = 1),
             
             "hgeom" = distribPlot(func = dhyper, 
                                   range = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                   args = c(input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                                   inputValue = input$xFixedPC, distribName = "Hypergeometric", numArgs = 3),
             
             "nbin" = distribPlot(func = dnbinom, range = input$numSuccesses:ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg)), 
                                  args = c(input$numSuccesses, input$pNeg),
                                  inputValue = input$xFixedPC, xlabel = "Number of Trials",
                                  distribName = "Negative Binomial", paramAdjust = input$numSuccesses),
             
             "poi" = distribPlot(func = dpois, range = 0:ceiling(qpois(0.9999, input$lambda)),
                                 args = c(input$lambda), inputValue = input$xFixedPC,
                                 xlabel = "Number of Occurrences",
                                 distribName = "Poisson", numArgs = 1),
             
             #Continuous
             "beta" = beta_prob_area_plot(input$xFixedPC-sqrt(as.numeric(input$alpha)*as.numeric(input$beta)/((as.numeric(input$alpha) + as.numeric(input$beta))^2*(as.numeric(input$alpha)+as.numeric(input$beta)+1)))/50.0,
                                          input$xFixedPC+sqrt(as.numeric(input$alpha)*as.numeric(input$beta)/((as.numeric(input$alpha) + as.numeric(input$beta))^2*(as.numeric(input$alpha)+as.numeric(input$beta)+1)))/50.0,
                                          shape1 = input$alpha, shape2 = input$beta),
             "chisq" = chisq_prob_area_plot(input$xFixedPC - sqrt(2*input$df)/50.0,
                                            input$xFixedPC + sqrt(2*input$df)/50.0,
                                            df = input$df),
             "f" = f_prob_area_plot(input$xFixedPC - (qf(0.999, df1=as.numeric(input$df1), df2=as.numeric(input$df2)) - qf(0.001, df1=as.numeric(input$df1), df2=as.numeric(input$df2)))/350.0,
                                    input$xFixedPC + (qf(0.999, df1=as.numeric(input$df1), df2=as.numeric(input$df2)) - qf(0.001, df1=as.numeric(input$df1), df2=as.numeric(input$df2)))/350.0,
                                    df1 = input$df1,
                                    df2 = input$df2),
             "norm" = normal_prob_area_plot(input$xFixedPC-sqrt(as.numeric(input$normVar))/50.0, input$xFixedPC+sqrt(as.numeric(input$normVar))/50.0, 
                                            mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
             "unif" = uniform_prob_area_plot(input$xFixedPC-(input$theta2-input$theta1)/500.0, input$xFixedPC+(input$theta2-input$theta1)/500.0, min = input$theta1, max = input$theta2),
             "exp" = exp_prob_area_plot(input$xFixedPC-input$betaE/100.0, input$xFixedPC+input$betaE/100.0, shape = 1, scale = input$betaE,
                                        limits = c(0, qgamma(0.999, shape = 1, 
                                                             scale = as.numeric(input$betaE)))),
             "gam" = gamma_prob_area_plot(input$xFixedPC-input$betaG/50.0, input$xFixedPC+input$betaG/50.0, shape = input$alphaG, scale = input$betaG,
                                          limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                               scale = as.numeric(input$betaG)))),
             "t" = t_prob_area_plot(input$xFixedPC - sqrt(input$df/(input$df - 2))/50.0,
                                    input$xFixedPC + sqrt(input$df/(input$df - 2))/50.0,
                                    df = input$df),
             # default: contSpec-based families (Weibull, Log-Normal, Cauchy, ...)
             { s <- contSpec(input$distrib); w <- (s$hi - s$lo) / 400
               cont_area_plot(input$xFixedPC - w, input$xFixedPC + w, s$d, c(s$lo, s$hi),
                              paste(s$name, "Probability Density Function")) }
      )
    }

    ######Need to put into function similar to distribPlot above
    #Plot PDF function with shading of appropriate quantile value
    else if(input$outType == "PDF" && input$percentile != "pdf" && !is.null(input$percentile)){
      switch(input$distrib,
             "bern" = qplot(factor(0:1), 
                            dbinom(0:1, 1, input$pBG),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Bernoulli Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = 0:1 <= if(!is.null(input$quantile) && !is.null(input$pBG))
                              qbinom(input$quantile, 1, input$pBG)
                            else {-1}
             )
             + scale_fill_manual(values= if(!is.null(input$quantile) && !is.null(input$pBG) && qbinom(input$quantile, 1, input$pBG) == 1) c(prob_hl, prob_base)
                                 else c(prob_base, prob_hl)
             )
             + guides(fill = "none") + theme_prob(),
             "bin" = qplot(factor(0:input$numBinTrials), 
                           dbinom(0:input$numBinTrials, input$numBinTrials, input$p),
                           xlab = "Number of Successes", 
                           ylab = "Probability", 
                           main = "Binomial Probability Mass Function\n",
                           geom = "bar", 
                           stat = "identity",
                           fill = 0:input$numBinTrials <= if(!is.null(input$quantile) && !is.null(input$numBinTrials) && !is.null(input$p))
                             qbinom(input$quantile, input$numBinTrials, input$p)
                           else {-1}
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= 
                                   if (!is.null(input$quantile) && !is.null(input$numBinTrials) && !is.null(input$p) && 
                                         qbinom(input$quantile, input$numBinTrials, input$p) == input$numBinTrials){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(), 
             "dunif" = qplot(factor(input$a:input$b), 
                             dunifdisc(input$a:input$b, input$a, input$b),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Discrete Uniform Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill =  input$a:input$b <= if(!is.null(input$quantile) && !is.null(input$a) && !is.null(input$b))
                               qunifdisc(input$quantile, input$a, input$b)
                             else {-1}
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= 
                                   if (!is.null(input$quantile) && !is.null(input$a) && !is.null(input$b) &&
                                         qunifdisc(input$quantile, input$a, input$b) == input$b){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(),
             "geom" = qplot(factor(1:ceiling(qgeom(0.9999, prob=input$pBG)+1)),  ##### How large to set the bounds?
                            dgeom(1:ceiling(qgeom(0.9999, prob=input$pBG)+1), input$pBG),
                            xlab = "Number of Trials", 
                            ylab = "Probability", 
                            main = "Geometric Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = 1:ceiling(qgeom(0.9999, prob=input$pBG)+1) <= if(!is.null(input$quantile) && !is.null(input$pBG))
                              qgeom(as.numeric(input$quantile), as.numeric(input$pBG)) + 1
                            else {-1}   
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= 
                                   if (!is.null(input$quantile) && !is.null(input$pBG) && 
                                         ceiling(qgeom(0.9999, prob=input$pBG)) <= qgeom(as.numeric(input$quantile), as.numeric(input$pBG)) + 1){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(), 
             "hgeom" = qplot(factor(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials)), 
                             dhyper(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                    input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Hypergeometric Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) 
                             <= if(!is.null(input$quantile) && !is.null(input$favBalls) && !is.null(input$numEvents) && !is.null(input$numTrials))
                               qhyper(as.numeric(input$quantile), as.numeric(input$favBalls), 
                                      (as.numeric(input$numEvents) - as.numeric(input$favBalls)), 
                                      as.numeric(input$numTrials))
                             else {-1}
             )
             + scale_fill_manual(values= 
                                   if ((!is.null(input$quantile) && !is.null(input$favBalls) && !is.null(input$numEvents) && !is.null(input$numTrials)) &&
                                         qhyper(as.numeric(input$quantile), as.numeric(input$favBalls), (as.numeric(input$numEvents) - as.numeric(input$favBalls)), 
                                                as.numeric(input$numTrials)) == input$favBalls){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(), 
             "nbin" = qplot(factor(input$numSuccesses:ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg))), 
                            dnbinom(input$numSuccesses:ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg)), input$numSuccesses, input$pNeg),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Negative Binomial Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = input$numSuccesses:ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg)) <= if(!is.null(input$quantile) && !is.null(input$numSuccesses) && !is.null(input$pNeg))
                              qnbinom(as.numeric(input$quantile), 
                                      as.numeric(input$numSuccesses), as.numeric(input$pNeg)) + as.numeric(input$numSuccesses)
                            else {-1}
             )
             + scale_fill_manual(values=  if(!is.null(input$quantile) && !is.null(input$numSuccesses) && !is.null(input$pNeg) &&
                                               ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg)) <= qnbinom(as.numeric(input$quantile), 
                                                                                                                             as.numeric(input$numSuccesses), 
                                                                                                                             as.numeric(input$pNeg)) + as.numeric(input$numSuccesses)){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(), 
             "poi" = qplot(factor(0:ceiling(qpois(0.9999, input$lambda))),
                           dpois(0:ceiling(qpois(0.9999, input$lambda)), input$lambda),
                           xlab = "Number of Occurrences", 
                           ylab = "Probability", 
                           main = "Poisson Probability Mass Function\n",
                           geom = "bar", 
                           stat = "identity",
                           fill = 0:ceiling(qpois(0.9999, input$lambda)) <= if(!is.null(input$quantile) && !is.null(input$lambda))
                             qpois(input$quantile, input$lambda)
                           else {-1}
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(!is.null(input$quantile) && !is.null(input$lambda) && 
                                              ceiling(qpois(0.9999, input$lambda)) <= qpois(input$quantile, input$lambda)){c(prob_hl, prob_base)}
                                 else{c(prob_base, prob_hl)})
             + guides(fill = "none") + theme_prob(),
             
             #Continuous
             "beta" = beta_prob_area_plot(0, qbeta(as.numeric(input$quantile), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)),
                                          shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)
             ),
             "chisq" = chisq_prob_area_plot(0,
                                            qchisq(as.numeric(input$quantile), df = as.numeric(input$df)),
                                            df = as.numeric(input$df)
             ),
             "f" = f_prob_area_plot(0,
                                    qf(as.numeric(input$quantile), df1=as.numeric(input$df1), df2=as.numeric(input$df2)),
                                    df1 = input$df1,
                                    df2 = input$df2
             ),
             "norm" = normal_prob_area_plot(floor(as.numeric(input$normMean) - 4 * sqrt(as.numeric(input$normVar))), 
                                            qnorm(as.numeric(input$quantile), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))), 
                                            mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))
             ),
             "t" = t_prob_area_plot(floor(qt(0.001, input$df)),
                                    qt(as.numeric(input$quantile), df = as.numeric(input$df)),
                                    df = as.numeric(input$df)
             ),
             "unif" = uniform_prob_area_plot(input$theta1, 
                                             qunif(input$quantile, as.numeric(input$theta1), as.numeric(input$theta2)), 
                                             min = as.numeric(input$theta1), 
                                             max = as.numeric(input$theta2)
                                             
             ),
             "exp" = exp_prob_area_plot(0, qgamma(as.numeric(input$quantile), shape = as.numeric(1), scale = as.numeric(input$betaE)), 
                                        shape = 1, scale = as.numeric(input$betaE),
                                        limits = c(0, qgamma(0.999, shape = 1, 
                                                             scale = as.numeric(input$betaE)))
             ),
             "gam" = gamma_prob_area_plot(0, qgamma(as.numeric(input$quantile), 
                                                    shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)), 
                                          shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG), 
                                          limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)))
             ),
             # default: contSpec-based families — shade from lo to the quantile
             { s <- contSpec(input$distrib)
               cont_area_plot(s$lo, s$q(as.numeric(input$quantile)), s$d, c(s$lo, s$hi),
                              paste(s$name, "Probability Density Function")) }
      )
    }
    #Plot CDF with appropriate shading corresponding to fixed value input
    else if(input$outType == "CDF"){
      if(is.null(input$xFixedL)) return ()
      switch(input$distrib,
             "bern" = distribPlot(func = pbinom, range = 0:1, args = c(1, input$pBG),
                                  inputValue = input$xFixedL, xlabel = "Number of Successes",
                                  distribName = "Bernoulli", plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "bin" = distribPlot(func = pbinom, range = 0:input$numBinTrials, args = c(input$numBinTrials, input$p),
                                 inputValue = input$xFixedL, xlabel = "Number of Successes",
                                 distribName = "Binomial", plotType = "Cumulative",
                                 mainLabel = "Cumulative Distribution Function"),
             
             "dunif" = distribPlot(func = punifdisc, range = input$a:input$b, args = c(input$a, input$b),
                                   inputValue = input$xFixedL, xlabel = "Discrete Values",
                                   distribName = "Discrete Uniform", plotType = "Cumulative",
                                   mainLabel = "Cumulative Distribution Function"),
             
             "geom" = distribPlot(func = pgeom, range = 1:ceiling(qgeom(0.9999, prob=input$pBG)), args = c(input$pBG),
                                  inputValue = input$xFixedL, xlabel = "Number of Trials",
                                  distribName = "Geometric", numArgs = 1, paramAdjust = 1,
                                  plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "hgeom" = distribPlot(func = phyper, 
                                   range = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                   args = c(input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                                   inputValue = input$xFixedL, distribName = "Hypergeometric", numArgs = 3,
                                   plotType = "Cumulative",
                                   mainLabel = "Cumulative Distribution Function"),
             
             "nbin" = distribPlot(func = pnbinom, range = input$numSuccesses:ceiling(qnbinom(0.9999, size=input$numSuccesses, prob=input$pNeg)), 
                                  args = c(input$numSuccesses, input$pNeg),
                                  inputValue = input$xFixedL, xlabel = "Number of Trials",
                                  distribName = "Negative Binomial", paramAdjust = input$numSuccesses,
                                  plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "poi" = distribPlot(func = ppois, range = 0:ceiling(qpois(0.9999, input$lambda)),
                                 args = c(input$lambda), inputValue = input$xFixedL,
                                 xlabel = "Number of Occurrences",
                                 distribName = "Poisson", numArgs = 1,
                                 plotType = "Cumulative",
                                 mainLabel = "Cumulative Distribution Function"),
             
             #Continuous
             "beta" = beta_prob_CDF_plot(input$xFixedL-sqrt(as.numeric(input$alpha)*as.numeric(input$beta)/((as.numeric(input$alpha) + as.numeric(input$beta))^2*(as.numeric(input$alpha)+as.numeric(input$beta)+1)))/50.0,
                                         input$xFixedL+sqrt(as.numeric(input$alpha)*as.numeric(input$beta)/((as.numeric(input$alpha) + as.numeric(input$beta))^2*(as.numeric(input$alpha)+as.numeric(input$beta)+1)))/50.0,
                                         shape1 = input$alpha, shape2 = input$beta),
             "norm" = normal_prob_CDF_plot(input$xFixedL-sqrt(as.numeric(input$normVar))/50.0, input$xFixedL+sqrt(as.numeric(input$normVar))/50.0, 
                                           mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
             "t" = t_prob_CDF_plot(input$xFixedL - sqrt(input$df/(input$df - 2))/50.0,
                                   input$xFixedL + sqrt(input$df/(input$df - 2))/50.0,
                                   df = input$df),
             "f" = f_prob_CDF_plot(input$xFixedL - (qf(0.999, df1=as.numeric(input$df1), df2=as.numeric(input$df2)) - qf(0.001, df1=as.numeric(input$df1), df2=as.numeric(input$df2)))/350.0,
                                   input$xFixedL + (qf(0.999, df1=as.numeric(input$df1), df2=as.numeric(input$df2)) - qf(0.001, df1=as.numeric(input$df1), df2=as.numeric(input$df2)))/350.0,
                                   df1 = input$df1,
                                   df2 = input$df2),
             "chisq" = chisq_prob_CDF_plot(input$xFixedL - sqrt(2*input$df)/50.0,
                                           input$xFixedL + sqrt(2*input$df)/50.0,
                                           df = input$df),
             "unif" = uniform_prob_CDF_plot(input$xFixedL-(input$theta2-input$theta1)/500.0, input$xFixedL+(input$theta2-input$theta1)/500.0, min = input$theta1, max = input$theta2),
             "exp" = exp_prob_CDF_plot(input$xFixedL-input$betaE/100.0, input$xFixedL+input$betaE/100.0, shape = 1, scale = input$betaE,
                                       limits = c(0, qgamma(0.999, shape = 1, 
                                                            scale = as.numeric(input$betaE)))),
             "gam" = gamma_prob_CDF_plot(input$xFixedL-input$beta/50.0, input$xFixedL+input$betaG/50.0, shape = input$alphaG, scale = input$betaG,
                                         limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG),
                                                              scale = as.numeric(input$betaG)))),
             # default: contSpec-based families
             { s <- contSpec(input$distrib); w <- (s$hi - s$lo) / 400
               cont_cdf_plot(input$xFixedL - w, input$xFixedL + w, s$p, c(s$lo, s$hi),
                             paste(s$name, "Cumulative Distribution Function")) }
      )
    }
    #####Need to turn into function calls
    #Plot PDF with appropriate shading depending on probType selection
    else if(input$outType == "Probability"){
      #  if((input$probType == "lowerTail" || input$probType == "upperTail") && is.null(input$xFixedL) && is.null(input$xFixedU)) return ()
      #  if((input$probType == "between" || input$probType == "extreme") && (is.null(input$x1) || is.null(input$x2))) return ()
      switch(input$distrib,
             "bern" = qplot(factor(0:1), 
                            dbinom(0:1, 1, input$pBG),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Bernoulli Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = switch(input$probType,
                                          "between" = 0:1 >= input$x1 & 0:1 <= input$x2,
                                          "lowerTail" = 0:1 <= input$xFixedL,
                                          "upperTail" = 0:1 >= input$xFixedU,
                                          "extreme" = 0:1 <= input$x1 | 0:1 >= input$x2, 
                            )
             )
             + scale_fill_manual(values=
                                   if(input$probType == "between" && pbinom(input$x2, 1, input$pBG) - pbinom(input$x1 - 1, 1, input$pBG) == 1){
                                     c(prob_hl, prob_base)
                                   }
                                 else if(input$probType == "lowerTail" && pbinom(input$xFixedL, 1, input$pBG) == 1){
                                   c(prob_hl, prob_base)
                                 }
                                 else if (input$probType == "upperTail" && 1 - pbinom(input$xFixedU - 1, 1, input$pBG) == 1){
                                   c(prob_hl, prob_base)
                                 }
                                 else if(input$probType == "extreme" && 1 - pbinom(input$x2-1, size = 1, input$pBG) + pbinom(input$x1, 1, input$pBG) == 1){
                                   c(prob_hl, prob_base)
                                 }
                                 else
                                   c(prob_base, prob_hl)
             )
             + guides(fill = "none") + theme_prob(),
             "bin" = qplot(factor(0:input$numBinTrials), 
                           dbinom(0:input$numBinTrials, input$numBinTrials, input$p),
                           xlab = "Number of Successes", 
                           ylab = "Probability", 
                           main = "Binomial Probability Mass Function\n",
                           geom= "bar", 
                           stat= "identity",
                           fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                             switch(input$probType,
                                    "between" = 0:input$numBinTrials >= input$x1 & 0:input$numBinTrials <= input$x2,
                                    "lowerTail" = 0:input$numBinTrials <= input$xFixedL,
                                    "upperTail" = 0:input$numBinTrials >= input$xFixedU,
                                    "extreme" = 0:input$numBinTrials <= input$x1 | 0:input$numBinTrials >= input$x2, 
                             )
                           }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if (pbinom(input$xFixedL, input$numBinTrials, input$p) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if ( pbinom(input$xFixedU - 1, input$numBinTrials, input$p, lower.tail = FALSE) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if (pbinom(as.numeric(input$x2), size = as.numeric(input$numBinTrials), 
                            prob = as.numeric(input$p)) 
                     - pbinom(as.numeric(input$x1)-1, size = as.numeric(input$numBinTrials), 
                              prob = as.numeric(input$p)) == 1)
                 {c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(), 
             "dunif" = qplot(factor(input$a:input$b), 
                             dunifdisc(input$a:input$b, input$a, input$b),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Discrete Uniform Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                               switch(input$probType,
                                      "between" = input$a:input$b >= input$x1 & input$a:input$b <= input$x2,
                                      "lowerTail" = input$a:input$b <= input$xFixedL,
                                      "upperTail" = input$a:input$b >= input$xFixedU,
                                      "extreme" = input$a:input$b <= input$x1 | input$a:input$b >= input$x2
                               ) 
                             }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if (punifdisc(input$xFixedL, input$a, input$b) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if (punifdisc(input$xFixedU - 1, input$a, input$b) == 0){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if (punifdisc(as.numeric(input$x2), as.numeric(input$a), as.numeric(input$b)) 
                     - punifdisc(as.numeric(input$x1)-1, as.numeric(input$a), as.numeric(input$b)) == 1)
                 {c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(),
             "geom" = qplot(factor(1:ceiling(qgeom(0.9999, prob=input$pBG))),
                            dgeom(1:ceiling(qgeom(0.9999, prob=input$pBG))-1, prob=input$pBG),
                            xlab = "Number of Trials", 
                            ylab = "Probability", 
                            main = "Geometric Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                              switch(input$probType,
                                     "between" = 1:ceiling(qgeom(0.9999, prob=input$pBG)) >= input$x1 & 1:ceiling(qgeom(0.9999, prob=input$pBG)) <= input$x2,
                                     "lowerTail" = 1:ceiling(qgeom(0.9999, prob=input$pBG)) <= input$xFixedL,
                                     "upperTail" = 1:ceiling(qgeom(0.9999, prob=input$pBG)) >= input$xFixedU,
                                     "extreme" = 1:ceiling(qgeom(0.9999, prob=input$pBG)) <= input$x1 | 1:ceiling(qgeom(0.9999, prob=input$pBG)) >= input$x2, 
                              )
                            }
             )
             
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if (ceiling(4*1/input$p) <= input$xFixedL){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if ( 1 >= input$xFixedU){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if (input$x1 <= 1 && input$x2 >= ceiling(4*1/input$p))
                   c(prob_hl, prob_base)
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(), 
             "hgeom" = qplot(factor(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials)), 
                             dhyper(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                    input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Hypergeometric Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                               switch(input$probType,
                                      "between" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$x1 & max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$x2,
                                      "lowerTail" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$xFixedL,
                                      "upperTail" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$xFixedU,
                                      "extreme" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$x1 | max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$x2, 
                               )
                             }
             )
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if (phyper(input$xFixedL, input$favBalls, (input$numEvents - input$favBalls), input$numTrials) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if (phyper(input$xFixedU - 1, input$favBalls, (input$numEvents - input$favBalls), input$numTrials) == 0){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if (phyper(as.numeric(input$x2), as.numeric(input$favBalls), 
                            as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) 
                     - phyper(as.numeric(input$x1) - 1, as.numeric(input$favBalls), 
                              as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) == 1)
                 {c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(), 
             "nbin" = qplot(factor(input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg))), 
                            dnbinom(input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)), size = input$numSuccesses, prob = input$pNeg),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Negative Binomial Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                              switch(input$probType,
                                     "between" = input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) >= input$x1 & input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) <= input$x2,
                                     "lowerTail" = input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) <= input$xFixedL,
                                     "upperTail" = input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) >= input$xFixedU,
                                     "extreme" = input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) <= input$x1 | input$numSuccesses:ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)) >= input$x2,
                                     #NULL
                              )
                            }
             )
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if ( ceiling(7*1/input$pNeg) <= input$xFixedL){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if ( pnbinom(input$xFixedU - 1, input$numSuccesses, input$pNeg, lower.tail = FALSE) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if (input$x1 <= input$numSuccesses && input$x2 >= ceiling(qnbinom(0.9999, size = input$numSuccesses, prob = input$pNeg)))
                 {c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(), 
             "poi" = qplot(factor(0:ceiling(qpois(0.9999, input$lambda))),
                           dpois(0:ceiling(qpois(0.9999, input$lambda)), input$lambda),
                           xlab = "Number of Occurrences", 
                           ylab = "Probability", 
                           main = "Poisson Probability Mass Function\n",
                           geom = "bar", 
                           stat = "identity",
                           fill = if(!(is.null(input$xFixedL) || is.null(input$xFixedU))){
                             switch(input$probType,
                                    "between" = 0:ceiling(qpois(0.9999, input$lambda)) >= input$x1 & 0:ceiling(qpois(0.9999, input$lambda)) <= input$x2,
                                    "lowerTail" = 0:ceiling(qpois(0.9999, input$lambda)) <= input$xFixedL,
                                    "upperTail" = 0:ceiling(qpois(0.9999, input$lambda)) >= input$xFixedU,
                                    "extreme" = 0:ceiling(qpois(0.9999, input$lambda)) <= input$x1 | 0:ceiling(qpois(0.9999, input$lambda)) >= input$x2,
                                    #NULL
                             )
                           }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c(prob_base, prob_hl)} 
               else{c(prob_hl,prob_base)}}
               else if (input$probType == "lowerTail"){
                 if (ceiling(4*input$lambda) <= input$xFixedL){c(prob_hl, prob_base)}
                 else{c(prob_base,prob_hl)}}
               else if (input$probType == "upperTail"){
                 if ( ppois(input$xFixedU - 1, input$lambda, lower.tail = FALSE) == 1){c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               else if (input$probType == "between"){
                 if ( input$x1 <= 0 && input$x2 >= ceiling(qpois(0.9999, input$lambda)))
                 {c(prob_hl, prob_base)}
                 else{c(prob_base, prob_hl)}}
               )
             + guides(fill = "none") + theme_prob(),
             
             #Continuous
             "norm" = switch(input$probType,
                             "between" = normal_prob_area_plot(input$x1, input$x2, 
                                                               mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "lowerTail" = normal_prob_area_plot(floor(input$normMean - 4 * sqrt(input$normVar)), input$xFixedL, 
                                                                 mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "upperTail" = normal_prob_area_plot(input$xFixedU, ceiling(input$normMean + 4 * sqrt(input$normVar)),
                                                                 mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "extreme" = normal_prob_area_plot(input$x1, input$x2, 
                                                               mean = input$normMean, sd = sqrt(as.numeric(input$normVar)),
                                                               extreme = TRUE),
                             #NULL
             ),
             "t" = switch(input$probType,
                          "between" = t_prob_area_plot(input$x1, input$x2,  df = as.numeric(input$df)),
                          "lowerTail" = t_prob_area_plot(floor(qt(0.001, df = input$df)), input$xFixedL, df = as.numeric(input$df)),
                          "upperTail" = t_prob_area_plot(input$xFixedU, ceiling(qt(0.999, df = input$df)), df = as.numeric(input$df)),
                          "extreme" = t_prob_area_plot(input$x1, input$x2, 
                                                       df = as.numeric(input$df),
                                                       extreme = TRUE),
                          #NULL
             ),
             "chisq" = switch(input$probType,
                              "between" = chisq_prob_area_plot(input$x1, input$x2,  df = as.numeric(input$df)),
                              "lowerTail" = chisq_prob_area_plot(0, input$xFixedL, df = as.numeric(input$df)),
                              "upperTail" = chisq_prob_area_plot(input$xFixedU, qchisq(0.999, df = input$df), df = as.numeric(input$df)),
                              "extreme" = chisq_prob_area_plot(input$x1, input$x2, 
                                                               df = as.numeric(input$df),
                                                               extreme = TRUE),
                              #NULL
             ),
             "f" = switch(input$probType,
                          "between" = f_prob_area_plot(input$x1, input$x2,  df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                          "lowerTail" = f_prob_area_plot(0, input$xFixedL, df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                          "upperTail" = f_prob_area_plot(input$xFixedU, ceiling(qf(0.999, df1 = as.numeric(input$df1), df2 = as.numeric(input$df2))), 
                                                         df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                          "extreme" = f_prob_area_plot(input$x1, input$x2, 
                                                       df1 = as.numeric(input$df1), df2 = as.numeric(input$df2),
                                                       extreme = TRUE),
                          #NULL
             ),
             "unif" = switch(input$probType,
                             "between" = uniform_prob_area_plot(input$x1, input$x2, 
                                                                min = input$theta1, max = input$theta2),
                             "lowerTail" = uniform_prob_area_plot(input$theta1, input$xFixedL, 
                                                                  min = input$theta1, max = input$theta2),
                             "upperTail" = uniform_prob_area_plot(input$xFixedU, input$theta2,
                                                                  min = input$theta1, max = input$theta2),
                             "extreme" = uniform_prob_area_plot(input$x1, input$x2, 
                                                                min = input$theta1, max = input$theta2, 
                                                                extreme = TRUE),
                             #NULL
             ),
             "beta" = switch(input$probType,
                             "between" = beta_prob_area_plot(input$x1, input$x2, 
                                                             shape1 = input$alpha, shape2 = input$beta),
                             "lowerTail" = beta_prob_area_plot(0, input$xFixedL, 
                                                               shape1 = input$alpha, shape2= input$beta),
                             "upperTail" = beta_prob_area_plot(input$xFixedU, 1,
                                                               shape1 = input$alpha, shape2 = input$beta),
                             "extreme" = beta_prob_area_plot(input$x1, input$x2, 
                                                             shape1 = input$alpha, shape2 = input$beta, 
                                                             extreme = TRUE),
                             #NULL
             ),             
             "exp" = switch(input$probType,
                            "between" = exp_prob_area_plot(input$x1, input$x2, 
                                                           shape = 1, scale = input$betaE),
                            "lowerTail" = exp_prob_area_plot(0, input$xFixedL, 
                                                             shape = 1, scale = input$betaE),
                            "upperTail" = exp_prob_area_plot(input$xFixedU, qgamma(0.999, shape=1, scale=input$betaE),
                                                             shape = 1, scale = input$betaE),
                            "extreme" = exp_prob_area_plot(input$x1, input$x2, 
                                                           shape = 1, scale = input$betaE, 
                                                           extreme = TRUE),
                            #NULL
             ),
             "gam" = switch(input$probType,
                            "between" = gamma_prob_area_plot(input$x1, input$x2, 
                                                             shape = input$alphaG, scale = input$betaG,
                                                             limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                                                  scale = as.numeric(input$betaG)))
                            ),
                            "lowerTail" = gamma_prob_area_plot(0, input$xFixedL, 
                                                               shape = input$alphaG, scale = input$betaG,
                                                               limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                                                    scale = as.numeric(input$betaG)))
                            ),
                            "upperTail" = gamma_prob_area_plot(input$xFixedU, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                                                     scale = as.numeric(input$betaG)),
                                                               shape = input$alphaG, scale = input$betaG,
                                                               limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                                                    scale = as.numeric(input$betaG)))
                            ),
                            "extreme" = gamma_prob_area_plot(input$x1, input$x2, 
                                                             shape = input$alphaG, scale = input$betaG, 
                                                             extreme = TRUE,
                                                             limits = c(0, qgamma(0.999, shape = as.numeric(input$alphaG), 
                                                                                  scale = as.numeric(input$betaG)))
                            ),
                            #NULL
             ),
             # default: contSpec-based families (shading by probability type)
             { s <- contSpec(input$distrib)
               ttl <- paste(s$name, "Probability Density Function")
               switch(input$probType,
                 "between"   = cont_area_plot(input$x1, input$x2, s$d, c(s$lo, s$hi), ttl),
                 "lowerTail" = cont_area_plot(s$lo, input$xFixedL, s$d, c(s$lo, s$hi), ttl),
                 "upperTail" = cont_area_plot(input$xFixedU, s$hi, s$d, c(s$lo, s$hi), ttl),
                 "extreme"   = cont_area_plot(input$x1, input$x2, s$d, c(s$lo, s$hi), ttl, extreme = TRUE)) }
      )
    }
  })

  # Render the plot interactively with plotly: hover tooltips (discrete bars show
  # the probability; continuous curves show x and the density/CDF value), a
  # download-PNG button, dark-mode theming, and click/drag-to-select (handled by
  # the observers below). source = "distribPlot" ties the click/select events.
  output$distribPlot <- renderPlotly({
    req(length(inputErrors()) == 0)   # blank the plot when an input is invalid
    p <- plotObj()
    req(!is.null(p))
    # Mixed custom distributions: overlay stems (height = probability) for the
    # point masses on the density / probability plots, widening the x-window so
    # masses outside the continuous support are still visible. (The CDF plot
    # already shows the jumps via the mixed CDF.)
    if (identical(input$distrib, "custom") && input$outType %in% c("PDF", "Probability")) {
      s <- contSpec("custom"); at <- s$atoms
      if (nrow(at) > 0) {
        p <- p +
          geom_segment(data = at, aes(x = loc, xend = loc, y = 0, yend = prob),
                       color = prob_hl, linewidth = 1) +
          geom_point(data = at, aes(x = loc, y = prob), color = prob_hl, size = 2.5)
        lims <- range(c(s$lo, s$hi, at$loc)); pad <- 0.04 * diff(lims)   # lo < hi => diff > 0
        # replaces the helper's coord_cartesian (benign "already present" note)
        p <- suppressMessages(p + coord_cartesian(xlim = c(lims[1] - pad, lims[2] + pad)))
      }
    }
    dark <- isTRUE(input$dark_mode == "dark")
    tip  <- if (input$distrib %in% discreteDists) "text" else c("x", "y")
    bg   <- if (dark) "#1f2937" else "white"
    grid <- if (dark) "#475569" else "#e5e7eb"
    fg   <- if (dark) "#e5e7eb" else "#1f2937"
    # light title/axis text in dark mode (ggplotly carries the ggplot theme's
    # text colours through, so set them on the ggplot before converting)
    if (dark) p <- p + theme(text = element_text(color = "#e5e7eb"),
                             plot.title = element_text(color = "#f8fafc"),
                             axis.text  = element_text(color = "#cbd5e1"))
    # suppress the benign "Ignoring unknown aesthetics: text" (ggplot doesn't know
    # the `text` aesthetic, but ggplotly uses it to build the hover tooltip)
    gp <- suppressWarnings(ggplotly(p, tooltip = tip, source = "distribPlot"))
    gp <- layout(gp,
                 dragmode = "select",
                 paper_bgcolor = bg, plot_bgcolor = bg,
                 font  = list(color = fg),
                 xaxis = list(gridcolor = grid, zerolinecolor = grid),
                 yaxis = list(gridcolor = grid, zerolinecolor = grid))
    # responsive = TRUE: plotly re-fits to its container on resize / phone
    # rotation rather than keeping the width it was first rendered at.
    gp <- config(gp, displaylogo = FALSE, responsive = TRUE,
                 modeBarButtonsToRemove = list("lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
                 toImageButtonOptions = list(format = "png", filename = "distribution-plot", scale = 2))
    gp <- event_register(gp, "plotly_click")
    event_register(gp, "plotly_selected")
  })

  # Clicking the plot sets the relevant x-value for the current output mode.
  observeEvent(suppressWarnings(event_data("plotly_click", source = "distribPlot")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "distribPlot"))
    if (is.null(ev) || is.null(ev$x)) return()
    xval <- suppressWarnings(as.numeric(ev$x[1]))
    if (!is.finite(xval)) return()
    if (input$distrib %in% discreteDists) xval <- round(xval)
    if (input$outType == "PDF" && identical(input$percentile, "pdf")) {
      updateNumericInput(session, "xFixedPC", value = xval)
    } else if (input$outType == "CDF") {
      updateNumericInput(session, "xFixedL", value = xval)
    } else if (input$outType == "Probability" && !is.null(input$probType)) {
      if (input$probType == "lowerTail") {
        updateNumericInput(session, "xFixedL", value = xval)
      } else if (input$probType == "upperTail") {
        updateNumericInput(session, "xFixedU", value = xval)
      } else if (input$probType %in% c("between", "extreme") &&
                 !is.null(input$x1) && !is.null(input$x2)) {
        # move the nearer endpoint to the clicked value
        if (abs(xval - input$x1) <= abs(xval - input$x2))
          updateNumericInput(session, "x1", value = xval)
        else
          updateNumericInput(session, "x2", value = xval)
      }
    }
  })

  # Dragging a box on the plot sets the lower/upper bounds (between / extreme).
  observeEvent(suppressWarnings(event_data("plotly_selected", source = "distribPlot")), {
    ev <- suppressWarnings(event_data("plotly_selected", source = "distribPlot"))
    if (is.null(ev) || is.null(ev$x) || !length(ev$x)) return()
    xs <- suppressWarnings(as.numeric(ev$x)); xs <- xs[is.finite(xs)]
    if (!length(xs)) return()
    lo <- min(xs); hi <- max(xs)
    if (input$distrib %in% discreteDists) { lo <- round(lo); hi <- round(hi) }
    if (input$outType == "Probability" &&
        !is.null(input$probType) && input$probType %in% c("between", "extreme")) {
      updateNumericInput(session, "x1", value = lo)
      updateNumericInput(session, "x2", value = hi)
    }
  })

  #Produce PDF value for given input value
  output$distribCalc <- renderUI({
    req(length(inputErrors()) == 0)
    if(is.null(input$distrib) || is.null(input$xFixedPC)) return ()
    if(input$outType == "PDF"){
      if(is.null(input$percentile) || input$percentile == "quant") return ()
      switch(input$distrib,
             "bern" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %s$$", input$xFixedPC, 
                                          fmtp(dbinom(as.numeric(input$xFixedPC), 
                                                 size = as.numeric(1), 
                                                 prob = as.numeric(input$pBG)
                                          ))
             )),
             "bin" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %s$$", input$xFixedPC, 
                                         fmtp(dbinom(as.numeric(input$xFixedPC), 
                                                size = as.numeric(input$numBinTrials), 
                                                prob = as.numeric(input$p)
                                         ))
             )),
             "dunif" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %s$$", input$xFixedPC,
                                           fmtp(dunifdisc(as.numeric(input$xFixedPC), 
                                                     as.numeric(input$a), 
                                                     as.numeric(input$b)
                                           ))
             )),
             "geom" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %s$$", input$xFixedPC, 
                                          fmtp(dgeom(as.numeric(input$xFixedPC)-1, 
                                                prob = as.numeric(input$pBG)
                                          ))
             )),
             "hgeom" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %s$$", input$xFixedPC,
                                           fmtp(dhyper(as.numeric(input$xFixedPC), 
                                                  as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), 
                                                  as.numeric(input$numTrials)
                                           ))
                                           
             )),
             "nbin" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %s$$", input$xFixedPC, 
                                          fmtp(dnbinom(as.numeric(input$xFixedPC) - as.numeric(input$numSuccesses), 
                                                  input$numSuccesses, 
                                                  input$pNeg))
             )),
             "poi" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %s$$", input$xFixedPC, 
                                         fmtp(dpois(as.numeric(input$xFixedPC), 
                                               lambda = as.numeric(input$lambda), 
                                         ))
             )),
             #Continuous
             "beta" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                          input$xFixedPC,  
                                          input$xFixedPC,
                                          fmtp(dbeta(as.numeric(input$xFixedPC), 
                                                shape1 = as.numeric(input$alpha),
                                                shape2 = as.numeric(input$beta)
                                          ))
             )),
             "chisq" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                           input$xFixedPC,  
                                           input$xFixedPC,
                                           fmtp(dchisq(as.numeric(input$xFixedPC), 
                                                  df = as.numeric(input$df)
                                           ))
             )),
             "exp" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                         input$xFixedPC,  
                                         input$xFixedPC,
                                         fmtp(dgamma(as.numeric(input$xFixedPC),
                                                shape = as.numeric(1),
                                                scale = as.numeric(input$betaE)
                                         ))
             )),
             "f" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$",
                                       input$xFixedPC,  
                                       input$xFixedPC,
                                       fmtp(df(as.numeric(input$xFixedPC), 
                                          df1 = as.numeric(input$df1),
                                          df2 = as.numeric(input$df2)
                                       ))
             )),
             "gam" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                         input$xFixedPC,  
                                         input$xFixedPC,
                                         fmtp(dgamma(as.numeric(input$xFixedPC), 
                                                shape = as.numeric(input$alphaG),
                                                scale = as.numeric(input$betaG)
                                         ))
             )),
             "norm" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                          input$xFixedPC,  
                                          input$xFixedPC,
                                          fmtp(dnorm(as.numeric(input$xFixedPC), 
                                                mean = as.numeric(input$normMean),
                                                sd = sqrt(as.numeric(input$normVar))
                                          ))
             )),
             "t" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                       input$xFixedPC,  
                                       input$xFixedPC,
                                       fmtp(dt(as.numeric(input$xFixedPC), 
                                          df = as.numeric(input$df)
                                       ))
             )),
             "unif" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$", 
                                          input$xFixedPC,  
                                          input$xFixedPC,
                                          fmtp(dunif(as.numeric(input$xFixedPC), 
                                                min = as.numeric(input$theta1),
                                                max = as.numeric(input$theta2)
                                          ))
             )),
             # Custom may have point masses, so show P(X = x) (the atom
             # probability, 0 if x is not an atom) alongside the density.
             "custom" = { s <- contSpec("custom"); xv <- as.numeric(input$xFixedPC)
               withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = %s \\\\ f(X =  %.03f ) = %s$$",
                                   xv, fmtp(s$pmass(xv)), xv, fmtp(s$d(xv)))) },
             # default: contSpec-based families
             { s <- contSpec(input$distrib)
               withMathJax(sprintf("$$\\mathbb{P}(X =  %.03f ) = 0 \\\\ f(X =  %.03f ) = %s$$",
                                   input$xFixedPC, input$xFixedPC, fmtp(s$d(as.numeric(input$xFixedPC))))) }
      )
      #Produce CDF value in nice LaTeX output
    } else if(input$outType == "CDF"){
      if(is.null(input$distrib) || is.null(input$xFixedL)) return ()
      switch(input$distrib,
             "bern" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL, 
                                          fmtp(pbinom(as.numeric(input$xFixedL), 
                                                 size = as.numeric(1), 
                                                 prob = as.numeric(input$pBG)
                                          ))
             )),
             "bin" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                         input$xFixedL, 
                                         input$xFixedL,
                                         fmtp(pbinom(as.numeric(input$xFixedL), 
                                                size = as.numeric(input$numBinTrials), 
                                                prob = as.numeric(input$p)
                                         ))
             )),
             "dunif" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                           input$xFixedL, 
                                           input$xFixedL,
                                           fmtp(punifdisc(as.numeric(input$xFixedL), 
                                                     as.numeric(input$a), 
                                                     as.numeric(input$b)
                                           ))
             )),
             "geom" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL,
                                          fmtp(pgeom(as.numeric(input$xFixedL)-1, 
                                                prob = as.numeric(input$pBG)
                                          ))
             )),
             "hgeom" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq %.03f ) = %s$$", 
                                           input$xFixedL,
                                           input$xFixedL,
                                           fmtp(phyper(as.numeric(input$xFixedL), 
                                                  as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), 
                                                  as.numeric(input$numTrials)
                                           ))
                                           
             )),
             "nbin" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq %.03f ) = %s$$", 
                                          input$xFixedL,
                                          input$xFixedL,
                                          fmtp(pnbinom(as.numeric(input$xFixedL) - as.numeric(input$numSuccesses), 
                                                  as.numeric(input$numSuccesses), 
                                                  as.numeric(input$pNeg)
                                          ))
             )),
             "poi" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL,
                                          fmtp(ppois(as.numeric(input$xFixedL), 
                                                lambda = as.numeric(input$lambda), 
                                          ))
             )),
             
             #Continuous
             "beta" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                           input$xFixedL, 
                                           input$xFixedL,
                                           fmtp(pbeta(as.numeric(input$xFixedL), 
                                                 shape1 = as.numeric(input$alpha),
                                                 shape2 = as.numeric(input$beta),
                                           ))
             )),
             "chisq" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                            input$xFixedL, 
                                            input$xFixedL,
                                            fmtp(pchisq(as.numeric(input$xFixedL), 
                                                   df = as.numeric(input$df),
                                            ))
             )),
             "exp" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL,
                                          fmtp(pgamma(as.numeric(input$xFixedL),
                                                 shape = as.numeric(1),
                                                 scale = as.numeric(input$betaE),
                                          ))
             )),
             "f" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                        input$xFixedL, 
                                        input$xFixedL,
                                        fmtp(pf(as.numeric(input$xFixedL), 
                                           df1 = as.numeric(input$df1),
                                           df2 = as.numeric(input$df2)
                                        ))
             )),
             "gam" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL,
                                          fmtp(pgamma(as.numeric(input$xFixedL),
                                                 shape = as.numeric(input$alphaG),
                                                 scale = as.numeric(input$betaG),
                                          ))
             )),             
             "norm" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                           input$xFixedL, 
                                           input$xFixedL,
                                           fmtp(pnorm(as.numeric(input$xFixedL), 
                                                 mean = as.numeric(input$normMean),
                                                 sd = sqrt(as.numeric(input$normVar)),
                                           ))
             )),
             "t" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                        input$xFixedL, 
                                        input$xFixedL,
                                        fmtp(pt(as.numeric(input$xFixedL), 
                                           df = as.numeric(input$df),
                                        ))
             )),
             "unif" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$", 
                                          input$xFixedL, 
                                          input$xFixedL,
                                          fmtp(punif(as.numeric(input$xFixedL),
                                                min = as.numeric(input$theta1),
                                                max = as.numeric(input$theta2),
                                          ))
             )),
             # default: contSpec-based families
             { s <- contSpec(input$distrib)
               withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %s$$",
                                   input$xFixedL, input$xFixedL, fmtp(s$p(as.numeric(input$xFixedL))))) }
      )
    }
  })
  
  #Select percentile option
  output$percentileUI <- renderUI({
    if(is.null(input$outType) || is.null(input$distrib)) return ()
    if( (input$outType == "PDF") ) #& (input$distrib == "norm") )
      radioButtons("percentile", "\nCalculate:", inline = TRUE,
                   selected = restoreInput(id = "percentile", default = "pdf"),
                   c("Quantile" = "quant",
                     "PDF" = "pdf"))
  })
  
  #   #Prompt for percentile choice
  #   output$percentBox <- renderUI({
  #     if(is.null(input$percentile)) return ()
  #     if(input$outType == "PDF"){
  #       if(input$percentile == "quant"){
  #         numericInput("quantile", withMathJax('Enter a percentile (between 0 and 1):'), 0.5, 
  #                      min = 0.0, max = 1.0, step = 0.1)
  #       }
  #     }
  #   }) 
  
  #Option to calculate the xvalue corresponding to a percentile.
  output$percentCalc <- renderUI({
    req(length(inputErrors()) == 0)
    if(is.null(input$percentile)) return ()
    if(input$outType == "PDF"){
      if(input$percentile == "quant"){
        withMathJax(sprintf("\\(\\mathbb{P}(X \\le x) = \\) %.03f corresponds to an \\(x\\) value of %.04f \\( = F^{-1}(%.03f).\\)", 
                            input$quantile,
                            switch(input$distrib,
                                   "bern" = qbinom(as.numeric(input$quantile), 
                                                   size = as.numeric(1), 
                                                   prob = as.numeric(input$pBG)),
                                   "bin" = qbinom(as.numeric(input$quantile), 
                                                  size = as.numeric(input$numBinTrials), 
                                                  prob = as.numeric(input$p)),
                                   "dunif" = qunifdisc(input$quantile, 
                                                       as.numeric(input$a), 
                                                       as.numeric(input$b)),
                                   "geom" = qgeom(as.numeric(input$quantile), 
                                                  prob = as.numeric(input$pBG)) + 1,
                                   "hgeom" = qhyper(as.numeric(input$quantile), 
                                                    as.numeric(input$favBalls), 
                                                    as.numeric((input$numEvents - input$favBalls)), 
                                                    as.numeric(input$numTrials) ),
                                   "nbin" = qnbinom(as.numeric(input$quantile), 
                                                    as.numeric(input$numSuccesses), 
                                                    as.numeric(input$pNeg)) + input$numSuccesses,
                                   "poi" = qpois(as.numeric(input$quantile), 
                                                 lambda = as.numeric(input$lambda) ),
                                   #Continuous
                                   "beta" = qbeta(as.numeric(input$quantile), 
                                                  shape1 = as.numeric(input$alpha),
                                                  shape2 = as.numeric(input$beta)),
                                   "chisq" = qchisq(as.numeric(input$quantile), 
                                                    df = as.numeric(input$df)),
                                   "exp" = qgamma(as.numeric(input$quantile), 
                                                  shape = as.numeric(1),
                                                  scale = as.numeric(input$betaE)),
                                   "f" = qf(as.numeric(input$quantile), 
                                            df1 = as.numeric(input$df1),
                                            df2 = as.numeric(input$df2)),
                                   "gam" = qgamma(as.numeric(input$quantile), 
                                                  shape = as.numeric(input$alphaG),
                                                  scale = as.numeric(input$betaG)),
                                   "norm" = qnorm(as.numeric(input$quantile), 
                                                  mean = as.numeric(input$normMean),
                                                  sd = sqrt(as.numeric(input$normVar))),
                                   "t" = qt(as.numeric(input$quantile), 
                                            df = as.numeric(input$df)),
                                   "unif" = qunif(as.numeric(input$quantile), 
                                                  min = as.numeric(input$theta1),
                                                  max = as.numeric(input$theta2)),
                                   contSpec(input$distrib)$q(as.numeric(input$quantile))
                            ),
                            input$quantile
        )) 
      }
    }
  })
  
  #Output results of probability calculations depending on probType selection
  output$probCalc <- renderUI({
    req(length(inputErrors()) == 0)
    if(is.null(input$distrib) || is.null(input$probType)) return ()
    
    if(input$outType == "Probability"){
      if(input$probType == "between" && input$distrib %in% discreteDists ){
        #input$distType == "Discrete"){
        #if(is.null(input$x1) || is.null(input$x2)) return ()
        
        withMathJax(sprintf("The probability that \\(X\\) is between %.03f and %.03f (inclusive) is $$\\sum\\limits_{i \\, = \\, %.03f}^{%.03f} \\mathbb{P}(X = i) = \\mathbb{P}(X \\leq %.03f) - \\mathbb{P}(X \\leq %.03f) = %s$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x2,
                            input$x1 - 1, 
                            fmtp(if(input$x2 <= input$x1) {0}
                            else{
                              switch(input$distrib,
                                     "bern" = pbinom(as.numeric(input$x2), size = as.numeric(1), prob = as.numeric(input$pBG)) 
                                     - pbinom(as.numeric(input$x1) - 1, size = as.numeric(1), prob = as.numeric(input$pBG)),
                                     "bin" = pbinom(as.numeric(input$x2), size = as.numeric(input$numBinTrials), 
                                                    prob = as.numeric(input$p)) 
                                     - pbinom(as.numeric(input$x1)-1, size = as.numeric(input$numBinTrials), 
                                              prob = as.numeric(input$p)),
                                     "dunif" = punifdisc(as.numeric(input$x2), as.numeric(input$a), as.numeric(input$b)) 
                                     - punifdisc(as.numeric(input$x1)-1, as.numeric(input$a), as.numeric(input$b)),
                                     "geom" = pgeom(as.numeric(input$x2)-1, prob = as.numeric(input$pBG)) 
                                     - pgeom(as.numeric(input$x1)-2, prob = as.numeric(input$pBG)),
                                     "hgeom" = phyper(as.numeric(input$x2), as.numeric(input$favBalls), 
                                                      as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) 
                                     - phyper(as.numeric(input$x1) - 1, as.numeric(input$favBalls), 
                                              as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                     "nbin" = pnbinom(as.numeric(input$x2 - input$numSuccesses), 
                                                      as.numeric(input$numSuccesses), as.numeric(input$pNeg)) 
                                     - pnbinom(as.numeric(input$x1 - input$numSuccesses) - 1, 
                                               as.numeric(input$numSuccesses), as.numeric(input$pNeg)),
                                     "poi" = ppois(as.numeric(input$x2), as.numeric(input$lambda)) 
                                     - ppois(as.numeric(input$x1) - 1, as.numeric(input$lambda))
                              )
                            })
        )
        ) 
      } else if(input$probType == "between" && !(input$distrib %in% discreteDists) #input$distType == "Continuous" 
                && !is.null(input$x1) && !is.null(input$x2)){
        #if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is between %.03f and %.03f (inclusive) is $$\\int\\limits_{%.03f}^{%.03f} f(x) \\, dx = \\mathbb{P}(X \\leq %.04f) - \\mathbb{P}(X \\leq %.04f) \\approx %s$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x2,
                            input$x1,
                            fmtp(if(input$x2 <= input$x1) {0}
                            else{
                              switch(input$distrib,
                                     "beta" = pbeta(as.numeric(input$x2), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta))
                                     - pbeta(as.numeric(input$x1), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)),
                                     "chisq" = pchisq(as.numeric(input$x2), df = as.numeric(input$df)) 
                                     - pchisq(as.numeric(input$x1), df = as.numeric(input$df)),
                                     "exp"  = pgamma(as.numeric(input$x2), shape = as.numeric(1), scale = as.numeric(input$betaE)) 
                                     - pgamma(as.numeric(input$x1), shape = as.numeric(1), scale = as.numeric(input$betaE)),
                                     "f" = pf(as.numeric(input$x2), df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)) 
                                     - pf(as.numeric(input$x1), df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                                     "gam"  = pgamma(as.numeric(input$x2), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)) 
                                     - pgamma(as.numeric(input$x1), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)),
                                     "norm" = pnorm(as.numeric(input$x2), mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))) 
                                     - pnorm(as.numeric(input$x1), mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))),
                                     "t" = pt(as.numeric(input$x2), df = as.numeric(input$df)) 
                                     - pt(as.numeric(input$x1), df = as.numeric(input$df)),
                                     "unif" = punif(as.numeric(input$x2), as.numeric(input$theta1), as.numeric(input$theta2))
                                     - punif(as.numeric(input$x1), as.numeric(input$theta1), as.numeric(input$theta2)),
                                     { s <- contSpec(input$distrib); s$p(as.numeric(input$x2)) - s$p(as.numeric(input$x1)) }
                              )
                            })
        )
        ) 
      } else if (input$probType == "lowerTail" && input$distrib %in% discreteDists){
        # input$distType == "Discrete"){
        # if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f is $$\\sum_{\\large{i \\, \\le \\, %.04f}}\\mathbb{P}(X = i) \\approx %s$$",
                            input$xFixedL,
                            input$xFixedL,
                            fmtp(switch(input$distrib,
                                   bern = pbinom(as.numeric(input$xFixedL), size = as.numeric(1), prob = as.numeric(input$pBG)),
                                   bin = pbinom(as.numeric(input$xFixedL), size = as.numeric(input$numBinTrials), prob = as.numeric(input$p)),
                                   dunif = punifdisc(as.numeric(input$xFixedL), as.numeric(input$a), as.numeric(input$b)),
                                   geom = pgeom(as.numeric(input$xFixedL)-1, prob = as.numeric(input$pBG)),
                                   hgeom = phyper(as.numeric(input$xFixedL), as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                   nbin = pnbinom(as.numeric(input$xFixedL - input$numSuccesses), 
                                                  as.numeric(input$numSuccesses), as.numeric(input$pNeg)),
                                   poi = ppois(as.numeric(input$xFixedL), as.numeric(input$lambda)),
                                   NULL
                            ))
        )
        )        
      } else if (input$probType == "lowerTail" && !(input$distrib %in% discreteDists)){ #input$distType == "Continuous"){
        #if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f is $$\\int_{-\\infty}^{%.03f} f(x) \\, dx \\approx %s$$",
                            input$xFixedL,
                            input$xFixedL,
                            fmtp(switch(input$distrib,
                                   beta = pbeta(as.numeric(input$xFixedL), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)),
                                   chisq = pchisq(as.numeric(input$xFixedL), df = as.numeric(input$df)),
                                   exp = pgamma(as.numeric(input$xFixedL), shape = as.numeric(1), scale = as.numeric(input$betaE)),
                                   f = pf(as.numeric(input$xFixedL), df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                                   gam = pgamma(as.numeric(input$xFixedL), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)),
                                   norm = pnorm(as.numeric(input$xFixedL), mean = as.numeric(input$normMean), 
                                                sd = sqrt(as.numeric(input$normVar))),
                                   t = pt(as.numeric(input$xFixedL), df = as.numeric(input$df)),
                                   unif = punif(as.numeric(input$xFixedL), as.numeric(input$theta1), as.numeric(input$theta2)),
                                   contSpec(input$distrib)$p(as.numeric(input$xFixedL))
                            ))
        )
        )        
      } else if (input$probType == "upperTail" && input$distrib %in% discreteDists){ #input$distType == "Discrete"){
        # if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is greater than or equal to %.03f is $$1 - \\left[ \\sum_{\\large{i \\, \\le \\, %.04f}}\\mathbb{P}(X = i) \\right] \\approx %s$$",
                            input$xFixedU,
                            input$xFixedU - 1,
                            fmtp(switch(input$distrib,
                                   "bern" = 1 - pbinom(as.numeric(input$xFixedU)-1, size = as.numeric(1), 
                                                       prob = as.numeric(input$pBG)),
                                   "bin" = 1 - pbinom(as.numeric(input$xFixedU)-1, size = as.numeric(input$numBinTrials), prob = as.numeric(input$p)),
                                   "dunif" = 1 - punifdisc(as.numeric(input$xFixedU)-1, as.numeric(input$a), as.numeric(input$b)),
                                   "geom" = 1 - pgeom(as.numeric(input$xFixedU)-2, prob = as.numeric(input$pBG)),
                                   "hgeom" = 1 - phyper(as.numeric(input$xFixedU)-1, as.numeric(input$favBalls), 
                                                        as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                   "nbin" = 1 - pnbinom(as.numeric(input$xFixedU - input$numSuccesses)-1, 
                                                        as.numeric(input$numSuccesses), as.numeric(input$pNeg)),
                                   "poi" = 1 - ppois(as.numeric(input$xFixedU)-1, as.numeric(input$lambda)),
                                   NULL
                            ))
        )
        )   
      } else if (input$probType == "upperTail" && !(input$distrib %in% discreteDists)){ #input$distType == "Continuous"){
        #  if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is greater than or equal to %.03f is $$1 - \\left[ \\int_{-\\infty}^{%.04f}f(x) \\, dx \\right] \\approx %s$$",
                            input$xFixedU,
                            input$xFixedU,
                            fmtp(switch(input$distrib,
                                   "beta" = 1 - pbeta(as.numeric(input$xFixedU), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)),
                                   "chisq" = 1 - pchisq(as.numeric(input$xFixedU), df = as.numeric(input$df)),
                                   "exp" = 1 - pgamma(as.numeric(input$xFixedU), shape = as.numeric(1), scale = as.numeric(input$betaE)),
                                   "f" = 1 - pf(as.numeric(input$xFixedU), df1 = as.numeric(input$df1), df2 = as.numeric(input$df2)),
                                   "gam" = 1 - pgamma(as.numeric(input$xFixedU), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)),
                                   "norm" = 1 - pnorm(as.numeric(input$xFixedU), mean = as.numeric(input$normMean), 
                                                      sd = sqrt(as.numeric(input$normVar))),
                                   "t" = 1 - pt(as.numeric(input$xFixedU), df = as.numeric(input$df)),
                                   "unif" = 1 - punif(as.numeric(input$xFixedU), as.numeric(input$theta1), as.numeric(input$theta2)),
                                   1 - contSpec(input$distrib)$p(as.numeric(input$xFixedU))
                            ))
        )
        )   
      } else if(input$probType == "extreme" && input$distrib %in% discreteDists){ #input$distType == "Discrete"){
        if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f or greater than or equal to %.03f is $$\\sum\\limits_{\\large{i \\, \\leq \\, %.03f}} \\mathbb{P}(X = i) + \\sum\\limits_{\\large{j \\, \\geq \\, %.03f}} \\mathbb{P}(X = j) = \\mathbb{P}(X \\leq %.03f) + \\mathbb{P}(X \\geq %.03f) \\approx %s$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            fmtp(switch(input$distrib,
                                   "bern" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pbinom(as.numeric(input$x2)-1, size = as.numeric(1), prob = as.numeric(input$pBG)) + pbinom(as.numeric(input$x1), size = as.numeric(1), prob = as.numeric(input$pBG))
                                   },
                                   "bin" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pbinom(as.numeric(input$x2)-1, size = as.numeric(input$numBinTrials), prob = as.numeric(input$p)) + pbinom(as.numeric(input$x1), size = as.numeric(input$numBinTrials), prob = as.numeric(input$p), lower.tail = TRUE)
                                   },
                                   "dunif" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - punifdisc(as.numeric(input$x2)-1, as.numeric(input$a), as.numeric(input$b)) + punifdisc(as.numeric(input$x1), as.numeric(input$a), as.numeric(input$b))
                                   },
                                   "geom" = if(input$x2 <= input$x1) 
                                   {1} else{ 
                                     1 - pgeom(as.numeric(input$x2)-2, prob = as.numeric(input$pBG)) + pgeom(as.numeric(input$x1)-1, prob = as.numeric(input$pBG))
                                   },
                                   "hgeom" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - phyper(as.numeric(input$x2)-1, as.numeric(input$favBalls), as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) + phyper(as.numeric(input$x1), as.numeric(input$favBalls), as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials))
                                   },
                                   "nbin" =if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pnbinom(as.numeric(input$x2 - input$numSuccesses)-1, as.numeric(input$numSuccesses), as.numeric(input$pNeg)) + pnbinom(as.numeric(input$x1 - input$numSuccesses), as.numeric(input$numSuccesses), as.numeric(input$pNeg))
                                   },
                                   "poi" = if(input$x2 <= input$x1) 
                                   {1} else{ 
                                     1 - ppois(as.numeric(input$x2)-1, as.numeric(input$lambda)) + ppois(as.numeric(input$x1), as.numeric(input$lambda))
                                   },
                                   NULL
                            ))
        )
        ) 
      } else if(input$probType == "extreme" && !(input$distrib %in% discreteDists)){ #input$distType == "Continuous"){
        if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f or greater than or equal to %.03f is $$\\int\\limits_{\\large{x \\, \\leq \\, %.03f}} f(x) \\, dx + \\int\\limits_{\\large{x \\, \\geq \\, %.03f}} f(x) \\, dx = \\mathbb{P}(X \\leq %.03f) + \\mathbb{P}(X \\geq %.03f) \\approx %s$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            fmtp(if(input$x2 <= input$x1) {1}
                            else{
                              switch(input$distrib,
                                     "beta" = 1 - pbeta(as.numeric(input$x2), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)) 
                                     + pbeta(as.numeric(input$x1), shape1 = as.numeric(input$alpha), shape2 = as.numeric(input$beta)),
                                     "chisq" = 1 - pchisq(as.numeric(input$x2), as.numeric(input$df)) 
                                     + pchisq(as.numeric(input$x1), as.numeric(input$df)),
                                     "exp" = 1 - pgamma(as.numeric(input$x2), shape = as.numeric(1), scale = as.numeric(input$betaE)) 
                                     + pgamma(as.numeric(input$x1), shape = as.numeric(1), scale = as.numeric(input$betaE)),
                                     "f" = 1 - pf(as.numeric(input$x2), as.numeric(input$df1), as.numeric(input$df2)) 
                                     + pf(as.numeric(input$x1), as.numeric(input$df1), as.numeric(input$df2)),
                                     "gam" = 1 - pgamma(as.numeric(input$x2), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)) 
                                     + pgamma(as.numeric(input$x1), shape = as.numeric(input$alphaG), scale = as.numeric(input$betaG)),
                                     "norm" = 1 - pnorm(as.numeric(input$x2), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))) 
                                     + pnorm(as.numeric(input$x1), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))),
                                     "t" = 1 - pt(as.numeric(input$x2), as.numeric(input$df)) 
                                     + pt(as.numeric(input$x1), as.numeric(input$df)),
                                     "unif" = 1 - punif(as.numeric(input$x2), as.numeric(input$theta1), as.numeric(input$theta2))
                                     + punif(as.numeric(input$x1), as.numeric(input$theta1), as.numeric(input$theta2)),
                                     { s <- contSpec(input$distrib); s$p(as.numeric(input$x1)) + (1 - s$p(as.numeric(input$x2))) }
                              )
                            })
        )
        )
      }
      
      
    } #Ends if(input$outType == "Probability"){
  }) #Ends output$probCalcBetween <- renderUI({
  
  #Calculate the mean and output results in LaTeX style
  output$meanCalc <- renderUI({
    req(length(inputErrors()) == 0)
    if(is.null(input$distrib)) return ()
    if(input$outType == "Mean"){
      switch(input$distrib,
             "dunif" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{a+b}{2} = \\frac{%.03f + %.03f}{2} = %s$$",
                                           input$a,
                                           input$b,
                                           fmtp((input$a + input$b)/2)
             )), 
             
             "bern" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = p = %s$$",
                                          fmtp(input$pBG)
             )), 
             
             "bin" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = np = (%.03f) (%.03f) = %s$$",
                                         input$numBinTrials,
                                         input$p,
                                         fmtp((input$numBinTrials * input$p))
             )), 
             
             "geom" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{1}{p} = \\frac{1}{%.03f} = %s$$",
                                          input$pBG,
                                          fmtp((1 / input$pBG))
             )),
             
             "hgeom" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{nm}{N} = \\frac{%d \\cdot %d}{%d} = %s$$",
                                           input$numTrials,
                                           input$favBalls,
                                           input$numEvents,
                                           fmtp((input$numTrials * input$favBalls / input$numEvents))
             )),
             
             "nbin" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{r}{p} = \\frac{%d}{%0.3f} = %s$$",
                                          input$numSuccesses,
                                          input$pNeg,
                                          fmtp((input$numSuccesses / input$pNeg))
             )), 
             
             "poi" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\lambda = %s$$",
                                         fmtp(input$lambda)
             )), 
             
             #Continuous
             
             "beta" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{\\alpha}{\\alpha + \\beta} = \\frac{%.03f}{%.03f + %.03f} = %s$$",
                                          input$alpha,
                                          input$alpha,
                                          input$beta,
                                          fmtp(input$alpha/(input$alpha + input$beta))
             )),
             
             "chisq" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\nu = %s$$",
                                           fmtp(input$df)
             )), 
             
             "exp" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\beta = %s$$",
                                         fmtp(input$betaE)
             )),
             
             "f" = withMathJax(sprintf("Mean (for \\( d_2 > 2 \\)) is $$\\mathbb{E}(X) = \\frac{d_2}{d_2 - 2} = \\frac{%.03f}{%.03f - 2} = %s$$",
                                       input$df2,
                                       input$df2,
                                       fmtp(input$df2/(input$df2 - 2))
             )),
             
             "gam" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\alpha \\beta = %.03f \\cdot %.03f  =  %s$$",
                                         input$alphaG,
                                         input$betaG,
                                         fmtp((input$alphaG * input$betaG))
             )),
             
             "norm" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\mu = %s$$",
                                          fmtp(input$normMean)
             )), 
             
             "t" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = 0$$"
             )), 
             
             "unif" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{\\theta_1+\\theta_2}{2} = \\frac{%.03f + %.03f}{2} = %s$$",
                                          input$theta1,
                                          input$theta2,
                                          fmtp((input$theta1 + input$theta2)/2)
             )),
             # default: contSpec-based families (undefined mean -> message)
             { s <- contSpec(input$distrib)
               if (is.null(s$mean) || !is.finite(s$mean))
                 withMathJax(sprintf("Mean of the %s distribution is undefined (does not exist).", s$name))
               else withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = %s$$", fmtp(s$mean))) }
      )
    }
  })

  #Calculate the variance and output results in LaTeX style
  output$varCalc <- renderUI({
    req(length(inputErrors()) == 0)
    if(is.null(input$distrib)) return ()
    if(input$outType == "Variance"){
      switch(input$distrib,
             "dunif" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{(b - a +1)^2 - 1}{12} = \\frac{(%.03f - %.03f + 1)^2 - 1}{12} = %s$$",
                                           input$b,
                                           input$a,
                                           fmtp(((input$b - input$a + 1)^2 -1)/12)
             )),
             
             "bern" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = p(1-p) = (%.03f)(1-%.03f) = %s$$",
                                          input$pBG,
                                          input$pBG,
                                          fmtp((input$pBG*(1-input$pBG)))
             )), 
             
             "bin" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = np(1-p) = (%d)(%.03f)(1-%.03f) = %s$$",
                                         input$numBinTrials,
                                         input$p,
                                         input$p,
                                         fmtp((input$numBinTrials * input$p * (1-input$p)))
             )), 
             
             "geom" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{1-p}{p^2} = \\frac{1-%.03f}{(%.03f)^2} = %s$$",
                                          input$pBG,
                                          input$pBG,
                                          fmtp(((1-input$pBG) / (input$pBG)^2))
             )),
             
             "hgeom" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{N-n}{N-1}\\left(\\frac{nm}{N}\\right)\\left( 1-\\frac{m}{N} \\right) = \\frac{%d-%d}{%d-1}\\left(\\frac{%d \\cdot %d}{%d}\\right)\\left( 1-\\frac{%d}{%d} \\right)  = %s$$",
                                           input$numEvents,
                                           input$numTrials,
                                           input$numEvents,
                                           input$numTrials,
                                           input$favBalls,
                                           input$numEvents,
                                           input$favBalls,
                                           input$numEvents,
                                           fmtp(((input$numEvents - input$numTrials)/(input$numEvents-1)) * ((input$numTrials * input$favBalls)/(input$numEvents))*(1- input$favBalls/(input$numEvents)))
             )),
             
             "nbin" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{r(1-p)}{p^2} = \\frac{%d(1-%.03f)}{(%.03f)^2} = %s$$",
                                          input$numSuccesses,
                                          input$pNeg,
                                          input$pNeg,
                                          fmtp(( (input$numSuccesses * (1-input$pNeg) ) / ( (input$pNeg)^2 ) ))
             )),
             
             "poi" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\lambda = %s$$",
                                         fmtp(input$lambda)
             )), 
             
             #Continuous
             
             "beta" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta + 1)}
                                          = \\frac{%.03f \\cdot %.03f}{(%.03f + %.03f)^2(%.03f + %.03f + 1)} = %s$$",
                                          input$alpha,
                                          input$beta,
                                          input$alpha,
                                          input$beta,
                                          input$alpha,
                                          input$beta,
                                          fmtp((input$alpha * input$beta)/((input$alpha + input$beta)^2 * (input$alpha + input$beta + 1)))
             )),
             
             "chisq" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = 2\\nu = 2 * %.03f = %s$$",
                                           input$df,
                                           fmtp(2 * input$df)
             )), 
             
             "exp" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\beta^2 = %s$$",
                                         fmtp(input$betaE^2)
             )),
             
             "f" = withMathJax(sprintf("Variance (for \\( d_2 > 4\\) ) is $$\\mathbb{V}(X) = \\frac{2 {d_2}^2 (d_1 + d_2 - 2)}{d_1 (d_2 - 2)^2 (d_2 - 4)}
                                       = \\frac{2 * %.02f^2 (%.02f + %.02f - 2)}{%.02f (%.02f - 2)^2 (%.02f - 4)} = %s$$",
                                       input$df2,
                                       input$df1,
                                       input$df2,
                                       input$df1,
                                       input$df2,
                                       input$df2,
                                       fmtp((2 * input$df2^2  * (input$df1 + input$df2 - 2))/(input$df1 * (input$df2 - 2)^2 * (input$df2 - 4)))
             )), 
             
             "gam" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\alpha \\beta^2 = %d (%d)^2  =  %s$$",
                                         input$alphaG,
                                         input$betaG,
                                         fmtp((input$alphaG * input$betaG^2))
             )),
             
             "norm" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\sigma^2 = %s$$",
                                          fmtp(input$normVar)
             )), 
             
             "t" = withMathJax(sprintf("Variance (for \\( \\nu > 2\\) ) is $$\\mathbb{V}(X) = \\frac{\\nu}{\\nu - 2} = \\frac{%.03f}{%.03f - 2} = %s$$",
                                       input$df,
                                       input$df,
                                       fmtp(input$df/(input$df-2))
             )), 
             
             "unif" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{(\\theta_2-\\theta_1)^2}{12} = \\frac{(%d - %d)^2}{12} = %s$$",
                                          input$theta2,
                                          input$theta1,
                                          fmtp((input$theta2 - input$theta1)^2 /12)
             )),
             # default: contSpec-based families (undefined variance -> message)
             { s <- contSpec(input$distrib)
               if (is.null(s$var) || !is.finite(s$var))
                 withMathJax(sprintf("Variance of the %s distribution is undefined (does not exist).", s$name))
               else withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = %s$$", fmtp(s$var))) }
      )
    }

  })
  
  #Close
}

# Returning the function as the last expression is the modern equivalent of
# shinyServer(appServer) for a ui.R/server.R app.
appServer









