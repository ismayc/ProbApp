# User-interface definition of the Calculator for Probability Distributions
# Shiny app. Modernized with bslib (Bootstrap 5): all controls live in the
# sidebar; results (formula, plot, calculation) are shown as cards.

library(shiny)
library(ggplot2)
library(bslib)
library(plotly)

# Clean academic theme: Inter type, calm teal accent, light/dark capable.
prob_theme <- bs_theme(
  version = 5,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  primary = "#0d9488",
  # Links/accent text use a darker teal that meets WCAG AA (>=4.5:1) on white;
  # the lighter primary is kept for UI components (buttons/radios), which only
  # need 3:1. (Plot-bar palette contrast is revisited in the Phase 2 plot rework.)
  "link-color" = "#0f766e",
  "border-radius" = "0.6rem"
)

# UI is a function of `request` so Shiny can restore bookmarked state from the URL.
function(request) page_sidebar(
  theme = prob_theme,
  title = div(
    # flex-wrap lets the control group drop below the title on narrow (phone)
    # screens instead of overflowing the header; gap-2 keeps a gap once wrapped.
    class = "d-flex justify-content-between align-items-center flex-wrap gap-2 w-100 app-title-bar",
    span("Calculator for Probability Distributions"),
    div(
      class = "d-flex align-items-center gap-3 app-title-controls",
      bookmarkButton(label = "Share link", title = "Capture this configuration in a shareable URL"),
      actionLink("about", "Help / About"),
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  ),
  window_title = "Calculator for Probability Distributions by Chester Ismay and Logan Soich",

  # ------------------------------------------------------------------ Sidebar
  sidebar = sidebar(
    width = 330,
    title = "Controls",

    actionButton("reset", "Reset to defaults",
                 class = "btn-outline-secondary btn-sm mb-2"),

    radioButtons("distType",
                 strong("Distribution Type:"),
                 list("Discrete", "Continuous", "CUSTOM"),
                 selected = "Discrete"),

    uiOutput("distName"),

    radioButtons("outType",
                 strong("Output Type:"),
                 list("Formulas", "PDF/Quantile" = "PDF",
                      "CDF", "Probability", "Mean", "Variance"),
                 selected = "Probability"),

    # PDF vs Quantile choice (rendered by server, shown only for PDF output)
    uiOutput("percentileUI"),

    # Probability sub-type (rendered by server, shown only for Probability)
    uiOutput("probTypeSelect"),

    # --- Parameters -------------------------------------------------------
    # Hidden for CUSTOM under Mean/Variance: the custom distribution's inputs
    # live in the card above the plot, so the sidebar has nothing to head there.
    conditionalPanel(condition = "input.outType != 'Formulas' && !(input.distType == 'CUSTOM' && (input.outType == 'Mean' || input.outType == 'Variance'))",
                     strong("Parameters:")),

    conditionalPanel(condition = "input.outType != 'Formulas' & (input.distrib == 'bern' || input.distrib == 'geom')",
                     numericInput("pBG", withMathJax('Enter the probability of success (\\(p\\)):'),
                                  0.5, step=0.1, min=0, max=1)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas'  && input.distrib == 'bin'",
                     numericInput("p", withMathJax('Enter the probability of success (\\(p\\)):'),
                                  0.5, step=0.1, min=0, max=1),
                     numericInput("numBinTrials", withMathJax('Enter the number of trials (\\(n\\)):'), 15,
                                  step = 1, min=1)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'dunif'",
                     numericInput("a", withMathJax('Enter the lower bound (\\(a\\)):'), 1),
                     numericInput("b", withMathJax('Enter the lower bound (\\(b\\)):'), 6)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'hgeom'",
                     numericInput("numEvents", withMathJax('Enter the total number of events (\\(N\\)) i.e. total number of balls:'), 10,
                                  step=1, min=1),
                     numericInput("numTrials",
                                  withMathJax('Enter the number of trials (\\(n\\)) where \\(n < N \\) i.e. number of balls chosen:'), 5,
                                  step=1, min=0),
                     numericInput("favBalls",
                                  withMathJax('Enter the total number of favorable events (\\(m\\)) where \\(m < N \\) i.e. total number of white balls:'), 3,
                                  step=1, min=0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas'  && input.distrib == 'nbin'",
                     numericInput("pNeg", withMathJax('Enter the probability of success (\\(p\\)):'),
                                  0.5, step=0.1, min=0, max=1),
                     numericInput("numSuccesses", withMathJax('Enter the number of successes (\\(r\\)):'), 2,
                                  step = 1, min=1)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'poi'",
                     numericInput("lambda", withMathJax('Enter the rate parameter (\\(\\lambda\\)):'), 4,
                                  step = 1, min = 0)
    ),

    # Continuous
    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'beta'",
                     numericInput("beta", withMathJax('Enter the shape parameter of the distribution (\\(\\beta\\)):'), 5.0,
                                  min = 0),
                     numericInput("alpha", withMathJax('Enter the shape parameter of the distribution (\\(\\alpha\\)):'), 2.0,
                                  min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'gam'",
                     numericInput("betaG", withMathJax('Enter the scale parameter of the distribution (\\(\\beta\\)):'), 6.0,
                                  min = 0),
                     numericInput("alphaG", withMathJax('Enter the shape parameter of the distribution (\\(\\alpha\\)):'), 3.0,
                                  min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && (input.distrib == 'chisq' || input.distrib == 't')",
                     numericInput("df", withMathJax('Enter the degrees of freedom (\\(\\nu\\)):'), 10.0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'exp'",
                     numericInput("betaE", withMathJax('Enter the scale parameter of the distribution (\\(\\beta\\)):'), 5.0,
                                  min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'f'",
                     numericInput("df1", withMathJax('Enter the numerator degrees of freedom (\\( d_1 \\)) :'), 5.0),
                     numericInput("df2", withMathJax('Enter the denominator degrees of freedom (\\( d_2 \\)):'), 10.0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'norm'",
                     numericInput("normMean", withMathJax('Enter the mean of the distribution (\\(\\mu\\)):'), 0.0),
                     numericInput("normVar", withMathJax('Enter the variance of the distribution (\\(\\sigma^2\\)):'), 1.0,
                                  min=0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'unif'",
                     numericInput("theta1", withMathJax('Enter the lower bound of the distribution (\\(\\theta_1\\)):'), 0.0),
                     numericInput("theta2", withMathJax('Enter the upper bound of the distribution (\\(\\theta_2\\)):'), 5.0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'weib'",
                     numericInput("weibShape", withMathJax('Enter the shape parameter (\\(k\\)):'), 2.0, min = 0),
                     numericInput("weibScale", withMathJax('Enter the scale parameter (\\(\\lambda\\)):'), 1.0, min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'lnorm'",
                     numericInput("lnMeanlog", withMathJax('Enter the mean of the log (\\(\\mu\\)):'), 0.0),
                     numericInput("lnSdlog", withMathJax('Enter the standard deviation of the log (\\(\\sigma\\)):'), 1.0, min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'cauchy'",
                     numericInput("cauchyLoc", withMathJax('Enter the location parameter (\\(x_0\\)):'), 0.0),
                     numericInput("cauchyScale", withMathJax('Enter the scale parameter (\\(\\gamma\\)):'), 1.0, min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'logis'",
                     numericInput("logisLoc", withMathJax('Enter the location parameter (\\(\\mu\\)):'), 0.0),
                     numericInput("logisScale", withMathJax('Enter the scale parameter (\\(s\\)):'), 1.0, min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'pareto'",
                     numericInput("paretoScale", withMathJax('Enter the scale parameter (minimum \\(x_m\\)):'), 1.0, min = 0),
                     numericInput("paretoShape", withMathJax('Enter the shape parameter (\\(\\alpha\\)):'), 3.0, min = 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.distrib == 'laplace'",
                     numericInput("laplaceLoc", withMathJax('Enter the location parameter (\\(\\mu\\)):'), 0.0),
                     numericInput("laplaceScale", withMathJax('Enter the scale parameter (\\(b\\)):'), 1.0, min = 0)
    ),

    # (The Custom distribution's density/support/LaTeX inputs live in a roomier
    # card above the plot — see the main area below — rather than this narrow
    # sidebar.)

    # --- Input values (x) -------------------------------------------------
    conditionalPanel(condition = "input.outType != 'Formulas' && (input.outType == 'PDF') && input.percentile != 'quant'",
                     numericInput("xFixedPC", withMathJax('Enter a discrete value (\\(x\\)):'), 1.0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'PDF' && input.percentile == 'quant'",
                     numericInput("quantile", withMathJax('Enter a percentile (between 0 and 1):'), 0.5,
                                  min = 0.0, max = 1.0, step = 0.1)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'Probability'
                     && (input.probType == 'between' || input.probType == 'extreme')",
                     numericInput("x1", withMathJax('Enter lower value (\\(x_1\\)):'), 0),
                     numericInput("x2", withMathJax('Enter upper value (\\(x_2\\)):'), 1)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && ((input.outType == 'Probability' && input.probType == 'lowerTail')
                     || input.outType == 'CDF')",
                     numericInput("xFixedL", withMathJax('Enter a discrete value (\\(x\\)):'), 0)
    ),

    conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'Probability'
                     && input.probType == 'upperTail'",
                     numericInput("xFixedU", withMathJax('Enter a discrete value (\\(x\\)):'), 1.0)
    )
  ),

  # --------------------------------------------------------------- Main area
  withMathJax(),

  # Mobile-browser tweaks (phones, <=575.98px = Bootstrap's `xs`). The sidebar
  # itself already collapses to a toggleable overlay on small screens via bslib;
  # these rules fix the header crowding and the fixed-height custom card, which
  # do not adapt on their own.
  tags$head(tags$style(HTML("
    @media (max-width: 575.98px) {
      /* Smaller, tighter header so the long app title fits and wraps cleanly */
      .app-title-bar { font-size: 0.95rem; line-height: 1.2; }
      .app-title-controls { font-size: 0.85rem; gap: 0.75rem !important; }
      /* The custom-distribution card's controls wrap onto more lines on a
         phone; let it grow to fit instead of clipping at the desktop 400px. */
      .custom-dist-card, .custom-dist-card .card-body { height: auto !important; min-height: 0 !important; }
    }
  "))),

  div(class = "text-muted small mb-2",
      "Developed by Dr. Chester Ismay (",
      a("chester.ismay@gmail.com", href = "mailto:chester.ismay@gmail.com"),
      ") and Logan Soich (",
      a("soichlogan@gmail.com", href = "mailto:soichlogan@gmail.com"), ")"
  ),

  # Validation alert (e.g. a non-integer entered for a count parameter)
  uiOutput("inputError"),

  # Custom-distribution definition — shown above the results whenever the Custom
  # distribution is selected. Its inputs (expression, support, LaTeX) get more
  # room here than in the narrow sidebar. Same input ids; only the location moved.
  conditionalPanel(
    condition = "input.distrib == 'custom'",
    card(
      # In the fillable main area this card does not auto-grow to its content,
      # so we give it an explicit height tall enough to show every control plus
      # the help link (fill = FALSE keeps it from stretching further). On phones
      # a media rule (see tags$head above) overrides this to auto so the wrapped
      # controls aren't clipped — `custom-dist-card` is that rule's hook.
      class = "custom-dist-card",
      fill = FALSE,
      min_height = "400px",
      height = "400px",
      card_header("Custom distribution"),
      card_body(
        fillable = FALSE,
        radioButtons("customMode", "Enter the density as:",
                     c("R expression" = "expr", "LaTeX" = "latex"),
                     selected = "expr", inline = TRUE),
        div(
          class = "d-flex flex-wrap align-items-end gap-3",
          # The density field's label changes with the input mode (see the
          # observer in server.R), so it reads "in x" for R and "as LaTeX".
          div(style = "flex: 4 1 320px;",
              textInput("customExpr", "Density f(x) in x:", value = "exp(-x)", width = "100%")),
          div(style = "flex: 1 1 130px;",
              numericInput("customLo", withMathJax('Lower \\(\\ell\\):'), 0.0, width = "100%")),
          div(style = "flex: 1 1 130px;",
              numericInput("customHi", withMathJax('Upper \\(u\\):'), 5.0, width = "100%"))
        ),
        textInput("customMasses",
                  HTML('Point masses &mdash; <code>location:weight</code>, comma-separated (optional):'),
                  value = "", width = "100%",
                  placeholder = "e.g.  2:0.3, 5:0.2"),
        # Full syntax / point-mass guidance lives in a modal (keeps the card
        # compact); the link opens it and the help expands across the window.
        actionLink("customHelp", "Syntax & point-mass help", class = "small")
      )
    )
  ),

  # Formula card (shown for the Formulas output type)
  conditionalPanel(
    condition = "input.outType == 'Formulas'",
    card(
      card_header("Formula"),
      # fillable = FALSE -> normal block flow so MathJax text/inline math reads
      # naturally instead of being laid out as flex items
      card_body(uiOutput("formulas"), fillable = FALSE)
    )
  ),

  # Plot card (shown for PDF / CDF / Probability output types)
  conditionalPanel(
    condition = "input.outType == 'PDF' || input.outType == 'CDF' || input.outType == 'Probability'",
    card(
      card_header("Plot"),
      card_body(plotlyOutput("distribPlot", height = "460px"))
    )
  ),

  # Result card (shown for every output type except Formulas)
  conditionalPanel(
    condition = "input.outType != 'Formulas'",
    card(
      card_header("Result"),
      # fillable = FALSE -> normal block flow: empty outputs collapse (no extra
      # vertical gaps) and the inline \(X\) stays inline instead of wrapping
      card_body(
        fillable = FALSE,
        uiOutput("meanCalc"),
        uiOutput("varCalc"),
        uiOutput("percentCalc"),
        uiOutput("distribCalc"),
        uiOutput("probCalc")
      )
    )
  )
)
