# Plotting helpers and small distribution utilities for the
# Calculator for Probability Distributions Shiny app.

# ---------------------------------------------------------------------------
# Shared modern plot styling
# ---------------------------------------------------------------------------
# A single palette + theme keeps every plot consistent with the app's
# "clean academic" look. HL is the teal accent used to highlight the value /
# region of interest; BASE is the muted bar fill; LINE is the density/CDF curve.
prob_hl   <- "#0d9488"  # teal accent (highlighted bars / shaded area)
prob_base <- "#94a3b8"  # muted slate (non-highlighted bars)
prob_line <- "#64748b"  # slate (density / CDF curve) — reads on light AND dark

theme_prob <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      # transparent backgrounds so the plot blends into its (light or dark) card
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.title       = element_text(face = "bold", hjust = 0.5,
                                       margin = margin(b = 8)),
      plot.title.position = "plot",
      axis.title       = element_text(face = "bold"),
      axis.title.x     = element_text(margin = margin(t = 6)),
      axis.title.y     = element_text(margin = margin(r = 6)),
      panel.grid.minor = element_blank(),
      plot.margin      = margin(12, 16, 10, 12)
    )
}

# Format a probability/result value for display inside MathJax. Very small
# nonzero magnitudes (which would otherwise round to "0.0000") are shown in
# scientific notation as LaTeX (e.g. 1.234 \times 10^{-6}); everything else uses
# fixed 4 decimals. Returns a LaTeX-ready string.
fmtp <- function(x) {
  if (length(x) != 1 || !is.finite(x)) return(sprintf("%.4f", x))
  if (x != 0 && abs(x) < 1e-4) {
    e <- floor(log10(abs(x)))
    m <- x / 10^e
    sprintf("%.3f \\times 10^{%d}", m, e)
  } else {
    sprintf("%.4f", x)
  }
}

# Common x-axis breaks used by all of the continuous shading plots.
prob_breaks <- function(limits) {
  if (limits[2] - limits[1] <= 15)
    seq(ceiling(limits[1]), ceiling(limits[2]), 1)
  else
    seq(ceiling(limits[1]), ceiling(limits[2]),
        ceiling((limits[2] - limits[1]) / 15))
}

# Drop-in replacement for the single way the app used ggplot2::qplot():
#   qplot(factor(range), y, xlab=, ylab=, main=, geom="bar", stat="identity", fill=)
# qplot()'s `stat`/`geom` arguments became defunct in ggplot2 4.0, so this
# reimplements that exact bar use-case with geom_col() and the shared theme.
# The discrete plot call sites in server.R then keep their fill logic verbatim,
# appending their own scale_fill_manual()/guides() exactly as before.
# `fill` (a logical vector) highlights the bars in the region of interest.
qplot <- function(x, y, xlab = "", ylab = "", main = "",
                  geom = "bar", stat = "identity", fill = NULL) {
  if (is.null(fill)) fill <- FALSE
  d <- data.frame(x = x, y = y, fill = fill,
                  tt = sprintf("ℙ(X = %s) = %.4f", as.character(x), y))
  ggplot(d, aes(x = x, y = y, fill = fill, text = tt)) +
    geom_col(width = 0.85) +
    labs(x = xlab, y = ylab, title = sub("\\s*\\n$", "", main)) +
    theme_prob()
}

# ---------------------------------------------------------------------------
# Discrete uniform distribution functions
# ---------------------------------------------------------------------------
dunifdisc <- function(x, min=0, max=1) ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
punifdisc <- function(q, min=0, max=1) ifelse(q<min, 0, ifelse(q>max, 1, floor(q)/(max-min+1)))
qunifdisc <- function(p, min=0, max=1) floor(p*(max-min+1))
runifdisc <- function(n, min=0, max=1) sample(min:max, n, replace=T)

# ---------------------------------------------------------------------------
# Discrete PMF / CDF bar plot (highlights the bar at `inputValue`)
# ---------------------------------------------------------------------------
distribPlot <- function(func = dbinom,
                        range = 0:1,
                        args = c(1, 0.5),
                        inputValue = 0,
                        xlabel = "Number of Successes",
                        distribName = "Bernoulli",
                        numArgs = 2,
                        paramAdjust = 0,
                        plotType = "",
                        mainLabel = "Probability Mass Function"
){
  if(is.null(inputValue)) return ()
  if(numArgs == 1 && is.null(args[1])) return ()
  else if(numArgs == 2 && (is.null(args[1]) || is.null(args[2]))) return ()
  else if(numArgs == 3 && (is.null(args[1]) || is.null(args[2])) || is.null(args[3])) return ()

  yvals <- if (numArgs == 1)
             func(range - paramAdjust, args[1])
           else if (numArgs == 2)
             func(range - paramAdjust, args[1], args[2])
           else
             func(range - paramAdjust, args[1], args[2], args[3])

  # Hover tooltip text shown on each bar (interactive via ggiraph). Uses the
  # blackboard-bold ℙ (U+2119) to match the \mathbb{P} shown in the Result card.
  tt <- if (identical(plotType, "Cumulative"))
          sprintf("ℙ(X ≤ %s) = %.4f", range, yvals)
        else
          sprintf("ℙ(X = %s) = %.4f", range, yvals)
  df <- data.frame(x = factor(range), y = yvals, hl = range == inputValue, tt = tt)

  # `text` carries the hover tooltip through to plotly (ggplotly(tooltip = "text"))
  ggplot(df, aes(x = x, y = y, fill = hl, text = tt)) +
    geom_col(width = 0.85) +
    scale_fill_manual(values = c(`FALSE` = prob_base, `TRUE` = prob_hl)) +
    labs(x = xlabel,
         y = paste(plotType, "Probability"),
         title = paste(distribName, mainLabel)) +
    guides(fill = "none") +
    theme_prob()
}

# ---------------------------------------------------------------------------
# Continuous shading plots
# Each draws the density / CDF curve and shades the region of interest.
# ---------------------------------------------------------------------------

#Chi-square
chisq_prob_area_plot <- function(lb, ub, df=10, limits = c(0, qchisq(0.999, df)), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 1000)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 1000)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(0, xmin, length.out = 1000)
    areax2 <- seq(xmax, qchisq(0.999, df), length.out = 1000)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dchisq(areax1, df = df))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dchisq(areax2, df = df))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Chi-Square Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dchisq(x, df = df)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

chisq_prob_CDF_plot <- function(lb, ub, df = 10, limits = c(0, qchisq(0.999, df))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pchisq(areax, df = df))

  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Chi-Square Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pchisq(x, df = df)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#F
f_prob_area_plot <- function(lb, ub, df1=5, df2=10, limits = c(0, qf(0.99, df1, df2)), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2]) || is.null(df1) || is.null(df2)) return ()
  x <- seq(limits[1], limits[2], length.out = 1000)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 1000)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(0, xmin, length.out = 1000)
    areax2 <- seq(xmax, qf(0.99, df1, df2), length.out = 1000)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = df(areax1, df1 = df1, df2 = df2))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = df(areax2, df1 = df1, df2 = df2))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("F Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = df(x, df1 = df1, df2 = df2)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

f_prob_CDF_plot <- function(lb, ub, df1 = 5, df2 = 10, limits = c(0, qf(0.99, df1, df2))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pf(areax, df1 = df1, df2 = df2))

  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("F Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pf(x, df1 = df1, df2 = df2)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Normal (shading adapted from https://gist.github.com/jrnold/6799152)
normal_prob_area_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(mean - 4 * sd), xmin, length.out = 100)
    areax2 <- seq(xmax, ceiling(mean + 4 * sd), length.out = 100)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dnorm(areax1, mean = mean, sd = sd))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dnorm(areax2, mean = mean, sd = sd))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Normal Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

normal_prob_CDF_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd)){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pnorm(areax, mean = mean, sd = sd))

  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Normal Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pnorm(x, mean = mean, sd = sd)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Student's t
t_prob_area_plot <- function(lb, ub, df=10, limits = c(qt(0.001, df), qt(0.999, df)), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 1000)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 1000)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(qt(0.001, df), xmin, length.out = 1000)
    areax2 <- seq(xmax, qt(0.999, df), length.out = 1000)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dt(areax1, df = df))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dt(areax2, df = df))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Student's t Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dt(x, df = df)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

t_prob_CDF_plot <- function(lb, ub, df = 10, limits = c(qt(0.001, df), qt(0.999, df))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pt(areax, df = df))

  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Student's t Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pt(x, df = df)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Uniform Distribution
uniform_prob_area_plot <- function(lb, ub, min, max, limits = c(min, max), extreme = FALSE) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(min - 1), xmin, length.out = 100)
    areax2 <- seq(xmax, ceiling(max + 1), length.out = 100)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dunif(areax1, min = min, max = max))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dunif(areax2, min = min, max = max))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Uniform Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dunif(x, min = min, max = max)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

uniform_prob_CDF_plot <- function(lb, ub = max + 1, min, max, limits = c(min - 1, max + 1)) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = punif(areax, min = min, max = max))
  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Uniform Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = punif(x, min = min, max = max)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Exponential Distribution
exp_prob_area_plot <- function(lb, ub, shape = 1, scale = 1, limits = c(0, qgamma(0.999, shape=1, scale=scale)), extreme = FALSE) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(0), xmin, length.out = 100)
    areax2 <- seq(xmax, ceiling(qgamma(0.999, shape=1, scale=scale)), length.out = 100)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dgamma(areax1, shape = 1, scale = scale))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dgamma(areax2, shape = 1, scale = scale))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Exponential Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dgamma(x, shape = 1, scale = scale)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

exp_prob_CDF_plot <- function(lb, ub = qgamma(0.999, shape=1, scale=scale), shape = 1, scale = 1, limits = c(0, qgamma(0.999, shape=1, scale=scale))) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pgamma(areax, shape = shape, scale = scale))
  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Exponential Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pgamma(x, shape = shape, scale = scale)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Gamma Distribution
gamma_prob_area_plot <- function(lb, ub, shape, scale, limits = c(0, qgamma(0.999, shape=shape, scale=scale)), extreme = FALSE) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(0), xmin, length.out = 100)
    areax2 <- seq(xmax, ceiling(qgamma(0.999, shape = shape, scale = scale)), length.out = 100)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dgamma(areax1, shape = shape, scale = scale))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dgamma(areax2, shape = shape, scale = scale))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Gamma Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dgamma(x, shape = shape, scale = scale)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

gamma_prob_CDF_plot <- function(lb, ub = qgamma(0.999, shape=shape, scale=scale), shape = 1, scale = 1,
                                limits = c(0, qgamma(0.999, shape=shape, scale=scale))) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pgamma(areax, shape = shape, scale = scale))
  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Gamma Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pgamma(x, shape = shape, scale = scale)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

#Beta Distribution
beta_prob_area_plot <- function(lb, ub, shape1, shape2, limits = c(0, 1), extreme = FALSE) {
  if(is.null(limits[1]) || is.null(limits[2]) || is.null(shape1) || is.null(shape2)) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(0, xmin, length.out = 100)
    areax2 <- seq(xmax, 1, length.out = 100)
  }

  area1 <- data.frame(x = areax1, ymin = 0, ymax = dbeta(areax1, shape1 = shape1, shape2 = shape2))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dbeta(areax2, shape1 = shape1, shape2 = shape2))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle("Beta Probability Density Function") +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dbeta(x, shape1 = shape1, shape2 = shape2)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

beta_prob_CDF_plot <- function(lb, ub, shape1, shape2, limits = c(0, 1)) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pbeta(areax, shape1 = shape1, shape2 = shape2))
  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle("Beta Cumulative Distribution Function") +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pbeta(x, shape1 = shape1, shape2 = shape2)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

# ---------------------------------------------------------------------------
# Custom distribution functions for families not in base R
# ---------------------------------------------------------------------------
# Pareto (scale xm > 0 is the minimum; shape a > 0)
dpareto <- function(x, xm, a) ifelse(x >= xm, a * xm^a / x^(a + 1), 0)
ppareto <- function(q, xm, a) ifelse(q >= xm, 1 - (xm / q)^a, 0)
qpareto <- function(p, xm, a) xm / (1 - p)^(1 / a)

# Laplace / double-exponential (location m, scale b > 0)
dlaplace <- function(x, m, b) exp(-abs(x - m) / b) / (2 * b)
plaplace <- function(q, m, b) ifelse(q < m, 0.5 * exp((q - m) / b), 1 - 0.5 * exp(-(q - m) / b))
qlaplace <- function(p, m, b) ifelse(p < 0.5, m + b * log(2 * p), m - b * log(2 * (1 - p)))

# ---------------------------------------------------------------------------
# Generic continuous shading plots: pass a density function `dfun` (area plot)
# or a CDF function `pfun` (CDF plot), already parameterized, plus the x window
# `limits` and a title. Used by the distributions added in functions.R that do
# not have a bespoke helper above.
# ---------------------------------------------------------------------------
cont_area_plot <- function(lb, ub, dfun, limits, title, extreme = FALSE) {
  if (is.null(limits[1]) || is.null(limits[2]) || !is.finite(limits[1]) || !is.finite(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 500)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if (extreme == FALSE) {
    areax1 <- seq(xmin, xmax, length.out = 500)
    areax2 <- 0
  } else {
    areax1 <- seq(limits[1], xmin, length.out = 500)
    areax2 <- seq(xmax, limits[2], length.out = 500)
  }
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dfun(areax1))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dfun(areax2))
  ggplot() +
    xlab("x") +
    ylab("Density") +
    ggtitle(title) +
    geom_ribbon(data = area1, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_ribbon(data = area2, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = dfun(x)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

cont_cdf_plot <- function(lb, ub, pfun, limits, title) {
  if (is.null(limits[1]) || is.null(limits[2]) || !is.finite(limits[1]) || !is.finite(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 200)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 200)
  area <- data.frame(x = areax, ymin = 0, ymax = pfun(areax))
  ggplot() +
    xlab("x") +
    ylab("Cumulative Probability") +
    ggtitle(title) +
    geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax),
                fill = prob_hl, alpha = 0.85) +
    geom_line(data.frame(x = x, y = pfun(x)),
              mapping = aes(x = x, y = y), color = prob_line, linewidth = 0.9) +
    scale_x_continuous(breaks = prob_breaks(limits)) +
    coord_cartesian(xlim = limits) +
    theme_prob()
}

############################################################################
# Custom user-defined distribution (sandboxed) + numeric engine            #
############################################################################
# A user supplies an (unnormalized) density expression f(x) in the variable
# x and a finite support [lo, hi]. We compute everything numerically:
# normalization constant, CDF, mean, variance and quantiles. The expression
# is evaluated in a locked-down sandbox so a hostile string cannot reach the
# file system, the network or arbitrary R — only the whitelisted elementary
# math below is permitted. PUBLIC DEPLOYMENTS MUST KEEP THIS SANDBOX.

# Whitelisted call names (operators, grouping, elementary math, and a few
# standard density functions for convenience). Anything not listed is rejected.
.custom_allowed <- c(
  "+", "-", "*", "/", "^", "(",
  "sqrt", "exp", "log", "log10", "log2", "sin", "cos", "tan",
  "asin", "acos", "atan", "sinh", "cosh", "tanh", "abs",
  "gamma", "lgamma", "beta", "lbeta", "factorial", "choose",
  "floor", "ceiling", "max", "min", "pmax", "pmin",
  # Piecewise support: the vectorized conditional plus comparison/logical
  # operators, so densities like ifelse(x < 1, x, 2 - x) or
  # (x >= 0) * (x < 1) * 2 can be expressed. All are pure base primitives.
  "ifelse", "<", ">", "<=", ">=", "==", "!=", "&", "|", "!",
  "dnorm", "dgamma", "dbeta", "dexp", "dunif", "dlnorm",
  "dt", "dchisq", "df", "dcauchy", "dlogis", "dweibull"
)

# Evaluation environment: only the whitelisted functions plus the symbols x and
# pi resolve here. Its parent is emptyenv(), so a name that somehow slipped past
# the static check still cannot fall through to base/global (where system(),
# file(), eval() live). This is the second of two independent safety layers.
.custom_env <- local({
  e <- new.env(parent = emptyenv())
  for (nm in .custom_allowed) {
    fn <- tryCatch(get(nm, envir = baseenv()), error = function(...) NULL)
    if (is.null(fn)) fn <- tryCatch(get(nm, envir = asNamespace("stats")), error = function(...) NULL)
    if (!is.null(fn)) assign(nm, fn, envir = e)
  }
  assign("pi", base::pi, envir = e)
  e
})

# Recursively assert that an unevaluated expression tree contains only allowed
# calls, the symbols x / pi, and numeric literals. Throws on the first offense.
custom_check_node <- function(node) {
  if (is.call(node)) {
    head <- node[[1L]]
    if (!is.name(head))
      stop("only simple math function calls are allowed", call. = FALSE)
    nm <- as.character(head)
    if (!nm %in% .custom_allowed)
      stop(sprintf("'%s' is not an allowed function", nm), call. = FALSE)
    if (length(node) > 1L)
      for (i in 2L:length(node)) custom_check_node(node[[i]])
  } else if (is.name(node)) {
    nm <- as.character(node)
    if (!nm %in% c("x", "pi"))
      stop(sprintf("'%s' is not allowed — use x, pi, numbers and math functions", nm), call. = FALSE)
  } else if (is.numeric(node) || is.logical(node)) {
    invisible(NULL)
  } else if (is.character(node)) {
    stop("text strings are not allowed in the expression", call. = FALSE)
  } else {
    stop("unsupported token in the expression", call. = FALSE)
  }
}

# Parse + sandbox-check a density string, returning a function f(x). Throws a
# human-readable error if the string is empty, unparseable, or uses anything
# outside the whitelist.
safe_pdf <- function(expr_string) {
  if (is.null(expr_string) || !nzchar(trimws(expr_string)))
    stop("enter an expression for f(x)", call. = FALSE)
  parsed <- tryCatch(parse(text = expr_string),
                     error = function(e) stop("could not parse the expression", call. = FALSE))
  if (length(parsed) != 1L)
    stop("enter a single expression for f(x)", call. = FALSE)
  ex <- parsed[[1L]]
  custom_check_node(ex)
  function(x) eval(ex, list(x = x), enclos = .custom_env)
}

# Cumulative trapezoidal integral of y over x (same length as x, starts at 0).
custom_cumtrapz <- function(x, y) {
  n <- length(x)
  if (n < 2L) return(rep(0, n))
  c(0, cumsum((y[-1L] + y[-n]) / 2 * diff(x)))
}

# Convert a common subset of LaTeX math into an R expression string (for the
# Custom distribution's "LaTeX" input mode). Handles \frac, ^{}, e^{}, \sqrt,
# \left/\right, \cdot/\times, \pi, \ln/\log/trig, |..|, { } grouping, and a few
# safe implicit-multiplication cases. Always returns a string; if the result
# isn't valid R, the downstream sandbox parser reports a clear error.
latex_to_expr <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)[1L]
  s <- sub("^\\s*f\\s*\\(\\s*x\\s*\\)\\s*=\\s*", "", s)     # strip a leading "f(x) ="
  s <- gsub("\\\\left|\\\\right", "", s)                    # \left( \right) -> ( )
  s <- gsub("\\\\,|\\\\;|\\\\!|\\\\quad|\\\\ ", "", s)       # spacing macros
  s <- gsub("\\\\cdot|\\\\times", "*", s)
  s <- gsub("\\\\pi", "pi", s)
  s <- gsub("\\\\ln", "log", s)
  s <- gsub("\\\\arcsin", "asin", s); s <- gsub("\\\\arccos", "acos", s); s <- gsub("\\\\arctan", "atan", s)
  s <- gsub("\\\\Gamma", "gamma", s)
  s <- gsub("\\\\(log|exp|sqrt|sin|cos|tan|sinh|cosh|tanh|gamma|abs|floor|ceil|frac)", "\\1", s)
  # brace macros, resolved innermost-first via a fixpoint
  repeat {
    old <- s
    s <- gsub("e\\^\\{([^{}]*)\\}", "exp(\\1)", s)          # e^{...} -> exp(...)
    s <- gsub("\\^\\{([^{}]*)\\}", "^(\\1)", s)             # x^{...} -> x^(...)
    s <- gsub("sqrt\\{([^{}]*)\\}", "sqrt(\\1)", s)         # \sqrt{...} (already de-slashed)
    s <- gsub("frac\\{([^{}]*)\\}\\{([^{}]*)\\}", "((\\1)/(\\2))", s)
    s <- gsub("\\{([^{}]*)\\}", "(\\1)", s)                 # leftover { } -> ( )
    if (identical(s, old)) break
  }
  s <- gsub("\\|([^|]*)\\|", "abs(\\1)", s)                  # |a| -> abs(a)
  # a space between two operands means multiplication (e.g. "\pi x", ") (")
  repeat {
    old <- s
    s <- gsub("([a-zA-Z0-9)])[[:space:]]+([a-zA-Z0-9(])", "\\1*\\2", s)
    if (identical(s, old)) break
  }
  s <- gsub("[[:space:]]+", "", s)                           # strip remaining spaces
  # safe implicit multiplication (no separating space)
  s <- gsub("([0-9.])([a-zA-Z])", "\\1*\\2", s)             # 2x -> 2*x, 2pi -> 2*pi
  s <- gsub("([0-9.])\\(", "\\1*(", s)                       # 2( -> 2*(
  s <- gsub("\\)([0-9.a-zA-Z(])", ")*\\1", s)               # )( -> )*(, )x -> )*x
  s
}

# Parse a "location:weight, ..." point-mass string into list(loc, wt), or NULL
# when empty. Throws a human-readable error on malformed entries.
parse_masses <- function(str) {
  if (is.null(str) || !nzchar(trimws(str))) return(NULL)
  loc <- numeric(0); wt <- numeric(0)
  for (part in strsplit(trimws(str), ",")[[1L]]) {
    part <- trimws(part); if (!nzchar(part)) next
    kv <- strsplit(part, ":")[[1L]]
    if (length(kv) != 2L) stop("each point mass must be written as 'location:weight'", call. = FALSE)
    l <- suppressWarnings(as.numeric(trimws(kv[1L])))
    w <- suppressWarnings(as.numeric(trimws(kv[2L])))
    if (!is.finite(l) || !is.finite(w)) stop("point-mass location and weight must be numbers", call. = FALSE)
    if (w <= 0) stop("point-mass weights must be greater than 0", call. = FALSE)
    loc <- c(loc, l); wt <- c(wt, w)
  }
  if (!length(loc)) return(NULL)
  if (anyDuplicated(loc)) stop("point-mass locations must be distinct", call. = FALSE)
  list(loc = loc, wt = wt)
}

# Build a full numeric spec (same shape as contSpec() entries) for the custom
# distribution: a continuous density g(x) on [lo, hi] and optional point masses
# (a list(loc, wt) of locations and positive weights). Everything is normalized
# jointly so the total probability (continuous + masses) is 1. Returns normalized
# density, mixed CDF (with jumps at masses), quantile, mean, variance, the atom
# table, and a pmass(x) accessor. With no masses this reduces exactly to the
# previous pure-continuous behavior.
make_custom_spec <- function(expr_string, lo, hi, masses = NULL) {
  f0 <- safe_pdf(expr_string)
  # Non-negative, finite, length-aligned base density (handles constant f(x)).
  fbase <- function(x) {
    v <- suppressWarnings(as.numeric(f0(x)))
    if (length(v) != length(x)) v <- rep_len(v, length(x))
    v[!is.finite(v)] <- 0
    v[v < 0] <- 0
    v
  }
  Zc <- tryCatch(integrate(fbase, lo, hi, stop.on.error = FALSE)$value,
                 error = function(e) NA_real_)
  if (!is.finite(Zc) || Zc < 0) Zc <- 0
  loc <- if (is.null(masses)) numeric(0) else masses$loc
  wt  <- if (is.null(masses)) numeric(0) else masses$wt
  total <- Zc + sum(wt)
  if (!is.finite(total) || total <= 0)
    stop("the expression does not integrate to a positive, finite value over the support", call. = FALSE)

  prob <- wt / total                       # atom probabilities (empty if no masses)
  dfun <- function(x) {                    # continuous density component
    x <- as.numeric(x)
    out <- fbase(x) / total
    out[x < lo | x > hi] <- 0
    out
  }
  grid <- seq(lo, hi, length.out = 2049L)
  cc <- custom_cumtrapz(grid, fbase(grid)) / total      # continuous CDF: 0 .. Zc/total

  if (length(loc)) {
    Fc <- function(x) approx(grid, cc, xout = pmin(pmax(as.numeric(x), lo), hi), rule = 2)$y
    Fa <- function(x) vapply(as.numeric(x), function(xi) sum(prob[loc <= xi]), numeric(1))
    pfun <- function(x) Fc(x) + Fa(x)
    # Invert the mixed CDF on a grid that brackets each jump (loc-eps, loc).
    xs <- sort(unique(c(grid, loc, loc - 1e-9)))
    Fs <- pfun(xs)
    qfun <- function(p) vapply(as.numeric(p), function(t) {
      i <- which(Fs >= t)[1L]; if (is.na(i)) xs[length(xs)] else xs[i]
    }, numeric(1))
  } else {
    tot <- cc[length(cc)]; if (is.finite(tot) && tot > 0) cc <- cc / tot   # normalize to exactly 1
    pfun <- function(x) approx(grid, cc, xout = pmin(pmax(as.numeric(x), lo), hi), rule = 2)$y
    qfun <- function(p) approx(cc, grid, xout = as.numeric(p), rule = 2, ties = "ordered")$y
  }

  muc <- tryCatch(integrate(function(x) x * dfun(x), lo, hi, stop.on.error = FALSE)$value,
                  error = function(e) 0)
  ex2c <- tryCatch(integrate(function(x) x^2 * dfun(x), lo, hi, stop.on.error = FALSE)$value,
                   error = function(e) 0)
  mu <- muc + sum(prob * loc)
  v2 <- (ex2c + sum(prob * loc^2)) - mu^2
  # Probability mass exactly at a query point (0 unless it is an atom location).
  pmass <- function(x) vapply(as.numeric(x), function(xi) {
    i <- which(abs(loc - xi) <= 1e-8 * pmax(1, abs(loc)))[1L]; if (is.na(i)) 0 else prob[i]
  }, numeric(1))

  list(name = "Custom", d = dfun, p = pfun, q = qfun,
       mean = mu, var = v2, lo = lo, hi = hi,
       # Normalizing constant for the continuous part: f_c(x) = C * g(x).
       C = 1 / total,
       atoms = data.frame(loc = loc, prob = prob),   # 0-row when no masses
       contMass = Zc / total, pmass = pmass)
}

# Validation hook for the server: returns NULL when the custom inputs yield a
# valid distribution, otherwise a human-readable error string.
validate_custom <- function(expr_string, lo, hi, masses_str = NULL) {
  if (is.null(lo) || is.null(hi) || !is.finite(lo) || !is.finite(hi))
    return("Enter numeric lower and upper bounds for the support.")
  if (lo >= hi)
    return("The lower bound of the support must be less than the upper bound.")
  msg <- tryCatch({ make_custom_spec(expr_string, lo, hi, parse_masses(masses_str)); NULL },
                  error = function(e) conditionMessage(e))
  if (!is.null(msg)) paste0("Custom f(x): ", msg) else NULL
}

############################################################################
# Render a custom expression as nicely formatted LaTeX (incl. piecewise)   #
############################################################################
# A small converter from the whitelisted expression sublanguage to LaTeX,
# used to show f(x) on the Formulas page — including a \begin{cases} block
# for piecewise (ifelse-based) densities. Pure string building, no eval.

.lx_num <- function(x) trimws(formatC(as.numeric(x), format = "g", digits = 6))

# One-argument functions -> LaTeX templates (sprintf with a single %s).
.lx_unary <- c(
  sqrt = "\\sqrt{%s}", abs = "\\left|%s\\right|", exp = "e^{%s}",
  log = "\\ln\\!\\left(%s\\right)", log10 = "\\log_{10}\\!\\left(%s\\right)",
  log2 = "\\log_{2}\\!\\left(%s\\right)",
  sin = "\\sin\\!\\left(%s\\right)", cos = "\\cos\\!\\left(%s\\right)",
  tan = "\\tan\\!\\left(%s\\right)", asin = "\\arcsin\\!\\left(%s\\right)",
  acos = "\\arccos\\!\\left(%s\\right)", atan = "\\arctan\\!\\left(%s\\right)",
  sinh = "\\sinh\\!\\left(%s\\right)", cosh = "\\cosh\\!\\left(%s\\right)",
  tanh = "\\tanh\\!\\left(%s\\right)", gamma = "\\Gamma\\!\\left(%s\\right)",
  lgamma = "\\ln\\Gamma\\!\\left(%s\\right)", factorial = "\\left(%s\\right)!",
  floor = "\\lfloor %s \\rfloor", ceiling = "\\lceil %s \\rceil"
)
# Binary operators (besides + - * / ^, which are handled specially).
.lx_binop <- c("<" = "<", ">" = ">", "<=" = "\\le", ">=" = "\\ge",
               "==" = "=", "!=" = "\\ne", "&" = "\\;\\text{and}\\;",
               "|" = "\\;\\text{or}\\;")

# Operator precedence for deciding when to add parentheses (higher binds tighter).
.lx_prec <- function(node) {
  if (!is.call(node)) return(10L)
  op <- as.character(node[[1L]])
  if (op == "(") return(.lx_prec(node[[2L]]))      # grouping is transparent
  if (op == "|") return(1L)
  if (op == "&") return(2L)
  if (op %in% c("<", ">", "<=", ">=", "==", "!=")) return(3L)
  if (op == "+") return(4L)
  if (op == "-") return(if (length(node) == 3L) 4L else 6L)
  if (op == "*") return(5L)
  if (op == "^") return(7L)
  if (op == "!") return(6L)
  10L
}

# Render a node, wrapping in parentheses if it binds looser than the context.
.lx_wrap <- function(node, parent_prec) {
  s <- expr_to_latex(node)
  if (.lx_prec(node) < parent_prec) paste0("\\left(", s, "\\right)") else s
}

# Convert a parsed expression (or a string) to LaTeX.
expr_to_latex <- function(node) {
  if (is.character(node) && length(node) == 1L) node <- parse(text = node)[[1L]]
  if (is.numeric(node) || is.logical(node)) return(.lx_num(node))
  if (is.name(node)) { nm <- as.character(node); return(if (nm == "pi") "\\pi" else nm) }
  if (!is.call(node)) return("?")

  op <- as.character(node[[1L]]); a <- as.list(node)[-1L]
  if (op == "(") return(expr_to_latex(a[[1L]]))                       # transparent grouping
  if (op == "ifelse" && length(a) == 3L)                             # nested piecewise -> inline cases
    return(sprintf("\\begin{cases} %s, & %s \\\\ %s, & \\text{otherwise} \\end{cases}",
                   expr_to_latex(a[[2L]]), expr_to_latex(a[[1L]]), expr_to_latex(a[[3L]])))
  if (op == "-" && length(a) == 1L) return(paste0("-", .lx_wrap(a[[1L]], 6L)))   # unary minus
  if (op == "!" && length(a) == 1L) return(paste0("\\lnot ", .lx_wrap(a[[1L]], 6L)))
  if (op == "/") return(sprintf("\\frac{%s}{%s}", expr_to_latex(a[[1L]]), expr_to_latex(a[[2L]])))
  if (op == "^") {
    base <- if (.lx_prec(a[[1L]]) < 7L) paste0("\\left(", expr_to_latex(a[[1L]]), "\\right)") else expr_to_latex(a[[1L]])
    return(paste0(base, "^{", expr_to_latex(a[[2L]]), "}"))
  }
  if (op == "*") {
    la <- .lx_wrap(a[[1L]], 5L); lb <- .lx_wrap(a[[2L]], 5L)
    return(if (is.numeric(a[[1L]])) paste0(la, " ", lb) else paste0(la, " \\cdot ", lb))   # 2x vs a·b
  }
  if (op %in% c("+", "-") && length(a) == 2L)
    return(paste(.lx_wrap(a[[1L]], 4L), op, .lx_wrap(a[[2L]], 4L)))
  if (op %in% names(.lx_binop) && length(a) == 2L) {
    p <- .lx_prec(node)
    return(paste(.lx_wrap(a[[1L]], p), .lx_binop[[op]], .lx_wrap(a[[2L]], p)))
  }
  if (op %in% names(.lx_unary) && length(a) == 1L) return(sprintf(.lx_unary[[op]], expr_to_latex(a[[1L]])))
  if (op == "choose") return(sprintf("\\binom{%s}{%s}", expr_to_latex(a[[1L]]), expr_to_latex(a[[2L]])))
  if (op %in% c("beta", "lbeta"))
    return(sprintf("%sB\\!\\left(%s, %s\\right)", if (op == "lbeta") "\\ln " else "",
                   expr_to_latex(a[[1L]]), expr_to_latex(a[[2L]])))
  # everything else (min/max/pmin/pmax, density functions) -> roman name + args
  disp <- c(pmin = "min", pmax = "max")[op]; if (is.na(disp)) disp <- op
  sprintf("\\operatorname{%s}\\!\\left(%s\\right)", disp,
          paste(vapply(a, expr_to_latex, character(1)), collapse = ", "))
}

# Flatten a (possibly nested-in-the-else-branch) ifelse into ordered pieces,
# each list(cond = <node or NULL for the final "otherwise">, val = <node>).
.flatten_ifelse <- function(node) {
  if (is.call(node) && identical(as.character(node[[1L]]), "ifelse") && length(node) == 4L)
    c(list(list(cond = node[[2L]], val = node[[3L]])), .flatten_ifelse(node[[4L]]))
  else
    list(list(cond = NULL, val = node))
}

# Wrap a value in parentheses when a leading constant factor would otherwise
# bind only to its first term (e.g. C * (2 - x)).
.lx_factorwrap <- function(node) {
  s <- expr_to_latex(node)
  if (is.call(node) && as.character(node[[1L]]) %in% c("+", "-") && length(node) == 3L)
    paste0("\\left(", s, "\\right)")
  else s
}

# LaTeX for the continuous density component f(x) = C * g(x) on [lo, hi], with a
# \begin{cases} breakdown for piecewise densities.
.custom_cont_latex <- function(ex, C, lo, hi) {
  cfac <- if (abs(C - 1) < 5e-4) "" else paste0(trimws(formatC(C, format = "g", digits = 4)), "\\,")
  los <- .lx_num(lo); his <- .lx_num(hi)
  # A piece's density value: fold the constant into a plain number when the
  # piece is itself a constant (e.g. C·2 -> a single number), else "C·g(x)".
  pval <- function(val) {
    if (is.numeric(val)) .lx_num(C * as.numeric(val)) else paste0(cfac, .lx_factorwrap(val))
  }
  pieces <- .flatten_ifelse(ex)
  if (length(pieces) == 1L) {
    sprintf("f(x) = %s, \\quad %s \\le x \\le %s", pval(pieces[[1L]]$val), los, his)
  } else {
    rows <- vapply(pieces, function(pc) {
      cond <- if (is.null(pc$cond)) "\\text{otherwise}" else expr_to_latex(pc$cond)
      paste0(pval(pc$val), ", & ", cond)
    }, character(1))
    sprintf("f(x) = \\begin{cases} %s \\end{cases} \\quad \\text{for } %s \\le x \\le %s",
            paste(rows, collapse = " \\\\ "), los, his)
  }
}

# Full LaTeX for the normalized custom distribution on [lo, hi]: point masses
# (P(X = x_i) = p_i) and/or the continuous density component, with the numeric
# normalizing constant. Throws on an invalid support or non-integrable /
# unparseable expression.
custom_density_latex <- function(expr_string, lo, hi, masses = NULL) {
  if (is.null(lo) || is.null(hi) || !is.finite(lo) || !is.finite(hi) || lo >= hi)
    stop("invalid support", call. = FALSE)
  ex <- parse(text = expr_string)[[1L]]
  custom_check_node(ex)                                  # only the sandboxed sublanguage
  spec <- make_custom_spec(expr_string, lo, hi, masses)  # also rejects non-integrable
  parts <- character(0)
  if (nrow(spec$atoms) > 0L) {                           # point masses
    mass_rows <- vapply(seq_len(nrow(spec$atoms)), function(i)
      sprintf("\\mathbb{P}(X = %s) = %s", .lx_num(spec$atoms$loc[i]), .lx_num(spec$atoms$prob[i])),
      character(1))
    parts <- c(parts, paste(mass_rows, collapse = ",\\quad "))
  }
  if (spec$contMass > 1e-9)                              # continuous density component
    parts <- c(parts, .custom_cont_latex(ex, spec$C, lo, hi))
  paste(parts, collapse = " \\\\[4pt] ")
}
