#Prob Formulas

#Bernoulli
bernForm <- withMathJax(
  h4(strong("Bernoulli Distribution")),
  helpText('Parameter is $$ p \\in (0, 1]$$'),
  helpText('Support is $$ x \\in \\{ 0, 1 \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = p^x(1-p)^{1-x} =
           \\cases{
           1-p, & \\text{if }x = 0 \\cr
           p, & \\text{if }x = 1
           }$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = p$$'),
  helpText('Variance is $$\\mathbb{V}(X) = p(1-p)$$')
  )

#Binomial
binForm <- withMathJax(
  h4(strong("Binomial Distribution")),
  helpText('Parameters are $$n \\in \\{1, 2, 3, \\ldots \\} 
           \\\\ p \\in (0, 1]$$'),
  helpText('Support is $$ x \\in \\{ 0, 1, \\ldots, n \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = {n \\choose {x}} p^x(1-p)^{n-x} $$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = np$$'),
  helpText('Variance is $$\\mathbb{V}(X) = np(1-p)$$')
)

#Discrete Uniform
discUnifForm <- withMathJax(
  h4(strong("Discrete Uniform Distribution")),
  helpText('Parameters are $$a \\in \\{\\ldots, -2, -1, 0, 1, 2, \\ldots \\} 
           \\\\ b \\in \\{\\ldots, -2, -1, 0, 1, 2, \\ldots \\}
           \\\\ \\text{with } b \\ge a$$'),
  helpText('Support is $$ x \\in \\{ a, a + 1, \\ldots, b - 1, b \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = \\frac{1}{b - a + 1}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{a+b}{2}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{(b-a+1)^2-1}{12}$$')
)

#Geometric
geomForm <- withMathJax(
  h4(strong("Geometric Distribution")),
  helpText('Parameter is $$ p \\in (0, 1]$$'),
  helpText('Support is $$ x \\in \\{ 1, 2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = p(1-p)^{x-1}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{1}{p}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{1-p}{p^2}$$')
)

#Hypergeometric
hyperGeomForm <- withMathJax(
  h4(strong("Hypergeometric Distribution")),
  helpText('Parameters are $$N \\in \\{0, 1, 2, \\ldots \\} 
           \\\\ n \\in \\{0, 1, 2, \\ldots N\\}
           \\\\ m \\in \\{0, 1, 2, \\ldots N\\}$$'),
  helpText('Support is $$ x \\in \\{ max(0, n + m - N), \\ldots, min(m,n)\\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = \\dfrac{{m \\choose {x}}{N-m \\choose {n-x}} }{ {N \\choose {n}}  }$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{nm}{N}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{N-n}{N-1}\\left(\\frac{nm}{N}\\right)\\left( 1-\\frac{m}{N} \\right)$$')
)

#Negative Binomial
negBinForm <- withMathJax(
  h4(strong("Negative Binomial Distribution")),
  helpText('Parameters are $$r \\in \\{1, 2, 3, \\ldots \\} 
           \\\\ p \\in (0, 1]$$'),
  helpText('Support is $$ x \\in \\{r, r+1, r+2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = {x+r-1 \\choose {r-1}}p^r(1-p)^{x-r}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{r}{p}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{r(1-p)}{p^2}$$')
)

#Poisson
poiForm <- withMathJax(
  h4(strong("Poisson Distribution")),
  helpText('Parameter is $$ \\lambda \\in (0, \\infty)$$'),
  helpText('Support is $$ x \\in \\{ 0, 1, 2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = e^{-\\lambda}\\cdot \\frac{\\lambda^x}{x!}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\lambda$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\lambda$$')
)

###Continuous
#Beta
betaForm <- withMathJax(
  h4(strong("Beta Distribution")),
  helpText('Parameters are $$\\beta \\in (0, \\infty) 
           \\\\ \\alpha \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in (0, 1)$$'),
  helpText('PDF is $$f(x) = \\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)} x^{\\alpha-1} (1-x)^{\\beta-1}$$
           \\( \\qquad \\) where $$\\Gamma(t) = \\int_0^\\infty y^{t-1} e^{-y} \\, dy$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{\\alpha}{\\alpha + \\beta}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{\\alpha\\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta + 1)}$$')
)

#Chi-Square
chisqForm <- withMathJax(
  h4(strong("Chi-Square Distribution")),
  helpText('Parameter is $$ \\nu \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{2^{\\nu/2} \\, \\Gamma\\left( \\frac{\\nu}{2}\\right)} x^{{\\large \\frac{\\nu}{2}} - 1} e^{-\\large \\frac{x}{2}}$$
           \\( \\qquad \\) where $$\\Gamma(t) = \\int_0^\\infty y^{t-1} e^{-y} \\, dy$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\nu$$'),
  helpText('Variance is $$\\mathbb{V}(X) = 2\\nu$$')
)

#Exponential
expForm <- withMathJax(
  h4(strong("Exponential Distribution")),
  helpText('Parameter is $$ \\beta \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{e^{-x/\\beta}}{\\beta }$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\beta$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\beta^2$$')
)

#F
fForm <- withMathJax(
  h4(strong("F Distribution")),
  helpText('Parameters are $$d_1 \\in (0, \\infty) 
           \\\\ d_2 \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{\\Gamma \\left( \\frac{d_1}{2} + \\frac{d_2}{2} \\right)}{\\Gamma \\left( \\frac{d_1}{2} \\right) \\Gamma \\left( \\frac{d_2}{2} \\right)} 
            \\left( \\frac{d_1}{d_2} \\right)^{d_1/2} x^{{\\large \\frac{d_1}{2}} - 1} \\left(  1 + \\frac{d_1}{d_2}x \\right)^{- \\large\\frac{d_1 + d_2}{2}}$$
           \\( \\qquad \\) where $$\\Gamma(t) = \\int_0^\\infty y^{t-1} e^{-y} \\, dy$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean (for \\( d_2 > 2 \\)) is $$\\mathbb{E}(X) = \\frac{d_2}{d_2 - 2}$$'),
  helpText('Variance (for \\( d_2 > 4 \\)) is $$\\mathbb{V}(X) = \\frac{2 {d_2}^2 (d_1 + d_2 - 2)}{d_1 (d_2 - 2)^2 (d_2 - 4)}$$')
)

#Gamma
gamForm <- withMathJax(
  h4(strong("Gamma Distribution")),
  helpText('Parameters are $$\\beta \\in (0, \\infty) 
           \\\\ \\alpha \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{x^{\\alpha - 1}}{\\beta^{\\alpha} \\Gamma(\\alpha) }e^{-x/\\beta}$$
           \\( \\qquad \\) where $$\\Gamma(t) = \\int_0^\\infty y^{t-1} e^{-y} \\, dy$$'),
  #helpText('CDF is 
  #         $$\\mbox{ for } \\alpha = 1, \\, \\, F(x) = 1 - e^{-x/\\beta}$$
  #         $$\\mbox{ for } \\alpha = 2, \\, \\, F(x)= 1 - e^{-x/\\beta} - \\left( \\frac{x}{\\beta} \\right) e^{-x/\\beta} $$
  #         $$\\mbox{ for } \\alpha = 3, \\, \\, F(x)= 1 - e^{-x/\\beta} - \\left( \\frac{x}{\\beta} \\right) e^{-x/\\beta} - \\left(\\frac{(x/\\beta)^2}{2}e^{-x/\\beta}  \\right) $$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\alpha\\beta$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\alpha\\beta^2$$')
)

#Normal
normForm <- withMathJax(
  h4(strong("Normal Distribution")),
  helpText('Parameters are $$\\mu \\in (-\\infty, \\infty) 
           \\\\ \\sigma^2 \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in ( -\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} \\, e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\mu$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\sigma^2$$')
)

#Student's t
tForm <- withMathJax(
  h4(strong("Student's t Distribution")),
  helpText('Parameter is $$ \\nu \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in ( -\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{\\Gamma\\left( \\frac{\\nu + 1}{2} \\right)}{\\Gamma\\left( \\frac{\\nu}{2}\\right) \\sqrt{\\nu \\pi} } \\left( 1 + \\frac{x^2}{\\nu} \\right)^{-\\frac{\\Large\\nu+1}{\\Large2}}$$
           \\( \\qquad \\) where $$\\Gamma(t) = \\int_0^\\infty y^{t-1} e^{-y} \\, dy$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = 0$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{\\nu}{\\nu - 2}$$')
)


#Uniform
unifForm <- withMathJax(
  h4(strong("Continuous Uniform Distribution")),
  helpText('Parameters are $$ -\\infty < \\theta_1 < \\theta_2 < \\infty $$'),
  helpText('Support is $$ x \\in [ \\theta_1, \\theta_2 ]$$'),
  helpText('PDF is $$f(x) = \\frac{1}{\\theta_2-\\theta_1}$$'),
  #helpText('CDF is $$F(x) = \\frac{x-\\theta_1}{\\theta_2-\\theta_1}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{\\theta_1 +\\theta_2}{2}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{(\\theta_2-\\theta_1)^2}{12}$$')
)

#Weibull
weibForm <- withMathJax(
  h4(strong("Weibull Distribution")),
  helpText('Parameters are $$k \\in (0, \\infty) 
           \\\\ \\lambda \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{k}{\\lambda} \\left( \\frac{x}{\\lambda} \\right)^{k-1} e^{-(x/\\lambda)^k}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = 1 - e^{-(x/\\lambda)^k}$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\lambda \\, \\Gamma\\!\\left(1 + \\tfrac{1}{k}\\right)$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\lambda^2 \\left[ \\Gamma\\!\\left(1 + \\tfrac{2}{k}\\right) - \\Gamma\\!\\left(1 + \\tfrac{1}{k}\\right)^2 \\right]$$')
)

#Log-Normal
lnormForm <- withMathJax(
  h4(strong("Log-Normal Distribution")),
  helpText('Parameters are $$\\mu \\in (-\\infty, \\infty) 
           \\\\ \\sigma \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in (0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{x \\sigma \\sqrt{2\\pi}} \\, e^{-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{0}^{x}f(t) \\, dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = e^{\\mu + \\sigma^2/2}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\left( e^{\\sigma^2} - 1 \\right) e^{2\\mu + \\sigma^2}$$')
)

#Cauchy
cauchyForm <- withMathJax(
  h4(strong("Cauchy Distribution")),
  helpText('Parameters are $$x_0 \\in (-\\infty, \\infty) 
           \\\\ \\gamma \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in (-\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{\\pi \\gamma \\left[ 1 + \\left( \\frac{x - x_0}{\\gamma} \\right)^2 \\right]}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\frac{1}{\\pi} \\arctan\\!\\left( \\frac{x - x_0}{\\gamma} \\right) + \\frac{1}{2}$$'),
  helpText('Mean is undefined (does not exist).'),
  helpText('Variance is undefined (does not exist).')
)

#Logistic
logisForm <- withMathJax(
  h4(strong("Logistic Distribution")),
  helpText('Parameters are $$\\mu \\in (-\\infty, \\infty) 
           \\\\ s \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in (-\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{e^{-(x-\\mu)/s}}{s \\left( 1 + e^{-(x-\\mu)/s} \\right)^2}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\frac{1}{1 + e^{-(x-\\mu)/s}}$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\mu$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{s^2 \\pi^2}{3}$$')
)

#Pareto
paretoForm <- withMathJax(
  h4(strong("Pareto Distribution")),
  helpText('Parameters are $$x_m \\in (0, \\infty) 
           \\\\ \\alpha \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in [x_m, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{\\alpha \\, x_m^{\\alpha}}{x^{\\alpha + 1}}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = 1 - \\left( \\frac{x_m}{x} \\right)^{\\alpha}$$'),
  helpText('Mean (for \\( \\alpha > 1 \\)) is $$\\mathbb{E}(X) = \\frac{\\alpha \\, x_m}{\\alpha - 1}$$'),
  helpText('Variance (for \\( \\alpha > 2 \\)) is $$\\mathbb{V}(X) = \\frac{\\alpha \\, x_m^2}{(\\alpha - 1)^2 (\\alpha - 2)}$$')
)

#Laplace
laplaceForm <- withMathJax(
  h4(strong("Laplace Distribution")),
  helpText('Parameters are $$\\mu \\in (-\\infty, \\infty) 
           \\\\ b \\in (0, \\infty) $$'),
  helpText('Support is $$ x \\in (-\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{2b} \\, e^{-\\frac{|x - \\mu|}{b}}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\begin{cases} \\frac{1}{2} e^{(x-\\mu)/b}, & x < \\mu \\\\ 1 - \\frac{1}{2} e^{-(x-\\mu)/b}, & x \\ge \\mu \\end{cases}$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\mu$$'),
  helpText('Variance is $$\\mathbb{V}(X) = 2b^2$$')
)

# Custom (user-defined) — built from the LaTeX string and support the user
# entered. Unlike the fixed families above this is a function of the live
# inputs, so the server calls customForm(latex, lo, hi) to render it.
customForm <- function(expr_string, latex, lo, hi, masses_str = NULL) {
  # Auto-generate the normalized distribution LaTeX (point masses and/or the
  # continuous density, with the numeric normalizing constant and a
  # \begin{cases} breakdown for piecewise densities).
  gen <- tryCatch(custom_density_latex(expr_string, lo, hi, parse_masses(masses_str)),
                  error = function(e) NULL)
  if (!is.null(gen)) {
    withMathJax(
      h4(strong("Custom Distribution")),
      helpText('The density you defined, rescaled to integrate to 1 over its support and shown with its normalizing constant:'),
      helpText(sprintf("$$%s$$", gen)),
      helpText('and \\(f(x) = 0\\) outside the support. The CDF, mean, variance and quantiles are computed numerically.')
    )
  } else if (!is.null(latex) && nzchar(trimws(latex))) {
    lo_s <- if (is.null(lo) || !is.finite(lo)) "\\ell" else .lx_num(lo)
    hi_s <- if (is.null(hi) || !is.finite(hi)) "u" else .lx_num(hi)
    withMathJax(
      h4(strong("Custom Distribution")),
      helpText(sprintf('Showing your LaTeX override (the expression could not be auto-formatted): $$f(x) \\propto %s, \\quad %s \\le x \\le %s$$',
                       trimws(latex), lo_s, hi_s)),
      helpText('The normalizing constant, CDF, mean, variance and quantiles are computed numerically.')
    )
  } else {
    withMathJax(
      h4(strong("Custom Distribution")),
      helpText('Enter a valid density and support (in the Custom distribution box) to see its formatted representation.')
    )
  }
}
