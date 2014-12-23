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
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{N-n}{N-1}\\left(\\frac{nm}{N}\\right)\\left( 1-\\frac{nm}{N} \\right)$$')
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
  helpText('PDF is $$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}}e^{-(x-\\mu)/2\\sigma^2}$$'),
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
