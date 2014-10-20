#Prob Formulas

#Bernoulli
bernForm <- withMathJax(
  h4(strong("Bernoulli Distribution")),
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

#Binary
binForm <- withMathJax(
  h4(strong("Binomial Distribution")),
  helpText('Support is $$ x \\in \\{ 0, 1, \\ldots, n \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = {n \\choose {x}} p^x(1-p)^{n-x} $$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = np$$'),
  helpText('Variance is $$\\mathbb{V}(X) = np(1-p)$$')
)

#Discrete Uniform
discUnifForm <- withMathJax(
  h4(strong("Discrete Uniform Distribution")),
  helpText('Support is $$ x \\in \\{ 1, 2, \\ldots, n \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = \\frac{1}{n}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{a+b}{2}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{(b-a+1)^2-1}{12}$$')
)

#Geometric
geomForm <- withMathJax(
  h4(strong("Geometric Distribution")),
  helpText('Support is $$ x \\in \\{ 1, 2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = p(1-p)^{x-1}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{1}{p}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{1-p}{p^2}$$')
)

#Hypergeometric
hyperGeomForm <- withMathJax(
  h4(strong("Hypergeometric Distribution")),
  helpText('Support is $$ x \\in \\{ max(0, n + m - N), \\ldots, min(m,n)\\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = \\dfrac{{m \\choose {x}}{N-m \\choose {n-x}} }{ {N \\choose {n}}  }$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{nm}{N}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{N-n}{N-1}\\left(\\frac{nm}{N}\\right)\\left( 1-\\frac{nm}{N} \\right)$$')
)

#Negative Binomial
negBinForm <- withMathJax(
  h4(strong("Negative Binomial Distribution")),
  helpText('Support is $$ x \\in \\{r, r+1, r+2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = {x+r-1 \\choose {r-1}}p^r(1-p)^{x-r}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{r}{p}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{r(1-p)}{p^2}$$')
)

#Poisson
poiForm <- withMathJax(
  h4(strong("Poisson Distribution")),
  helpText('Support is $$ x \\in \\{ 0, 1, 2, \\ldots \\}$$'),
  helpText('PDF is $$f(x) = \\mathbb{P}(X = x) = e^{-\\lambda}\\cdot \\frac{\\lambda^x}{x!}$$'),
  helpText('CDF is $$F(x) =  \\mathbb{P} (X \\le x) = \\sum_{i \\, \\le \\, x} \\mathbb{P}(X = i)$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\lambda$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\lambda$$')
)

###Continuous
#Exponential
expForm <- withMathJax(
  h4(strong("Exponential Distribution")),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{e^{-x/\\beta}}{\\beta }$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\beta$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\beta^2$$')
)

#Gamma
gamForm <- withMathJax(
  h4(strong("Gamma Distribution")),
  helpText('Support is $$ x \\in [0, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{x^{\\alpha - 1}}{\\beta^{\\alpha} (\\alpha - 1)! }e^{-x/\\beta}$$'),
  #helpText('CDF is 
  #         $$\\mbox{ for } \\alpha = 1, \\, \\, F(x) = 1 - e^{-x/\\beta}$$
  #         $$\\mbox{ for } \\alpha = 2, \\, \\, F(x)= 1 - e^{-x/\\beta} - \\left( \\frac{x}{\\beta} \\right) e^{-x/\\beta} $$
  #         $$\\mbox{ for } \\alpha = 3, \\, \\, F(x)= 1 - e^{-x/\\beta} - \\left( \\frac{x}{\\beta} \\right) e^{-x/\\beta} - \\left(\\frac{(x/\\beta)^2}{2}e^{-x/\\beta}  \\right) $$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\alpha\\beta$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\alpha\\beta^2$$')
)

#Normal
normForm <- withMathJax(
  h4(strong("Normal Distribution")),
  helpText('Support is $$ x \\in ( -\\infty, \\infty )$$'),
  helpText('PDF is $$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}}e^{-(x-\\mu)/2\\sigma^2}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\mu$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\sigma^2$$')
)

#Uniform
unifForm <- withMathJax(
  h4(strong("Continuous Uniform Distribution")),
  helpText('Support is $$ x \\in [ \\theta_1, \\theta_2 ]$$'),
  helpText('PDF is $$f(x) = \\frac{1}{\\theta_2-\\theta_1}$$'),
  #helpText('CDF is $$F(x) = \\frac{x-\\theta_1}{\\theta_2-\\theta_1}$$'),
  helpText('CDF is $$F(x) = \\mathbb{P}\\{X \\le x\\} = \\int_{-\\infty}^{x}f(t) dt$$'),
  helpText('Mean is $$\\mathbb{E}(X) = \\frac{\\theta_1 +\\theta_2}{2}$$'),
  helpText('Variance is $$\\mathbb{V}(X) = \\frac{(\\theta_2-\\theta_1)^2}{12}$$')
)
