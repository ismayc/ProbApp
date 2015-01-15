# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  #pageWithSidebar(
  
  # Application title
  titlePanel(h3("Calculator for Probability Distributions"), 
             windowTitle = "Calculator for Probability Distributions by Chester Ismay and Logan Soich"),
  
  p("Developed by Dr. Chester Ismay (", a("ismayc@ripon.edu", href="mailto:ismayc@ripon.edu"),
    ") and Logan Soich (", a("soichlo@ripon.edu", href="mailto:soichlo@ripon.edu"), ")"
  ),
  
  sidebarLayout(
    # Sidebar
    sidebarPanel(
      
      #wellPanel(
      radioButtons("distType", 
                   strong("Distribution Type:"),
                   list("Discrete","Continuous"),
                   selected="Discrete",
      ),
      
      #wellPanel(
      uiOutput("distName"), 
      #          ),
      
      #Select output type
      #wellPanel(
      radioButtons("outType", 
                   strong("Output Type:"), 
                   list("Formulas", "PDF/Quantile" = "PDF", 
                        "CDF", "Probability", "Mean", "Variance"), 
                   selected="Probability")
      #,
      # ),
      
      
      # uiOutput("param1"),
      # uiOutput("param2"),
      #uiOutput("param3"),
      
      
      
      # uiOutput("fixedVal1"),
      #  uiOutput("fixedVal2")
      
    ),
    
    # Output results of calculations & plots
    mainPanel(
      withMathJax(),
      uiOutput("formulas"),
      
      #uiOutput("param1"),
      #uiOutput("param2"),
      #uiOutput("param3"),
      
      conditionalPanel(condition = "input.outType != 'Formulas'",
                       strong("Parameters:")),
      
      #Prompt for input parameters into distribution
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
      
      #Continuous
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
      
      uiOutput("meanCalc"),
      uiOutput("varCalc"),
      
      plotOutput("distribPlot"),
      
      uiOutput("percentile"),
      #uiOutput("percentBox"),
      
      
      # conditionalPanel(condition = "input.outType =='PDF' || input.outType == 'CDF' || input.outType == 'Probability'",
      #                  strong("Inputs:")
      # ),
      
      #Prompt for input values
      # conditionalPanel(condition = "input.outType != 'Formulas' && (input.outType == 'PDF' || input.outType == 'CDF') && input.percentile != 'quant' && input.distrib == 'beta'",
      #                   numericInput("xFixed", withMathJax('Enter a discrete value (\\(x\\)):'), 0.5)
      #  ),
      
      conditionalPanel(condition = "input.outType != 'Formulas' && (input.outType == 'PDF') && input.percentile != 'quant'", 
                       #&& input.distrib != 'beta'",
                       numericInput("xFixedPC", withMathJax('Enter a discrete value (\\(x\\)):'), 1.0)
      ),
      
      conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'PDF' && input.percentile == 'quant'",
                       numericInput("quantile", withMathJax('Enter a percentile (between 0 and 1):'), 0.5, 
                                    min = 0.0, max = 1.0, step = 0.1)
      ),
      
      conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'Probability' 
                       && (input.probType == 'between' || input.probType == 'extreme')",
                       #&& input.distrib == 'beta'",
                       # numericInput("x1", withMathJax('Enter lower value (\\(x_1\\)):'), 0.5),
                       # numericInput("x2", withMathJax('Enter upper value (\\(x_2\\)):'), 0.7)
                       numericInput("x1", withMathJax('Enter lower value (\\(x_1\\)):'), 0),
                       numericInput("x2", withMathJax('Enter upper value (\\(x_2\\)):'), 1) 
      ),   
      
      #     conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'Probability' 
      #                                   && (input.probType == 'between' || input.probType == 'extreme')
      #                                   && input.distrib != 'beta'",
      #                      numericInput("x1", withMathJax('Enter lower value (\\(x_1\\)):'), 3.0),
      #                      numericInput("x2", withMathJax('Enter upper value (\\(x_2\\)):'), 6.0)
      #     ),     
      
      
      ###HERE
      conditionalPanel(condition = "input.outType != 'Formulas' && ((input.outType == 'Probability' && input.probType == 'lowerTail')
                       || input.outType == 'CDF')",
                       # && input.distrib != 'beta'",
                       numericInput("xFixedL", withMathJax('Enter a discrete value (\\(x\\)):'), 0)
    ), 
    
    conditionalPanel(condition = "input.outType != 'Formulas' && input.outType == 'Probability' 
                     && input.probType == 'upperTail'",
                     #   && input.distrib != 'beta'",
                     numericInput("xFixedU", withMathJax('Enter a discrete value (\\(x\\)):'), 1.0)
    ), 
    
    uiOutput("percentCalc"),
    
    #uiOutput("fixedVal1"),
    #uiOutput("fixedVal2"),
    
    
    uiOutput("distribCalc"),
    uiOutput("probTypeSelect"),
    uiOutput("probCalc")
    
    )
  )
)
)