
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("ggplot2" %in% names(installed.packages()[,"Package"]))) {install.packages("ggplot2")}
suppressMessages(library(ggplot2, quietly = TRUE))

source("functions.R")
source("formulas.R")

shinyServer(function(input, output, session) {
  
  #Select distribution after choosing Discrete or Continuous type
  output$distName <- renderUI({
    if(is.null(input$distType)) return ()
    if(input$distType=="Discrete"){
      radioButtons("distrib","",selected="bin",
                   list("Bernoulli"="bern",
                        "Binomial"="bin","Discrete Uniform"="dunif",
                        "Geometric"="geom","Hypergeometric"="hgeom",
                        "Negative Binomial"="nbin","Poisson"="poi")
      )
    } else if(input$distType=="Continuous"){
      radioButtons("distrib","",
                   selected="exp",
                   list(#"Beta"="beta","Cauchy"="cauchy",
                     #"Chi-squared"="chisq",
                     "Exponential"="exp",
                     #"F"="F",
                     "Gamma"="gam",
                     #"Laplace"="lap",
                     #"Logistic"="logi",
                     #"Log-Normal"="lognorm",
                     "Normal"="norm",
                     #"Pareto"="pareto","t"="t",
                     "Uniform"="unif")
                   #"Weibull"="weib")
      )
    }
  })
  
  output$probTypeSelect <- renderUI({
    if(input$outType == "Probability"){
      selectInput("probType", label = h5("Select probability type:"), 
                  selected="lowerTail",
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
             exp = expForm,
             gam = gamForm,
             norm = normForm,
             unif = unifForm,
      )
    }
  })
  
  #Prompt for first input value
  output$fixedVal1 <- renderUI({
    if(is.null(input$probType)) return ()
    if(input$outType != "Formulas"){
      switch(input$outType,
             PDF = if(input$percentile == "quant" && !is.null(input$percentile)) return () 
                   else numericInput("xFixed", withMathJax('Enter a discrete value (\\(x\\)):'), 4.0),
             CDF = numericInput("xFixed", withMathJax('Enter a discrete value (\\(x\\)):'), 1.0),
             Probability = switch(input$probType,
                                  "between" = numericInput("x1", 
                                                           withMathJax('Enter lower value (\\(x_1\\)):'), 4),
                                  "lowerTail" = numericInput("xFixed", 
                                                             withMathJax('Enter a discrete value (\\(x\\)):'), 4),
                                  "upperTail" = numericInput("xFixed", 
                                                             withMathJax('Enter a discrete value (\\(x\\)):'), 4),
                                  "extreme" = numericInput("x1", 
                                                           withMathJax('Enter lower value (\\(x_1\\)):'), 4),
                                  NULL 
             )
      )
    }
  })

  #Prompt for second input value if needed
  output$fixedVal2 <- renderUI({
    if(is.null(input$probType) || is.null(input$x1)) return ()
    if(input$outType != "Formulas"){
      switch(input$outType, 
             Probability = switch(input$probType,
                                  "between" = numericInput("x2", 
                                                           withMathJax('Enter upper value (\\(x_2\\)):'), 6,
                                                           min = input$x1 + 1),
                                  "extreme" = numericInput("x2", 
                                                           withMathJax('Enter upper value (\\(x_2\\)):'), 6,
                                                           min = input$x1 + 1),
                                  NULL
             ),
             NULL
      )
    }
  }) 
  
  
  #Prompt for first input parameter into distribution
  output$param1 <- renderUI({
    if(is.null(input$distrib)) return ()
    if(input$outType != "Formulas"){
      switch(input$distrib, 
             #####Zero is not really a valid entry but the input boxes act 
             ##### strangely when you enter 0.001 or smaller
             #Discrete
             bern = numericInput("p", withMathJax('Enter the probability of success (\\(p\\)):'), 0.5, 
                                 step=0.1, min=0, max=1),
             bin = numericInput("p", withMathJax('Enter the probability of success (\\(p\\)):'), 0.5,
                                step=0.1, min=0, max=1),
             geom = numericInput("p", withMathJax('Enter the probability of success (\\(p\\)):'), 0.5,
                                 step=0.1, min=0, max=1),
             dunif = numericInput("a", withMathJax('Enter the lower bound (\\(a\\)):'),1),
             nbin = numericInput("p", withMathJax('Enter the probability of success (\\(p\\)):'), 0.5,
                                 step=0.1, min=0, max=1),
             poi = numericInput("lambda", withMathJax('Enter the rate parameter (\\(\\lambda\\)):'), 4,
                                step = 1, min = 0), 
             hgeom = numericInput("numEvents", 
                                  withMathJax('Enter the total number of events (\\(N\\)) i.e. total number of balls:'), 10,
                                  step=1, min=1),
             #Continuous
             exp = numericInput("beta", withMathJax('Enter the scale parameter of the distribution (\\(\\beta\\)):'), 5.0,
                                min = 0),
             gam = numericInput("beta", withMathJax('Enter the scale parameter of the distribution (\\(\\beta\\)):'), 5.0,
                                min = 0),
             norm = numericInput("normMean", withMathJax('Enter the mean of the distribution (\\(\\mu\\)):'), 0.0),
             unif = numericInput("theta1", withMathJax('Enter the lower bound of the distribution (\\(\\theta_1\\)):'), 0.0),
      )
    }
  })

  #Prompt for second input parameter into distribution
  output$param2 <- renderUI({
    if(is.null(input$distrib)) return ()
    if(input$outType != "Formulas"){
      switch(input$distrib,
             bin = numericInput("numTrials", withMathJax('Enter the number of trials (\\(n\\)):'), 15,
                                step = 1, min=1),
             dunif = numericInput("b", 
                                  withMathJax('Enter the upper bound (\\(b\\)) where \\(b > a\\):'), 6),
                                  # min=input$a + 1),
             nbin = numericInput("numSuccesses", withMathJax('Enter the number of successes (\\(r\\)):'), 2,
                                 step = 1, min=1),
             hgeom = numericInput("numTrials", 
                                  withMathJax('Enter the number of trials (\\(n\\)) where \\(n < N \\) i.e. number of balls chosen:'), 5,
                                  step=1, min=0),
                                  #, max = input$numEvents - 1),
             #Continuous
             gam = numericInput("alpha", withMathJax('Enter the shape parameter of the distribution (\\(\\alpha\\)):'), 2.0,
                                min = 0),
             norm = numericInput("normVar", withMathJax('Enter the variance of the distribution (\\(\\sigma^2\\)):'), 1.0,
                                 min=0),
             unif = numericInput("theta2", withMathJax('Enter the upper bound of the distribution (\\(\\theta_2\\)):'), 5.0),
             NULL
      )
    }
  })

  #Prompt for third input parameter into distribution  
  output$param3 <- renderUI({
    if(is.null(input$distrib)) return ()
    if(input$outType != "Formulas"){
      switch(input$distrib,
             hgeom = numericInput("favBalls", 
                                  withMathJax('Enter the total number of favorable events (\\(m\\)) where \\(m < N \\) i.e. total number of white balls:'), 3,
                                  step=1, min=0),
                                  #max=input$numEvents -1),
             NULL
      )
    }
  })
  
  output$distribPlot <- renderPlot({
    #To allow for the parameters to fill before calculating/plotting
    if(is.null(input$distrib)) return ()
    if(is.null(input$probType)) return()
    if(input$distrib == "bern" && is.null(input$p)) return ()
    else if(input$distrib == "bin" && (is.null(input$numTrials) || is.null(input$p))) return ()
    else if(input$distrib == "dunif" && (is.null(input$a) || is.null(input$b))) return ()
    else if(input$distrib == "geom" && is.null(input$p)) return ()
    else if(input$distrib == "hgeom" && (is.null(input$numTrials) || is.null(input$numEvents) || is.null(input$favBalls))) return ()
    else if(input$distrib == "nbin" && (is.null(input$p) || is.null(input$numSuccesses))) return ()
    else if(input$distrib == "poi" && is.null(input$lambda)) return ()
    else if(input$distrib == "exp" && is.null(input$beta)) return ()
    else if(input$distrib == "gam" && (is.null(input$beta) || is.null(input$alpha))) return ()
    else if(input$distrib == "norm" && (is.null(input$normMean) || is.null(input$normVar))) return ()
    else if(input$distrib == "unif" && (is.null(input$theta1) || is.null(input$theta2))) return ()
    
    if(input$outType == "PDF" && input$percentile == "pdf" && !is.null(input$percentile)){
      if(is.null(input$xFixed)) return ()
      
      #Plot PDF function using the distribPlot function in functions.R with shading of parameter value
      switch(input$distrib, 
             "bern" = distribPlot(func = dbinom, args = c(1, input$p), inputValue = input$xFixed),
             
             "bin" = distribPlot(range = 0:input$numTrials, args = c(input$numTrials, input$p), 
                                 inputValue = input$xFixed, distribName = "Binomial"),
             
             "dunif" = distribPlot(func = dunifdisc, range = input$a:input$b, args = c(input$a, input$b),
                                   inputValue = input$xFixed, xlabel = "Discrete Values",
                                   distribName = "Discrete Uniform"),
             
             "geom" = distribPlot(func = dgeom, range = 1:ceiling(4*1/input$p), args = c(input$p),
                                  inputValue = input$xFixed, xlabel = "Number of Trials",
                                  distribName = "Geometric", numArgs = 1, paramAdjust = 1),
             
             "hgeom" = distribPlot(func = dhyper, 
                                   range = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                   args = c(input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                                   inputValue = input$xFixed, distribName = "Hypergeometric", numArgs = 3),
             
             "nbin" = distribPlot(func = dnbinom, range = input$numSuccesses:ceiling(7*1/input$p), 
                                  args = c(input$numSuccesses, input$p),
                                  inputValue = input$xFixed, xlabel = "Number of Trials",
                                  distribName = "Negative Binomial", paramAdjust = input$numSuccesses),
             
             "poi" = distribPlot(func = dpois, range = 0:ceiling(4*input$lambda),
                                 args = c(input$lambda), inputValue = input$xFixed,
                                 xlabel = "Number of Occurrences",
                                 distribName = "Poisson", numArgs = 1),
             
             #Continuous
             "norm" = normal_prob_area_plot(input$xFixed, input$xFixed, 
                                            mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
             "unif" = uniform_prob_area_plot(input$xFixed, input$xFixed, min = input$theta1, max = input$theta2),
             "exp" = exp_prob_area_plot(input$xFixed, input$xFixed, shape = 1, scale = input$beta),
             "gam" = gamma_prob_area_plot(input$xFixed, input$xFixed, shape = input$alpha, scale = input$beta),
             #NULL
      )
    } 
    
    ######Need to put into function similar to distribPlot above
    #Plot PDF function with shading of appropriate quantile value
    else if(input$outType == "PDF" && input$percentile != "pdf" && !is.null(input$percentile)){
      switch(input$distrib,
      "bern" = qplot(factor(0:1), 
                     dbinom(0:1, 1, input$p),
                     xlab = "Number of Successes", 
                     ylab = "Probability", 
                     main = "Bernoulli Probability Mass Function\n",
                     geom = "bar", 
                     stat = "identity",
                     fill = 0:1 <= if(!is.null(input$quantile) && !is.null(input$p))
                                      qbinom(input$quantile, 1, input$p)
                                  else {-1}
      )
      + scale_fill_manual(values= if(!is.null(input$quantile) && !is.null(input$p) && qbinom(input$quantile, 1, input$p) == 1) c("#00BA38", "black")
                                  else c("black", "#00BA38")
      )
      + guides(fill=FALSE),
      "bin" = qplot(factor(0:input$numTrials), 
                    dbinom(0:input$numTrials, input$numTrials, input$p),
                    xlab = "Number of Successes", 
                    ylab = "Probability", 
                    main = "Binomial Probability Mass Function\n",
                    geom = "bar", 
                    stat = "identity",
                    fill = 0:input$numTrials <= if(!is.null(input$quantile) && !is.null(input$numTrials) && !is.null(input$p))
                                                    qbinom(input$quantile, input$numTrials, input$p)
                                                else {-1}
      )
      #To get the default shading colors to black with green
      + scale_fill_manual(values= 
          if (!is.null(input$quantile) && !is.null(input$numTrials) && !is.null(input$p) && 
                qbinom(input$quantile, input$numTrials, input$p) == input$numTrials){c("#00BA38", "#000000")}
          else{c("#000000", "#00BA38")})
      + guides(fill=FALSE), 
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
                                  qunifdisc(input$quantile, input$a, input$b) == input$b){c("#00BA38", "#000000")}
        else{c("#000000", "#00BA38")})
      + guides(fill=FALSE),
      "geom" = qplot(factor(1:ceiling(4*1/input$p)),  ##### How large to set the bounds?
                     dgeom(1:ceiling(4*1/input$p)-1, input$p),
                     xlab = "Number of Trials", 
                     ylab = "Probability", 
                     main = "Geometric Probability Mass Function\n",
                     geom = "bar", 
                     stat = "identity",
                     fill = 1:ceiling(4*1/input$p) <= if(!is.null(input$quantile) && !is.null(input$p))
                                                          qgeom(as.numeric(input$quantile), as.numeric(input$p)) + 1
                                                      else {-1}   
      )
      #To get the default shading colors to black with green
      + scale_fill_manual(values= 
          if (!is.null(input$quantile) && !is.null(input$p) && 
                ceiling(4*1/input$p) <= qgeom(as.numeric(input$quantile), as.numeric(input$p)) + 1){c("#00BA38", "#000000")}
        else{c("#000000", "#00BA38")})
      + guides(fill=FALSE), 
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
                     as.numeric(input$numTrials)) == input$favBalls){c("#00BA38", "#000000")}
        else{c("#000000", "#00BA38")})
      + guides(fill=FALSE), 
      "nbin" = qplot(factor(input$numSuccesses:ceiling(7*1/input$p)), 
                     dnbinom(input$numSuccesses:ceiling(7*1/input$p), input$numSuccesses, input$p),
                     xlab = "Number of Successes", 
                     ylab = "Probability", 
                     main = "Negative Binomial Probability Mass Function\n",
                     geom = "bar", 
                     stat = "identity",
                     fill = input$numSuccesses:ceiling(7*1/input$p) <= if(!is.null(input$quantile) && !is.null(input$numSuccesses) && !is.null(input$p))
                                                                          qnbinom(as.numeric(input$quantile), 
                                                                               as.numeric(input$numSuccesses), as.numeric(input$p)) + as.numeric(input$numSuccesses)
                                                                      else {-1}
      )
      + scale_fill_manual(values=  if(!is.null(input$quantile) && !is.null(input$numSuccesses) && !is.null(input$p) &&
                                        ceiling(7*1/input$p) <= qnbinom(as.numeric(input$quantile), 
                                                                      as.numeric(input$numSuccesses), 
                                                                      as.numeric(input$p)) + as.numeric(input$numSuccesses)){c("#00BA38", "#000000")}
        else{c("#000000", "#00BA38")})
      + guides(fill=FALSE), 
      "poi" = qplot(factor(0:ceiling(4*input$lambda)),
                    dpois(0:ceiling(4*input$lambda), input$lambda),
                    xlab = "Number of Occurrences", 
                    ylab = "Probability", 
                    main = "Poisson Probability Mass Function\n",
                    geom = "bar", 
                    stat = "identity",
                    fill = 0:ceiling(4*input$lambda) <= if(!is.null(input$quantile) && !is.null(input$lambda))
                                                            qpois(input$quantile, input$lambda)
                                                        else {-1}
      )
      #To get the default shading colors to black with green
      + scale_fill_manual(values= if(!is.null(input$quantile) && !is.null(input$lambda) && 
                                       ceiling(4*input$lambda) <= qpois(input$quantile, input$lambda)){c("#00BA38", "#000000")}
        else{c("#000000", "#00BA38")})
      + guides(fill=FALSE),
      
      #Continuous
      "norm" = normal_prob_area_plot(floor(as.numeric(input$normMean) - 4 * sqrt(as.numeric(input$normVar))), 
                                     qnorm(as.numeric(input$quantile), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))), 
                                     mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))
      ),
      "unif" = uniform_prob_area_plot(input$theta1, 
                                      qunif(input$quantile, as.numeric(input$theta1), as.numeric(input$theta2)), 
                                      min = as.numeric(input$theta1), 
                                      max = as.numeric(input$theta2)

      ),
      "exp" = exp_prob_area_plot(0, qgamma(as.numeric(input$quantile), shape = as.numeric(1), scale = as.numeric(input$beta)), 
                                                      shape = 1, scale = as.numeric(input$beta)
      ),
      "gam" = gamma_prob_area_plot(0, qgamma(as.numeric(input$quantile), 
                                             shape = as.numeric(input$alpha), scale = as.numeric(input$beta)), 
                                  shape = as.numeric(input$alpha), scale = as.numeric(input$beta)
      )
      )
    }
    #Plot CDF with appropriate shading corresponding to fixed value input
    else if(input$outType == "CDF"){
      if(is.null(input$xFixed)) return ()
      switch(input$distrib,
             "bern" = distribPlot(func = pbinom, range = 0:1, args = c(1, input$p),
                                  inputValue = input$xFixed, xlabel = "Number of Successes",
                                  distribName = "Bernoulli", plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "bin" = distribPlot(func = pbinom, range = 0:input$numTrials, args = c(input$numTrials, input$p),
                                 inputValue = input$xFixed, xlabel = "Number of Successes",
                                 distribName = "Binomial", plotType = "Cumulative",
                                 mainLabel = "Cumulative Distribution Function"),
             
             "dunif" = distribPlot(func = punifdisc, range = input$a:input$b, args = c(input$a, input$b),
                                   inputValue = input$xFixed, xlabel = "Discrete Values",
                                   distribName = "Discrete Uniform", plotType = "Cumulative",
                                   mainLabel = "Cumulative Distribution Function"),
             
             "geom" = distribPlot(func = pgeom, range = 1:ceiling(4*1/input$p), args = c(input$p),
                                  inputValue = input$xFixed, xlabel = "Number of Trials",
                                  distribName = "Geometric", numArgs = 1, paramAdjust = 1,
                                  plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "hgeom" = distribPlot(func = phyper, 
                                   range = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                   args = c(input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                                   inputValue = input$xFixed, distribName = "Hypergeometric", numArgs = 3,
                                   plotType = "Cumulative",
                                   mainLabel = "Cumulative Distribution Function"),
             
             "nbin" = distribPlot(func = pnbinom, range = input$numSuccesses:ceiling(7*1/input$p), 
                                  args = c(input$numSuccesses, input$p),
                                  inputValue = input$xFixed, xlabel = "Number of Trials",
                                  distribName = "Negative Binomial", paramAdjust = input$numSuccesses,
                                  plotType = "Cumulative",
                                  mainLabel = "Cumulative Distribution Function"),
             
             "poi" = distribPlot(func = ppois, range = 0:ceiling(4*input$lambda),
                                 args = c(input$lambda), inputValue = input$xFixed,
                                 xlabel = "Number of Occurrences",
                                 distribName = "Poisson", numArgs = 1,
                                 plotType = "Cumulative",
                                 mainLabel = "Cumulative Distribution Function"),
             
             #Continuous
             "norm" = normal_prob_CDF_plot(input$xFixed, mean = input$normMean, sd = sqrt(input$normVar)),
             "unif" = uniform_prob_CDF_plot(input$xfixed, min = input$theta1, max = input$theta2),
             "exp" = exp_prob_CDF_plot(input$xFixed, shape = 1, scale = input$beta),
             "gam" = gamma_prob_CDF_plot(input$xFixed, shape = input$alpha, scale = input$beta),
             #NULL
      )      
    } 
    #####Need to turn into function calls
    #Plot PDF with appropriate shading depending on probType selection
    else if(input$outType == "Probability"){
      if(is.null(input$probType)) return ()
      if((input$probType == "lowerTail" || input$probType == "upperTail") && is.null(input$xFixed)) return ()
      if((input$probType == "between" || input$probType == "extreme") && (is.null(input$x1) || is.null(input$x2))) return ()
      switch(input$distrib,
             "bern" = qplot(factor(0:1), 
                            dbinom(0:1, 1, input$p),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Bernoulli Probability Mass Function\n",
                            geom= "bar", 
                            stat= "identity",
                            fill = switch(input$probType,
                                          "between" = 0:1 >= input$x1 & 0:1 <= input$x2,
                                          "lowerTail" = 0:1 <= input$xFixed,
                                          "upperTail" = 0:1 >= input$xFixed,
                                          "extreme" = 0:1 <= input$x1 | 0:1 >= input$x2, 
                            )
             )
             + scale_fill_manual(values=
                                   if(input$probType == "between" && pbinom(input$x2, 1, input$p) - pbinom(input$x1 - 1, 1, input$p) == 1){
                                     c("#00BA38", "black")
                                   }
                                 else if(input$probType == "lowerTail" && pbinom(input$xFixed, 1, input$p) == 1){
                                   c("#00BA38", "black")
                                 }
                                 else if (input$probType == "upperTail" && 1 - pbinom(input$xFixed - 1, 1, input$p) == 1){
                                   c("#00BA38", "black")
                                 }
                                 else if(input$probType == "extreme" && 1 - pbinom(input$x2-1, size = 1, input$p) + pbinom(input$x1, 1, input$p) == 1){
                                   c("#00BA38", "black")
                                 }
                                 else
                                   c("black", "#00BA38")
             )
             + guides(fill=FALSE),
             "bin" = qplot(factor(0:input$numTrials), 
                           dbinom(0:input$numTrials, input$numTrials, input$p),
                           xlab = "Number of Successes", 
                           ylab = "Probability", 
                           main = "Binomial Probability Mass Function\n",
                           geom= "bar", 
                           stat= "identity",
                           fill = if(!is.null(input$xFixed)){
                             switch(input$probType,
                                         "between" = 0:input$numTrials >= input$x1 & 0:input$numTrials <= input$x2,
                                         "lowerTail" = 0:input$numTrials <= input$xFixed,
                                         "upperTail" = 0:input$numTrials >= input$xFixed,
                                         "extreme" = 0:input$numTrials <= input$x1 | 0:input$numTrials >= input$x2, 
                           )
                           }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if (pbinom(input$xFixed, input$numTrials, input$p) == 1){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if ( pbinom(input$xFixed - 1, input$numTrials, input$p, lower.tail = FALSE) == 1){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if (pbinom(as.numeric(input$x2), size = as.numeric(input$numTrials), 
                            prob = as.numeric(input$p)) 
                     - pbinom(as.numeric(input$x1)-1, size = as.numeric(input$numTrials), 
                              prob = as.numeric(input$p)) == 1)
                 {c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE), 
             "dunif" = qplot(factor(input$a:input$b), 
                             dunifdisc(input$a:input$b, input$a, input$b),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Discrete Uniform Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill = if(!is.null(input$xFixed)){
                               switch(input$probType,
                                           "between" = input$a:input$b >= input$x1 & input$a:input$b <= input$x2,
                                           "lowerTail" = input$a:input$b <= input$xFixed,
                                           "upperTail" = input$a:input$b >= input$xFixed,
                                           "extreme" = input$a:input$b <= input$x1 | input$a:input$b >= input$x2
                             ) 
                             }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if (punifdisc(input$xFixed, input$a, input$b) == 1){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if (punifdisc(input$xFixed - 1, input$a, input$b) == 0){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if (punifdisc(as.numeric(input$x2), as.numeric(input$a), as.numeric(input$b)) 
                     - punifdisc(as.numeric(input$x1)-1, as.numeric(input$a), as.numeric(input$b)) == 1)
                 {c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE),
             "geom" = qplot(factor(1:ceiling(4*1/input$p)),  ##### How large to set the bounds?
                            dgeom(1:ceiling(4*1/input$p)-1, input$p),
                            xlab = "Number of Trials", 
                            ylab = "Probability", 
                            main = "Geometric Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = if(!is.null(input$xFixed)){
                              switch(input$probType,
                                          "between" = 1:ceiling(4*1/input$p) >= input$x1 & 1:ceiling(4*1/input$p) <= input$x2,
                                          "lowerTail" = 1:ceiling(4*1/input$p) <= input$xFixed,
                                          "upperTail" = 1:ceiling(4*1/input$p) >= input$xFixed,
                                          "extreme" = 1:ceiling(4*1/input$p) <= input$x1 | 1:ceiling(4*1/input$p) >= input$x2, 
                            )
                            }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if (ceiling(4*1/input$p) <= input$xFixed){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if ( 1 >= input$xFixed){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if (input$x1 <= 1 && input$x2 >= ceiling(4*1/input$p))
                 c("#00BA38", "#000000")
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE), 
             "hgeom" = qplot(factor(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials)), 
                             dhyper(max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials), 
                                    input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
                             xlab = "Number of Successes", 
                             ylab = "Probability", 
                             main = "Hypergeometric Probability Mass Function\n",
                             geom = "bar", 
                             stat = "identity",
                             fill = if(!is.null(input$xFixed)){
                               switch(input$probType,
                                           "between" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$x1 & max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$x2,
                                           "lowerTail" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$xFixed,
                                           "upperTail" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$xFixed,
                                           "extreme" = max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) <= input$x1 | max(0, input$numTrials+input$favBalls-input$numEvents):min(input$favBalls, input$numTrials) >= input$x2, 
                             )
                             }
             )
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if (phyper(input$xFixed, input$favBalls, (input$numEvents - input$favBalls), input$numTrials) == 1){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if (phyper(input$xFixed - 1, input$favBalls, (input$numEvents - input$favBalls), input$numTrials) == 0){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if (phyper(as.numeric(input$x2), as.numeric(input$favBalls), 
                            as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) 
                     - phyper(as.numeric(input$x1) - 1, as.numeric(input$favBalls), 
                              as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) == 1)
                 {c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE), 
             "nbin" = qplot(factor(input$numSuccesses:ceiling(7*1/input$p)), 
                            dnbinom(input$numSuccesses:ceiling(7*1/input$p), input$numSuccesses, input$p),
                            xlab = "Number of Successes", 
                            ylab = "Probability", 
                            main = "Negative Binomial Probability Mass Function\n",
                            geom = "bar", 
                            stat = "identity",
                            fill = if(!is.null(input$xFixed)){
                              switch(input$probType,
                                          "between" = input$numSuccesses:ceiling(7*1/input$p) >= input$x1 & input$numSuccesses:ceiling(7*1/input$p) <= input$x2,
                                          "lowerTail" = input$numSuccesses:ceiling(7*1/input$p) <= input$xFixed,
                                          "upperTail" = input$numSuccesses:ceiling(7*1/input$p) >= input$xFixed,
                                          "extreme" = input$numSuccesses:ceiling(7*1/input$p) <= input$x1 | input$numSuccesses:ceiling(7*1/input$p) >= input$x2,
                                          #NULL
                            )
                            }
             )
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if ( ceiling(7*1/input$p) <= input$xFixed){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if ( pnbinom(input$xFixed - 1, input$numSuccesses, input$p, lower.tail = FALSE) == 1){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if (input$x1 <= input$numSuccesses && input$x2 >= ceiling(7*1/input$p))
                 {c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE), 
             "poi" = qplot(factor(0:ceiling(4*input$lambda)),
                           dpois(0:ceiling(4*input$lambda), input$lambda),
                           xlab = "Number of Occurrences", 
                           ylab = "Probability", 
                           main = "Poisson Probability Mass Function\n",
                           geom = "bar", 
                           stat = "identity",
                           fill = if(!is.null(input$xFixed)){
                             switch(input$probType,
                                    "between" = 0:ceiling(4*input$lambda) >= input$x1 & 0:ceiling(4*input$lambda) <= input$x2,
                                    "lowerTail" = 0:ceiling(4*input$lambda) <= input$xFixed,
                                    "upperTail" = 0:ceiling(4*input$lambda) >= input$xFixed,
                                    "extreme" = 0:ceiling(4*input$lambda) <= input$x1 | 0:ceiling(4*input$lambda) >= input$x2,
                                    #NULL
                             )
                           }
             )
             #To get the default shading colors to black with green
             + scale_fill_manual(values= if(input$probType == "extreme"){ 
               if(input$x2 - input$x1 > 1){c("#000000", "#00BA38")} 
               else{c("#00BA38","#000000")}}
               else if (input$probType == "lowerTail"){
                 if (ceiling(4*input$lambda) <= input$xFixed){c("#00BA38", "#000000")}
                 else{c("#000000","#00BA38")}}
               else if (input$probType == "upperTail"){
                 if ( ppois(input$xFixed - 1, input$lambda, lower.tail = FALSE) == 1){c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else if (input$probType == "between"){
                 if ( input$x1 <= 0 && input$x2 >= ceiling(4*input$lambda))
                 {c("#00BA38", "#000000")}
                 else{c("#000000", "#00BA38")}}
               else{c("#000000", "#00BA38")})
             + guides(fill=FALSE),
             
             #Continuous
             "norm" = switch(input$probType,
                             "between" = normal_prob_area_plot(input$x1, input$x2, 
                                                               mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "lowerTail" = normal_prob_area_plot(floor(input$normMean - 4 * sqrt(input$normVar)), input$xFixed, 
                                                                 mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "upperTail" = normal_prob_area_plot(input$xFixed, ceiling(input$normMean + 4 * sqrt(input$normVar)),
                                                                 mean = input$normMean, sd = sqrt(as.numeric(input$normVar))),
                             "extreme" = normal_prob_area_plot(input$x1, input$x2, 
                                                               mean = input$normMean, sd = sqrt(as.numeric(input$normVar)),
                                                               extreme = TRUE),
                             #NULL
             ),
             "unif" = switch(input$probType,
                             "between" = uniform_prob_area_plot(input$x1, input$x2, 
                                                                min = input$theta1, max = input$theta2),
                             "lowerTail" = uniform_prob_area_plot(input$theta1, input$xFixed, 
                                                                  min = input$theta1, max = input$theta2),
                             "upperTail" = uniform_prob_area_plot(input$xFixed, input$theta2,
                                                                  min = input$theta1, max = input$theta2),
                             "extreme" = uniform_prob_area_plot(input$x1, input$x2, 
                                                                min = input$theta1, max = input$theta2, 
                                                                extreme = TRUE),
                             #NULL
             ),
             "exp" = switch(input$probType,
                            "between" = exp_prob_area_plot(input$x1, input$x2, 
                                                           shape = 1, scale = input$beta),
                            "lowerTail" = exp_prob_area_plot(0, input$xFixed, 
                                                             shape = 1, scale = input$beta),
                            "upperTail" = exp_prob_area_plot(input$xFixed, 1 + 4 * input$beta,
                                                             shape = 1, scale = input$beta),
                            "extreme" = exp_prob_area_plot(input$x1, input$x2, 
                                                           shape = 1, scale = input$beta, 
                                                           extreme = TRUE),
                            #NULL
             ),
             "gam" = switch(input$probType,
                            "between" = gamma_prob_area_plot(input$x1, input$x2, 
                                                             shape = input$alpha, scale = input$beta),
                            "lowerTail" = gamma_prob_area_plot(0, input$xFixed, 
                                                               shape = input$alpha, scale = input$beta),
                            "upperTail" = gamma_prob_area_plot(input$xFixed, input$alpha + 10 * input$beta,
                                                               shape = input$alpha, scale = input$beta),
                            "extreme" = gamma_prob_area_plot(input$x1, input$x2, 
                                                             shape = input$alpha, scale = input$beta, 
                                                             extreme = TRUE),
                            #NULL
             ),
             # NULL
      )
    }
  })
  
  #Produce PDF value for given input value
  output$distribCalc <- renderUI({
    if(is.null(input$distrib) || is.null(input$xFixed)) return ()
    if(input$outType == "PDF"){
      if(is.null(input$percentile) || input$percentile == "quant") return ()
      switch(input$distrib,
             "bern" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %.04f$$", input$xFixed, 
                                          dbinom(as.numeric(input$xFixed), 
                                                 size = as.numeric(1), 
                                                 prob = as.numeric(input$p)
                                          )
             )),
             "bin" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %.04f$$", input$xFixed, 
                                         dbinom(as.numeric(input$xFixed), 
                                                size = as.numeric(input$numTrials), 
                                                prob = as.numeric(input$p)
                                         )
             )),
             "dunif" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %.04f$$", input$xFixed,
                                           dunifdisc(input$xFixed, 
                                                     as.numeric(input$a), 
                                                     as.numeric(input$b)
                                           )
             )),
             "geom" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %.04f$$", input$xFixed, 
                                          dgeom(as.numeric(input$xFixed)-1, 
                                                prob = as.numeric(input$p)
                                          )
             )),
             "hgeom" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %.04f$$", input$xFixed,
                                           dhyper(as.numeric(input$xFixed), 
                                                  as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), 
                                                  as.numeric(input$numTrials)
                                           )
                                           
             )),
             "nbin" = withMathJax(sprintf("$$\\mathbb{P}(X = %.f ) = %.04f$$", input$xFixed, 
                                          dnbinom(as.numeric(input$xFixed) - as.numeric(input$numSuccesses), 
                                                  input$numSuccesses, 
                                                  input$p)
             )),
             "poi" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = %.04f$$", input$xFixed, 
                                         dpois(as.numeric(input$xFixed), 
                                               lambda = as.numeric(input$lambda), 
                                         )
             )),
             #Continuous
             "exp" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = 0 \\\\ f(X =  %.f ) = %.04f$$", 
                                         input$xFixed,  
                                         input$xFixed,
                                         dgamma(as.numeric(input$xFixed), 
                                                shape = as.numeric(1),
                                                scale = as.numeric(input$beta)
                                         )
             )),
             "gam" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = 0 \\\\ f(X =  %.f ) = %.04f$$", 
                                         input$xFixed,  
                                         input$xFixed,
                                         dgamma(as.numeric(input$xFixed), 
                                                shape = as.numeric(input$alpha),
                                                scale = as.numeric(input$beta)
                                         )
             )),
             "norm" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = 0 \\\\ f(X =  %.f ) = %.04f$$", 
                                          input$xFixed,  
                                          input$xFixed,
                                          dnorm(as.numeric(input$xFixed), 
                                                mean = as.numeric(input$normMean),
                                                sd = sqrt(as.numeric(input$normVar))
                                          )
             )),
             "unif" = withMathJax(sprintf("$$\\mathbb{P}(X =  %.f ) = 0 \\\\ f(X =  %.f ) = %.04f$$", 
                                          input$xFixed,  
                                          input$xFixed,
                                          dunif(as.numeric(input$xFixed), 
                                                min = as.numeric(input$theta1),
                                                max = as.numeric(input$theta2)
                                          )
             )),
             #NULL
      )
      #Produce CDF value in nice LaTeX output
    } else if(input$outType == "CDF"){
      if(is.null(input$distrib) || is.null(input$xFixed)) return ()
      switch(input$distrib,
             "bern" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed, 
                                          pbinom(as.numeric(input$xFixed), 
                                                 size = as.numeric(1), 
                                                 prob = as.numeric(input$p)
                                          )
             )),
             "bin" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                         input$xFixed, 
                                         input$xFixed,
                                         pbinom(as.numeric(input$xFixed), 
                                                size = as.numeric(input$numTrials), 
                                                prob = as.numeric(input$p)
                                         )
             )),
             "dunif" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                           input$xFixed, 
                                           input$xFixed,
                                           punifdisc(as.numeric(input$xFixed), 
                                                     as.numeric(input$a), 
                                                     as.numeric(input$b)
                                           )
             )),
             "geom" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed,
                                          pgeom(as.numeric(input$xFixed)-1, 
                                                prob = as.numeric(input$p)
                                          )
             )),
             "hgeom" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq %.03f ) = %.04f$$", 
                                           input$xFixed,
                                           input$xFixed,
                                           phyper(as.numeric(input$xFixed), 
                                                  as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), 
                                                  as.numeric(input$numTrials)
                                           )
                                           
             )),
             "nbin" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq %.03f ) = %.04f$$", 
                                          input$xFixed,
                                          input$xFixed,
                                          pnbinom(as.numeric(input$xFixed) - as.numeric(input$numSuccesses), 
                                                  as.numeric(input$numSuccesses), 
                                                  as.numeric(input$p)
                                          )
             )),
             "poi" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed,
                                          ppois(as.numeric(input$xFixed), 
                                                lambda = as.numeric(input$lambda), 
                                          )
             )),
             
             #Continuous
             "exp" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed,
                                          pgamma(as.numeric(input$xFixed), 
                                                 shape = as.numeric(1),
                                                 scale = as.numeric(input$beta),
                                          )
             )),
             "gam" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed,
                                          pgamma(as.numeric(input$xFixed), 
                                                 shape = as.numeric(input$alpha),
                                                 scale = as.numeric(input$beta),
                                          )
             )),             
             "norm" =  withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                           input$xFixed, 
                                           input$xFixed,
                                           pnorm(as.numeric(input$xFixed), 
                                                 mean = as.numeric(input$normMean),
                                                 sd = sqrt(as.numeric(input$normVar)),
                                           )
             )),
             "unif" = withMathJax(sprintf("$$F(%.03f) = \\mathbb{P}(X \\leq  %.03f ) = %.04f$$", 
                                          input$xFixed, 
                                          input$xFixed,
                                          punif(as.numeric(input$xFixed), 
                                                min = as.numeric(input$theta1),
                                                max = as.numeric(input$theta2),
                                          )
             )),
             #NULL
      )
    }
  })
  
  #Select percentile option
  output$percentile <- renderUI({
    if(is.null(input$outType) || is.null(input$distrib)) return ()
    if( (input$outType == "PDF")) #& (input$distrib == "norm") )
      radioButtons("percentile", "\nCalculate:", selected = "pdf", inline = TRUE,
                   c("Quantile" = "quant",
                     "PDF" = "pdf"))
  })

  #Prompt for percentile choice
  output$percentBox <- renderUI({
    if(is.null(input$percentile)) return ()
    if(input$outType == "PDF"){
      if(input$percentile == "quant"){
        numericInput("quantile", withMathJax('Enter a percentile (between 0 and 1):'), 0.5, 
                     min = 0.0, max = 1.0, step = 0.1)
      }
    }
  }) 
  
  #Option to calculate the xvalue corresponding to a percentile.
  output$percentCalc <- renderUI({
    if(is.null(input$percentile)) return ()
    if(input$outType == "PDF"){
    if(input$percentile == "quant"){
      withMathJax(sprintf("\\(\\mathbb{P}(X \\le x) = \\) %.03f corresponds to an \\(x\\) value of %.04f \\( = F^{-1}(%.03f).\\)", 
                          input$quantile,
                          switch(input$distrib,
                                 "bern" = qbinom(as.numeric(input$quantile), 
                                                 size = as.numeric(1), 
                                                 prob = as.numeric(input$p)),
                                 "bin" = qbinom(as.numeric(input$quantile), 
                                                size = as.numeric(input$numTrials), 
                                                prob = as.numeric(input$p)),
                                 "dunif" = qunifdisc(input$quantile, 
                                                     as.numeric(input$a), 
                                                     as.numeric(input$b)),
                                 "geom" = qgeom(as.numeric(input$quantile), 
                                                prob = as.numeric(input$p)) + 1,
                                 "hgeom" = qhyper(as.numeric(input$quantile), 
                                                  as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), 
                                                  as.numeric(input$numTrials) ),
                                 "nbin" = qnbinom(as.numeric(input$quantile), 
                                                  as.numeric(input$numSuccesses), 
                                                  as.numeric(input$p)) + input$numSuccesses,
                                 "poi" = qpois(as.numeric(input$quantile), 
                                               lambda = as.numeric(input$lambda) ),
                                 #Continuous
                                 "exp" = qgamma(as.numeric(input$quantile), 
                                                shape = as.numeric(1),
                                                scale = as.numeric(input$beta)),
                                 "gam" = qgamma(as.numeric(input$quantile), 
                                                shape = as.numeric(input$alpha),
                                                scale = as.numeric(input$beta)),
                                 "norm" = qnorm(as.numeric(input$quantile), 
                                                mean = as.numeric(input$normMean),
                                                sd = sqrt(as.numeric(input$normVar))),
                                 "unif" = qunif(as.numeric(input$quantile), 
                                                min = as.numeric(input$theta1),
                                                max = as.numeric(input$theta2)),
                          ),
                          input$quantile
      )) 
    }
    }
  })
  
  #Output results of probability calculations depending on probType selection
  output$probCalc <- renderUI({
    
    if(is.null(input$distrib) || is.null(input$probType)) return ()
    
    if(input$outType == "Probability"){
      if(input$probType == "between" && input$distType == "Discrete"){
        if(is.null(input$x1) || is.null(input$x2)) return ()
        
        withMathJax(sprintf("The probability that \\(X\\) is between %.03f and %.03f (inclusive) is $$\\sum\\limits_{i \\, = \\, %.03f}^{%.03f} \\mathbb{P}(X = i) = \\mathbb{P}(X \\leq %.03f) - \\mathbb{P}(X \\leq %.03f) = %.04f$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x2,
                            input$x1 - 1, 
                            if(input$x2 <= input$x1) {0}
                            else{
                              switch(input$distrib,
                                     "bern" = pbinom(as.numeric(input$x2), size = as.numeric(1), prob = as.numeric(input$p)) 
                                     - pbinom(as.numeric(input$x1) - 1, size = as.numeric(1), prob = as.numeric(input$p)),
                                     "bin" = pbinom(as.numeric(input$x2), size = as.numeric(input$numTrials), 
                                                    prob = as.numeric(input$p)) 
                                     - pbinom(as.numeric(input$x1)-1, size = as.numeric(input$numTrials), 
                                              prob = as.numeric(input$p)),
                                     "dunif" = punifdisc(as.numeric(input$x2), as.numeric(input$a), as.numeric(input$b)) 
                                     - punifdisc(as.numeric(input$x1)-1, as.numeric(input$a), as.numeric(input$b)),
                                     "geom" = pgeom(as.numeric(input$x2)-1, prob = as.numeric(input$p)) 
                                     - pgeom(as.numeric(input$x1)-2, prob = as.numeric(input$p)),
                                     "hgeom" = phyper(as.numeric(input$x2), as.numeric(input$favBalls), 
                                                      as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) 
                                     - phyper(as.numeric(input$x1) - 1, as.numeric(input$favBalls), 
                                              as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                     "nbin" = pnbinom(as.numeric(input$x2 - input$numSuccesses), 
                                                      as.numeric(input$numSuccesses), as.numeric(input$p)) 
                                     - pnbinom(as.numeric(input$x1 - input$numSuccesses) - 1, 
                                               as.numeric(input$numSuccesses), as.numeric(input$p)),
                                     "poi" = ppois(as.numeric(input$x2), as.numeric(input$lambda)) 
                                     - ppois(as.numeric(input$x1) - 1, as.numeric(input$lambda)),
                                     NULL
                              )
                            }
        )
        ) 
      } else if(input$probType == "between" && input$distType == "Continuous" && !is.null(input$x1) && !is.null(input$x2)){
        #if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is between %.03f and %.03f (inclusive) is $$\\int\\limits_{%.03f}^{%.03f} f(x) \\, dx = \\mathbb{P}(X \\leq %.04f) - \\mathbb{P}(X \\leq %.04f) \\approx %.04f$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x2,
                            input$x1,
                            if(input$x2 <= input$x1) {0}
                            else{
                              if(is.null(input$x2) || is.null(input$beta)) return ()
                              switch(input$distrib,
                                     "exp"  = pgamma(as.numeric(input$x2), shape = as.numeric(1), scale = as.numeric(input$beta)) 
                                     - pgamma(as.numeric(input$x1), shape = as.numeric(1), scale = as.numeric(input$beta)),
                                     "gam"  = pgamma(as.numeric(input$x2), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)) 
                                     - pgamma(as.numeric(input$x1), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)),
                                     "norm" = pnorm(as.numeric(input$x2), mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))) 
                                     - pnorm(as.numeric(input$x1), mean = as.numeric(input$normMean), sd = sqrt(as.numeric(input$normVar))),
                                     "unif" = punif(as.numeric(input$x2), as.numeric(input$theta1), as.numeric(input$theta2)) 
                                     - punif(as.numeric(input$x1), as.numeric(input$theta1), as.numeric(input$theta2)), 
                                     NULL
                              )
                            }
        )
        ) 
      } else if (input$probType == "lowerTail" && input$distType == "Discrete"){
        if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f is $$\\sum_{\\large{i \\, \\le \\, %.04f}}\\mathbb{P}(X = i) \\approx %.04f$$",
                            input$xFixed,
                            input$xFixed,
                            switch(input$distrib,
                                   bern = pbinom(as.numeric(input$xFixed), size = as.numeric(1), prob = as.numeric(input$p)),
                                   bin = pbinom(as.numeric(input$xFixed), size = as.numeric(input$numTrials), prob = as.numeric(input$p)),
                                   dunif = punifdisc(as.numeric(input$xFixed), as.numeric(input$a), as.numeric(input$b)),
                                   geom = pgeom(as.numeric(input$xFixed)-1, prob = as.numeric(input$p)),
                                   hgeom = phyper(as.numeric(input$xFixed), as.numeric(input$favBalls), 
                                                  as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                   nbin = pnbinom(as.numeric(input$xFixed - input$numSuccesses), 
                                                  as.numeric(input$numSuccesses), as.numeric(input$p)),
                                   poi = ppois(as.numeric(input$xFixed), as.numeric(input$lambda)),
                                   NULL
                            )
        )
        )        
      } else if (input$probType == "lowerTail" && input$distType == "Continuous"){
        if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f is $$\\int_{-\\infty}^{%.03f} f(x) \\, dx \\approx %.04f$$",
                            input$xFixed,
                            input$xFixed,
                            switch(input$distrib,
                                   exp = pgamma(as.numeric(input$xFixed), shape = as.numeric(1), scale = as.numeric(input$beta)),
                                   gam = pgamma(as.numeric(input$xFixed), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)),
                                   norm = pnorm(as.numeric(input$xFixed), mean = as.numeric(input$normMean), 
                                                sd = sqrt(as.numeric(input$normVar))),
                                   unif = punif(as.numeric(input$xFixed), as.numeric(input$theta1), as.numeric(input$theta2)),
                                   NULL
                            )
        )
        )        
      } else if (input$probType == "upperTail" && input$distType == "Discrete"){
        if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is greater than or equal to %.03f is $$1 - \\left[ \\sum_{\\large{i \\, \\le \\, %.04f}}\\mathbb{P}(X = i) \\right] \\approx %.04f$$",
                            input$xFixed,
                            input$xFixed - 1,
                            switch(input$distrib,
                                   "bern" = 1 - pbinom(as.numeric(input$xFixed)-1, size = as.numeric(1), prob = as.numeric(input$p)),
                                   "bin" = 1 - pbinom(as.numeric(input$xFixed)-1, size = as.numeric(input$numTrials), prob = as.numeric(input$p)),
                                   "dunif" = 1 - punifdisc(as.numeric(input$xFixed)-1, as.numeric(input$a), as.numeric(input$b)),
                                   "geom" = 1 - pgeom(as.numeric(input$xFixed)-2, prob = as.numeric(input$p)),
                                   "hgeom" = 1 - phyper(as.numeric(input$xFixed)-1, as.numeric(input$favBalls), 
                                                        as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                   "nbin" = 1 - pnbinom(as.numeric(input$xFixed - input$numSuccesses)-1, 
                                                        as.numeric(input$numSuccesses), as.numeric(input$p)),
                                   "poi" = 1 - ppois(as.numeric(input$xFixed)-1, as.numeric(input$lambda)),
                                   "exp" = 1 - pgamma(as.numeric(input$xFixed), shape = as.numeric(1), scale = as.numeric(input$beta)),
                                   "gam" = 1 - pgamma(as.numeric(input$xFixed), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)),
                                   "norm" = 1 - pnorm(as.numeric(input$xFixed), mean = as.numeric(input$normMean), 
                                                      sd = sqrt(as.numeric(input$normVar))),
                                   "unif" = 1 - punif(as.numeric(input$xFixed), as.numeric(input$theta1), as.numeric(input$theta2)),
                                   NULL
                            )
        )
        )   
      } else if (input$probType == "upperTail" && input$distType == "Continuous"){
        if(is.null(input$xFixed)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is greater than or equal to %.03f is $$1 - \\left[ \\int_{-\\infty}^{%.04f}f(x) \\, dx \\right] \\approx %.04f$$",
                            input$xFixed,
                            input$xFixed,
                            switch(input$distrib,
                                   "bern" = 1 - pbinom(as.numeric(input$xFixed)-1, size = as.numeric(1), prob = as.numeric(input$p)),
                                   "bin" = 1 - pbinom(as.numeric(input$xFixed)-1, size = as.numeric(input$numTrials), prob = as.numeric(input$p)),
                                   "dunif" = 1 - punifdisc(as.numeric(input$xFixed)-1, as.numeric(input$a), as.numeric(input$b)),
                                   "geom" = 1 - pgeom(as.numeric(input$xFixed)-2, prob = as.numeric(input$p)),
                                   "hgeom" = 1 - phyper(as.numeric(input$xFixed)-1, as.numeric(input$favBalls), 
                                                        as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)),
                                   "nbin" = 1 - pnbinom(as.numeric(input$xFixed - input$numSuccesses)-1, 
                                                        as.numeric(input$numSuccesses), as.numeric(input$p)),
                                   "poi" = 1 - ppois(as.numeric(input$xFixed)-1, as.numeric(input$lambda)),
                                   "exp" = 1 - pgamma(as.numeric(input$xFixed), shape = as.numeric(1), scale = as.numeric(input$beta)),
                                   "gam" = 1 - pgamma(as.numeric(input$xFixed), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)),
                                   "norm" = 1 - pnorm(as.numeric(input$xFixed), mean = as.numeric(input$normMean), 
                                                      sd = sqrt(as.numeric(input$normVar))),
                                   "unif" = 1 - punif(as.numeric(input$xFixed), as.numeric(input$theta1), as.numeric(input$theta2)),
                                   NULL
                            )
        )
        )   
      } else if(input$probType == "extreme" && input$distType == "Discrete"){
        if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f or greater than or equal to %.03f is $$\\sum\\limits_{\\large{i \\, \\leq \\, %.03f}} \\mathbb{P}(X = i) + \\sum\\limits_{\\large{j \\, \\geq \\, %.03f}} \\mathbb{P}(X = j) = \\mathbb{P}(X \\leq %.03f) + \\mathbb{P}(X \\geq %.03f) \\approx %.04f$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            switch(input$distrib,
                                   "bern" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pbinom(as.numeric(input$x2)-1, size = as.numeric(1), prob = as.numeric(input$p)) + pbinom(as.numeric(input$x1), size = as.numeric(1), prob = as.numeric(input$p))
                                   },
                                   "bin" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pbinom(as.numeric(input$x2)-1, size = as.numeric(input$numTrials), prob = as.numeric(input$p)) + pbinom(as.numeric(input$x1), size = as.numeric(input$numTrials), prob = as.numeric(input$p), lower.tail = TRUE)
                                   },
                                   "dunif" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - punifdisc(as.numeric(input$x2)-1, as.numeric(input$a), as.numeric(input$b)) + punifdisc(as.numeric(input$x1), as.numeric(input$a), as.numeric(input$b))
                                   },
                                   "geom" = if(input$x2 <= input$x1) 
                                   {1} else{ 
                                     1 - pgeom(as.numeric(input$x2)-2, prob = as.numeric(input$p)) + pgeom(as.numeric(input$x1)-1, prob = as.numeric(input$p))
                                   },
                                   "hgeom" = if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - phyper(as.numeric(input$x2)-1, as.numeric(input$favBalls), as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials)) + phyper(as.numeric(input$x1), as.numeric(input$favBalls), as.numeric((input$numEvents - input$favBalls)), as.numeric(input$numTrials))
                                   },
                                   "nbin" =if(input$x2 <= input$x1) 
                                   {1} else{
                                     1 - pnbinom(as.numeric(input$x2 - input$numSuccesses)-1, as.numeric(input$numSuccesses), as.numeric(input$p)) + pnbinom(as.numeric(input$x1), as.numeric(input$numSuccesses), as.numeric(input$p))
                                   },
                                   "poi" = if(input$x2 <= input$x1) 
                                   {1} else{ 
                                     1 - ppois(as.numeric(input$x2)-1, as.numeric(input$lambda)) + ppois(as.numeric(input$x1), as.numeric(input$lambda))
                                   },
                                   NULL
                            )
        )
        ) 
      } else if(input$probType == "extreme" && input$distType == "Continuous"){
        if(is.null(input$x1) || is.null(input$x2)) return ()
        withMathJax(sprintf("The probability that \\(X\\) is less than or equal to %.03f or greater than or equal to %.03f is $$\\int\\limits_{\\large{x \\, \\leq \\, %.03f}} f(x) \\, dx + \\int\\limits_{\\large{x \\, \\geq \\, %.03f}} f(x) \\, dx = \\mathbb{P}(X \\leq %.03f) + \\mathbb{P}(X \\geq %.03f) \\approx %.04f$$",
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            input$x1,
                            input$x2,
                            if(input$x2 <= input$x1) {1}
                            else{
                              switch(input$distrib,
                                     "exp" = 1 - pgamma(as.numeric(input$x2), shape = as.numeric(1), scale = as.numeric(input$beta)) 
                                     + pgamma(as.numeric(input$x1), shape = as.numeric(1), scale = as.numeric(input$beta)),
                                     "gam" = 1 - pgamma(as.numeric(input$x2), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)) 
                                     + pgamma(as.numeric(input$x1), shape = as.numeric(input$alpha), scale = as.numeric(input$beta)),
                                     "norm" = 1 - pnorm(as.numeric(input$x2), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))) 
                                     + pnorm(as.numeric(input$x1), as.numeric(input$normMean), sqrt(as.numeric(input$normVar))),
                                     "unif" = 1 - punif(as.numeric(input$x2), as.numeric(input$theta1), as.numeric(input$theta2)) 
                                     + punif(as.numeric(input$x1), as.numeric(input$theta1), as.numeric(input$theta2)),
                                     NULL
                              )
                            }
        )
        ) 
      }
      
      
    } #Ends if(input$outType == "Probability"){
  }) #Ends output$probCalcBetween <- renderUI({
  
  #Calculate the mean and output results in LaTeX style
  output$meanCalc <- renderUI({
    if(is.null(input$distrib)) return ()
    if(input$outType == "Mean"){
      switch(input$distrib,
             "dunif" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{a+b}{2} = \\frac{%.03f + %.03f}{2} = %.04f$$",
                                           input$a,
                                           input$b,
                                           (input$a + input$b)/2
             )), 
             
             "bern" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = %.04f$$",
                                          input$p
             )), 
             
             "bin" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = np = (%.03f) (%.03f) = %.04f$$",
                                         input$numTrials,
                                         input$p,
                                         (input$numTrials * input$p)
             )), 
             
             "geom" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{1}{p} = \\frac{1}{%.03f} = %.04f$$",
                                          input$p,
                                          (1 / input$p)
             )),
             
             "hgeom" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{nm}{N} = \\frac{%d \\cdot %d}{%d} = %.04f$$",
                                           input$numTrials,
                                           input$favBalls,
                                           input$numEvents,
                                           (input$numTrials * input$favBalls / input$numEvents)
             )),
             
             "nbin" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{r}{p} = \\frac{%d}{%0.3f} = %.04f$$",
                                          input$numSuccesses,
                                          input$p,
                                          (input$numSuccesses / input$p)
             )), 
             
             "poi" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\lambda = %.04f$$",
                                         input$lambda
             )), 
             
             #Continuous
             
             "exp" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\beta = %.04f$$",
                                         input$beta
             )),
             
             "gam" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\alpha \\beta = %d \\cdot %d  =  %.04f$$",
                                         input$alpha,
                                         input$beta,
                                         (input$alpha * input$beta)
             )),
             
             "norm" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\mu = %.04f$$",
                                          input$normMean
             )), 
             
             "unif" = withMathJax(sprintf("Mean is $$\\mathbb{E}(X) = \\frac{\\theta_1+\\theta_2}{2} = \\frac{%d + %d}{2} = %.04f$$",
                                          input$theta1,
                                          input$theta2,
                                          (input$theta1 + input$theta2)/2
             )), 
      )
    }
  })
  
  #Calculate the variance and output results in LaTeX style
  output$varCalc <- renderUI({
    if(is.null(input$distrib)) return ()
    if(input$outType == "Variance"){
      switch(input$distrib,
             "dunif" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{(b - a +1)^2 - 1}{12} = \\frac{(%.03f - %.03f + 1)^2 - 1}{12} = %.04f$$",
                                           input$b,
                                           input$a,
                                           ((input$b - input$a + 1)^2 -1)/12
             )),
             
             "bern" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = p(1-p) = (%.03f)(1-%.03f) = %.04f$$",
                                          input$p,
                                          input$p,
                                          (input$p*(1-input$p))
             )), 
             
             "bin" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = np(1-p) = (%d)(%.03f)(1-%.03f) = %.04f$$",
                                         input$numTrials,
                                         input$p,
                                         input$p,
                                         (input$numTrials * input$p * (1-input$p))
             )), 
             
             "geom" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{1-p}{p^2} = \\frac{1-%.03f}{(%.03f)^2} = %.04f$$",
                                          input$p,
                                          input$p,
                                          ((1-input$p) / (input$p)^2)
             )),
             
             "hgeom" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{N-n}{N-1}\\left(\\frac{nm}{N}\\right)\\left( 1-\\frac{m}{N} \\right) = \\frac{%d-%d}{%d-1}\\left(\\frac{%d \\cdot %d}{%d}\\right)\\left( 1-\\frac{%d}{%d} \\right)  = %.04f$$",
                                           input$numEvents,
                                           input$numTrials,
                                           input$numEvents,
                                           input$numTrials,
                                           input$favBalls,
                                           input$numEvents,
                                           input$favBalls,
                                           input$numEvents,
                                           ((input$numEvents - input$numTrials)/(input$numEvents-1)) * ((input$numTrials * input$favBalls)/(input$numEvents))*(1- input$favBalls/(input$numEvents))
             )),
             
             "nbin" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{r(1-p)}{p^2} = \\frac{%d(1-%.03f)}{(%.03f)^2} = %.04f$$",
                                          input$numSuccesses,
                                          input$p,
                                          input$p,
                                          ( (input$numSuccesses * (1-input$p) ) / ( (input$p)^2 ) )
             )),
             
             "poi" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\lambda = %.04f$$",
                                         input$lambda
             )), 
             
             #Continuous
             
             "exp" = withMathJax(sprintf("Mean is $$\\mathbb{V}(X) = \\beta^2 = %.04f$$",
                                         input$beta^2
             )),
             
             "gam" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\alpha \\beta^2 = %d (%d)^2  =  %.04f$$",
                                         input$alpha,
                                         input$beta,
                                         (input$alpha * input$beta^2)
             )),
             
             "norm" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\sigma^2 = %.04f$$",
                                          input$normVar
             )), 
             
             "unif" = withMathJax(sprintf("Variance is $$\\mathbb{V}(X) = \\frac{(\\theta_2-\\theta_1)^2}{12} = \\frac{(%d - %d)^2}{12} = %.04f$$",
                                          input$theta2,
                                          input$theta1,
                                          (input$theta2 - input$theta1)^2 /12
             )), 
      )
    }   
    
  })
  
  #Close
})







