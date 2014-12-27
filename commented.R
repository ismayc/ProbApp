# wellPanel(
#   radioButtons("distType",
#                        strong("Distribution Type:"),
#                        list("Discrete","Continuous"),
#                        selected="Discrete",
#                        inline=TRUE),
#  
#    uiOutput("distName")
#  ),
#Select distribution after choosing Discrete or Continuous type
#  conditionalPanel(
#    condition="input.distType=='Discrete'",
#       radioButtons(inputId = "distrib",
#                    label = "",#strong("Distribution:"),
#                    choices = c("Bernoulli"="bern",
#                         "Binomial"="bin",
#                         "Discrete Uniform"="dunif",
#                         "Geometric"="geom",
#                         "Hypergeometric"="hgeom",
#                         "Negative Binomial"="nbin",
#                         "Poisson"="poi",
#                         "Beta"="beta",
#                         #"Cauchy"="cauchy",
#                         "Chi-square"="chisq",
#                         "Exponential"="exp",
#                         "F"="f",
#                         "Gamma"="gam",
#                         #"Laplace"="lap",
#                         #"Logistic"="logi",
#                         #"Log-Normal"="lognorm",
#                         "Normal"="norm",
#                         #"Pareto"="pareto",
#                         "t"="t",
#                         "Uniform"="unif"),
#                    selected = "bin",
#                    inline = TRUE
#       )
#)
#,


#   conditionalPanel(
#     condition="input.distType=='Continuous'",
#       radioButtons(inputId = "distrib",
#                    label = strong("Distribution:"),
#                    choices = c("Beta"="beta",
#                         #"Cauchy"="cauchy",
#                         "Chi-square"="chisq",
#                         "Exponential"="exp",
#                         "F"="f",
#                         "Gamma"="gam",
#                         #"Laplace"="lap",
#                         #"Logistic"="logi",
#                         #"Log-Normal"="lognorm",
#                         "Normal"="norm",
#                         #"Pareto"="pareto",
#                         "t"="t",
#                         "Uniform"="unif"),
#                    selected = "beta",
#                    #"Weibull"="weib")
#                    inline = TRUE
#       )
#   )


#wellPanel(
# uiOutput("distName")
#  ) 
#,
#),







