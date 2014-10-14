
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Calculator for Probability Distributions"),
  
  # Sidebar
  sidebarPanel(
    wellPanel(radioButtons("distType",strong("Distribution Type:"),
                           list("Discrete","Continuous"),
                           selected="Discrete") ),
    wellPanel(uiOutput("distName") ),
    
    #Select output type
    wellPanel(radioButtons("outType", strong("Output Type:"), 
                           list("Formulas", "PDF/Quantile" = "PDF", 
                                "CDF", "Probability", "Mean", "Variance"), 
                           selected="Probability"))
  ),
  
  # Output results of calculations & plots
  mainPanel(
    withMathJax(),
    uiOutput("formulas"),
    
    uiOutput("param1"),
    uiOutput("param2"),
    uiOutput("param3"),
    
    uiOutput("meanCalc"),
    uiOutput("varCalc"),
    
    plotOutput("distribPlot"),
    
    uiOutput("percentile"),
    uiOutput("percentBox"),
    uiOutput("percentCalc"),
    uiOutput("fixedVal1"),
    uiOutput("fixedVal2"),

    
    uiOutput("distribCalc"),
    uiOutput("probTypeSelect"),
    uiOutput("probCalc")
    
  )
)
)
