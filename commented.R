#PDF
#"bern" =              
  #                qplot(factor(0:1), 
  #                             dbinom(0:1, 1, input$p),
  #                             xlab = "Number of Successes", 
  #                             ylab = "Probability", 
  #                             main = "Bernoulli Probability Mass Function\n",
  #                             geom = "bar", 
  #                             stat = "identity",
  #                             fill = 0:1 == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE), 
  
#"bin" = 
  #                    qplot(factor(0:input$numTrials), 
  #                            dbinom(0:input$numTrials, input$numTrials, input$p),
  #                            xlab = "Number of Successes", 
  #                            ylab = "Probability", 
  #                            main = "Binomial Probability Mass Function\n",
  #                            geom = "bar", 
  #                            stat = "identity",
  #                            fill = 0:input$numTrials == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE), 

#"dunif" = 
  #                       qplot(factor(input$a:input$b), 
  #                              dunifdisc(input$a:input$b, input$a, input$b),
  #                              xlab = "Number of Successes", 
  #                              ylab = "Probability", 
  #                              main = "Discrete Uniform Probability Mass Function\n",
  #                              geom = "bar", 
  #                              stat = "identity",
  #                              fill = input$a:input$b == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"geom" = 
  #                qplot(factor(1:ceiling(4*1/input$p)),  ##### How large to set the bounds?
  #                                          dgeom(1:ceiling(4*1/input$p)-1, input$p),
  #                                          xlab = "Number of Trials", 
  #                                          ylab = "Probability", 
  #                                          main = "Geometric Probability Mass Function\n",
  #                                          geom = "bar", 
  #                                          stat = "identity",
  #                                          fill = 1:ceiling(4*1/input$p) == input$xFixed)
  #                           + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #                           + guides(fill=FALSE),
  
#"hgeom" =
  #               qplot(factor(0:input$favBalls), 
  #                              dhyper(0:input$favBalls, input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
  #                              xlab = "Number of Successes", 
  #                              ylab = "Probability", 
  #                              main = "Hypergeometric Probability Mass Function\n",
  #                              geom = "bar", 
  #                              stat = "identity",
  #                              fill = 0:input$numTrials == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),

#"nbin" =
  #                qplot(factor(input$numSuccesses:ceiling(7*1/input$p)), 
  #                                          dnbinom(input$numSuccesses:ceiling(7*1/input$p) - input$numSuccesses, input$numSuccesses, input$p),
  #                                          xlab = "Number of Trials", 
  #                                          ylab = "Probability", 
  #                                          main = "Negative Binomial Probability Mass Function\n",
  #                                          geom = "bar", 
  #                                          stat = "identity",
  #                                          fill = input$numSuccesses:ceiling(7*1/input$p) == input$xFixed)
  #                           + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #                           + guides(fill=FALSE), 
  
#"poi" =
  #                         qplot(factor(0:ceiling(4*input$lambda)),   ##### Not sure on the bounds here either?
  #                            dpois(0:ceiling(4*input$lambda), input$lambda),
  #                            xlab = "Number of Occurrences", 
  #                            ylab = "Probability", 
  #                            main = "Poisson Probability Mass Function\n",
  #                            geom = "bar", 
  #                            stat = "identity",
  #                            fill = 0:ceiling(4*input$lambda) == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),

#CDF
#"bern" =     
  #                qplot(factor(0:1), 
  #                      pbinom(0:1, 1, input$p),
  #                      xlab = "Number of Successes", 
  #                      ylab = "Cumulative Probability", 
  #                      main = "Bernoulli Cumulative Distribution Function\n",
  #                      geom= "bar", 
  #                      stat= "identity",
  #                      fill = 0:1 == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"bin" = 
  #                qplot(factor(0:input$numTrials), 
  #                      pbinom(0:input$numTrials, input$numTrials, input$p),
  #                      xlab = "Number of Successes", 
  #                      ylab = "Cumulative Probability", 
  #                      main = "Binomial Cumulative Distribution Function\n",
  #                      geom = "bar", 
  #                      stat = "identity",
  #                      fill = 0:input$numTrials == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"dunif" = 
  #                qplot(factor(input$a:input$b), 
  #                              punifdisc(input$a:input$b, input$a, input$b),
  #                              xlab = "Number of Successes", 
  #                              ylab = "Cumulative Probability", 
  #                              main = "Discrete Uniform Cumulative Distribution Function\n",
  #                              geom = "bar", 
  #                              stat = "identity",
  #                              fill = input$a:input$b == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"geom" = 
  #                qplot(factor(1:ceiling(4*1/input$p)),  #####Not sure what to put for limits here?
  #                             pgeom(1:ceiling(4*1/input$p)-1,input$p),
  #                             xlab = "Number of Failures Before Success Occurs", 
  #                             ylab = "Cumulative Probability", 
  #                             main = "Geometric Cumulative Distribution Function\n",
  #                             geom = "bar", 
  #                             stat = "identity",
  #                             fill = 1:ceiling(4*1/input$p) == input$xFixed
  #              )
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"hgeom" =
  #                qplot(factor(0:input$favBalls), 
  #                              phyper(0:input$favBalls, input$favBalls, (input$numEvents - input$favBalls), input$numTrials),
  #                              xlab = "Number of Successes", 
  #                              ylab = "Cumulative Probability", 
  #                              main = "Hypergeometric Cumulative Distribution Function\n",
  #                              geom = "bar", 
  #                              stat = "identity",
  #                              fill = 0:input$favBalls == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE), 
  
  
#"nbin" =
  #                qplot(factor(input$numSuccesses:ceiling(7*1/input$p)), 
  #                             pnbinom(input$numSuccesses:ceiling(7*1/input$p) - input$numSuccesses, input$numSuccesses, input$p),
  #                             xlab = "Number of Successes", 
  #                             ylab = "Cumulative Probability", 
  #                             main = "Negative Binomial Cumulative Distribution Function\n",
  #                             geom = "bar", 
  #                             stat = "identity",
  #                             fill = input$numSuccesses:ceiling(7*1/input$p) == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE),
  
#"poi" =
  #                qplot(factor(0:ceiling(4*input$lambda)),   ##### Not sure on the bounds here either?
  #                            ppois(0:ceiling(4*input$lambda), input$lambda),
  #                            xlab = "Number of Occurrences", 
  #                            ylab = "Cumulative Probability", 
  #                            main = "Poisson Cumulative Distribution Function\n",
  #                            geom = "bar", 
  #                            stat = "identity",
  #                            fill = 0:ceiling(4*input$lambda) == input$xFixed)
  #              + scale_fill_manual(values=c("#000000", "#00BA38")) #To get the default shading colors to black with green
  #              + guides(fill=FALSE), 
