#Create functions for discrete uniform distribution
dunifdisc <- function(x, min=0, max=1) ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
punifdisc <- function(q, min=0, max=1) ifelse(q<min, 0, ifelse(q>max, 1, floor(q)/(max-min+1)))
qunifdisc <- function(p, min=0, max=1) floor(p*(max-min+1))
runifdisc <- function(n, min=0, max=1) sample(min:max, n, replace=T)

#Function for plotting PDF and CDF
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
#  if(is.null(args[1]) || is.null(args[2]) || is.null(args[3])) return ()
   if(numArgs == 1 && is.null(args[1])) return ()
   else if(numArgs == 2 && (is.null(args[1]) || is.null(args[2]))) return ()
   else if(numArgs == 3 && (is.null(args[1]) || is.null(args[2])) || is.null(args[3])) return ()

  qplot(factor(range),
        if (numArgs == 1)
          func(range - paramAdjust, args[1])
        else if(numArgs == 2)
          func(range - paramAdjust, args[1], args[2])
        else
          func(range - paramAdjust, args[1], args[2], args[3])
        ,
        xlab = xlabel,
        ylab = paste(plotType, "Probability"),
        main = paste(distribName, mainLabel, "\n"),
        geom = "bar", 
        stat = "identity",
        fill = range == inputValue) +
    scale_fill_manual(values=c("#000000", "#00BA38")) + #To get the default shading colors to black with green                  
    guides(fill=FALSE) 
}


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
    areax1 <- seq(0, 
                  xmin, 
                  length.out = 1000)
    areax2 <- seq(xmax, 
                  qchisq(0.999, df), 
                  length.out = 1000)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dchisq(areax1, df = df))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dchisq(areax2, df = df))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Chi-Square Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dchisq(x, df = df)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

chisq_prob_CDF_plot <- function(lb, ub, df = 10, limits = c(0, qchisq(0.999, df))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pchisq(areax, df = df))
  
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Chi-Square Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pchisq(x, df = df)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
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
    areax1 <- seq(0, 
                  xmin, 
                  length.out = 1000)
    areax2 <- seq(xmax, 
                  qf(0.99, df1, df2), 
                  length.out = 1000)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = df(areax1, df1 = df1, df2 = df2))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = df(areax2, df1 = df1, df2 = df2))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("F Probability Density Function\n")
   + geom_line(data.frame(x = x, y = df(x, df1 = df1, df2 = df2)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

f_prob_CDF_plot <- function(lb, ub, df1 = 5, df2 = 10, limits = c(0, qf(0.99, df1, df2))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pf(areax, df1 = df1, df2 = df2))
  
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("F Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pf(x, df1 = df1, df2 = df2)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

#Functions for normal distribution shading (Modified from https://gist.github.com/jrnold/6799152) 
normal_prob_area_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(mean - 4 * sd), 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  ceiling(mean + 4 * sd), 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dnorm(areax1, mean = mean, sd = sd))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dnorm(areax2, mean = mean, sd = sd))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Normal Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                                             if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                                              else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
                        )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

normal_prob_CDF_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd)){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pnorm(areax, mean = mean, sd = sd))

  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Normal Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pnorm(x, mean = mean, sd = sd)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
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
    areax1 <- seq(qt(0.001, df), 
                  xmin, 
                  length.out = 1000)
    areax2 <- seq(xmax, 
                  qt(0.999, df), 
                  length.out = 1000)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dt(areax1, df = df))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dt(areax2, df = df))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Student's t Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dt(x, df = df)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

t_prob_CDF_plot <- function(lb, ub, df = 10, limits = c(qt(0.001, df), qt(0.999, df))){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pt(areax, df = df))
  
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Student's t Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pt(x, df = df)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
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
    areax1 <- seq(ceiling(min - 1), 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  ceiling(max + 1), 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dunif(areax1, min = min, max = max))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dunif(areax2, min = min, max = max))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Uniform Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dunif(x, min = min, max = max)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

uniform_prob_CDF_plot <- function(lb, ub = max + 1, min, max, limits = c(min - 1, max + 1)) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = punif(areax, min = min, max = max))
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Uniform Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = punif(x, min = min, max = max)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
  + scale_fill_manual(values=c("black"))
  + guides(fill=FALSE)
  )
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
    areax1 <- seq(ceiling(0), 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  ceiling(qgamma(0.999, shape=1, scale=scale)), 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dgamma(areax1, shape = 1, scale = scale))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dgamma(areax2, shape = 1, scale = scale))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Exponential Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dgamma(x, shape = 1, scale = scale)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

exp_prob_CDF_plot <- function(lb, ub = qgamma(0.999, shape=1, scale=scale), shape = 1, scale = 1, limits = c(0, qgamma(0.999, shape=1, scale=scale))) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pgamma(areax, shape = shape, scale = scale))
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Exponential Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pgamma(x, shape = shape, scale = scale)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
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
    areax1 <- seq(ceiling(0), 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  ceiling(qgamma(0.999, shape = shape, scale = scale)), 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dgamma(areax1, shape = shape, scale = scale))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dgamma(areax2, shape = shape, scale = scale))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Gamma Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dgamma(x, shape = shape, scale = scale)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

gamma_prob_CDF_plot <- function(lb, ub = qgamma(0.999, shape=shape, scale=scale), shape = 1, scale = 1, 
                                limits = c(0, qgamma(0.999, shape=shape, scale=scale))) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pgamma(areax, shape = shape, scale = scale))
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Gamma Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pgamma(x, shape = shape, scale = scale)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
  )
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
    areax1 <- seq(0, 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  1, 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dbeta(areax1, shape1 = shape1, shape2 = shape2))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dbeta(areax2, shape1 = shape1, shape2 = shape2))
  (ggplot()
   + xlab("x")
   + ylab("Density")
   + ggtitle("Beta Probability Density Function\n")
   + geom_line(data.frame(x = x, y = dbeta(x, shape1 = shape1, shape2 = shape2)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area1, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + geom_ribbon(data = area2, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
   + scale_fill_manual(values=c("black"))
   + guides(fill=FALSE))
}

beta_prob_CDF_plot <- function(lb, ub, shape1, shape2, limits = c(0, 1)) {
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pbeta(areax, shape1 = shape1, shape2 = shape2))
  (ggplot()
   + xlab("x")
   + ylab("Cumulative Probability")
   + ggtitle("Beta Cumulative Distribution Function\n")
   + geom_line(data.frame(x = x, y = pbeta(x, shape1 = shape1, shape2 = shape2)),
               mapping = aes(x = x, y = y))
   + geom_ribbon(data = area, 
                 mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                 fill = "#00BA38")
   + scale_x_continuous(limits = limits, breaks = 
                          if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                        else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
   )
  )
}
