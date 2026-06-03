suppressMessages({library(shiny);library(ggplot2);library(bslib);library(plotly)})
source("server.R")
strip <- function(x) paste(gsub("<[^>]+>"," ",as.character(x)),collapse=" ")
S <- function(o) tryCatch({v<-strip(o); if(length(v)==0)"" else v}, error=function(e) paste("ERR:",conditionMessage(e)))
nums <- function(s){ sci <- regmatches(s, gregexpr("(-?[0-9.]+)\\s*\\\\times\\s*10\\^\\{(-?[0-9]+)\\}", s, perl=TRUE))[[1]]
  if(length(sci)){ z<-sci[length(sci)]; return(as.numeric(sub("^\\s*(-?[0-9.]+).*","\\1",z))*10^as.numeric(sub(".*\\{(-?[0-9]+)\\}.*","\\1",z))) }
  v<-as.numeric(regmatches(s,gregexpr("-?[0-9]+\\.?[0-9]*",s))[[1]]); v[length(v)] }

params <- list(weib=list(weibShape=2,weibScale=3), lnorm=list(lnMeanlog=0,lnSdlog=1),
  cauchy=list(cauchyLoc=0,cauchyScale=1), logis=list(logisLoc=0,logisScale=2),
  pareto=list(paretoScale=1,paretoShape=3), laplace=list(laplaceLoc=0,laplaceScale=1))
xv <- list(weib=2.5, lnorm=1.5, cauchy=0.5, logis=1, pareto=2, laplace=0.5)
# independent references
ref <- list(
  weib =list(d=function(x)dweibull(x,2,3),p=function(x)pweibull(x,2,3),q=function(p)qweibull(p,2,3),mean=3*gamma(1.5),var=9*(gamma(2)-gamma(1.5)^2)),
  lnorm=list(d=function(x)dlnorm(x,0,1),p=function(x)plnorm(x,0,1),q=function(p)qlnorm(p,0,1),mean=exp(0.5),var=(exp(1)-1)*exp(1)),
  cauchy=list(d=function(x)dcauchy(x,0,1),p=function(x)pcauchy(x,0,1),q=function(p)qcauchy(p,0,1),mean=NA,var=NA),
  logis=list(d=function(x)dlogis(x,0,2),p=function(x)plogis(x,0,2),q=function(p)qlogis(p,0,2),mean=0,var=4*pi^2/3),
  pareto=list(d=function(x)ifelse(x>=1,3*1/x^4,0),p=function(x)ifelse(x>=1,1-(1/x)^3,0),q=function(p)1/(1-p)^(1/3),mean=3*1/2,var=3*1/(4*1)),
  laplace=list(d=function(x)exp(-abs(x))/2,p=function(x)ifelse(x<0,0.5*exp(x),1-0.5*exp(-x)),q=function(p)ifelse(p<0.5,log(2*p),-log(2*(1-p))),mean=0,var=2))
P<-0L; F<-0L
chk <- function(l,a,b,tol=2e-3){ ok<-isTRUE(is.finite(a)&&is.finite(b)&&abs(a-b)<=tol); if(ok)P<<-P+1L else {F<<-F+1L; cat(sprintf("  FAIL %s app=%s ref=%s\n",l,format(a),format(b)))} }

for (d in names(params)) {
  R <- ref[[d]]; x <- xv[[d]]
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType="Continuous", distrib=d, percentile="pdf", probType="lowerTail",
      xFixedPC=x, xFixedL=x, xFixedU=x, x1=x-0.5, x2=x+0.5, quantile=0.5), params[[d]]))
    session$setInputs(outType="Formulas"); fok <- nchar(S(output$formulas))>20
    session$setInputs(outType="PDF", percentile="pdf"); chk(paste0(d,":pdf"), nums(S(output$distribCalc)), R$d(x)); plok <- !is.null(output$distribPlot)
    session$setInputs(percentile="quant", quantile=0.5); chk(paste0(d,":quant"), as.numeric(sub(".*value of (-?[0-9.eE+-]+).*","\\1",S(output$percentCalc))), R$q(0.5))
    session$setInputs(outType="CDF"); chk(paste0(d,":cdf"), nums(S(output$distribCalc)), R$p(x))
    session$setInputs(outType="Probability", probType="lowerTail"); chk(paste0(d,":lower"), nums(S(output$probCalc)), R$p(x))
    session$setInputs(probType="upperTail"); chk(paste0(d,":upper"), nums(S(output$probCalc)), 1-R$p(x))
    session$setInputs(probType="between"); chk(paste0(d,":between"), nums(S(output$probCalc)), R$p(x+0.5)-R$p(x-0.5))
    session$setInputs(probType="extreme"); chk(paste0(d,":extreme"), nums(S(output$probCalc)), R$p(x-0.5)+(1-R$p(x+0.5)))
    session$setInputs(outType="Mean"); mn <- S(output$meanCalc)
    session$setInputs(outType="Variance"); vr <- S(output$varCalc)
    if (is.na(R$mean)) { if(grepl("undefined",mn)) P<<-P+1L else {F<<-F+1L; cat("  FAIL",d,"mean not undefined\n")} } else chk(paste0(d,":mean"), nums(mn), R$mean)
    if (is.na(R$var))  { if(grepl("undefined",vr)) P<<-P+1L else {F<<-F+1L; cat("  FAIL",d,"var not undefined\n")} } else chk(paste0(d,":var"), nums(vr), R$var)
    cat(sprintf("%-8s formula:%s plot:%s\n", d, fok, plok))
  })
}
cat(sprintf("\n==== NEW-DIST AUDIT: %d passed, %d FAILED ====\n", P, F))
