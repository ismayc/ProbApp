# Expanded statistical audit: multiple parameter sets AND multiple x-values per
# distribution, comparing the app's displayed numbers to independent base-R refs.
suppressMessages({library(shiny); library(ggplot2); library(bslib)})
source("server.R")

strip <- function(x) paste(gsub("<[^>]+>", " ", as.character(x)), collapse = " ")
nums  <- function(s) as.numeric(regmatches(s, gregexpr("-?[0-9]+\\.?[0-9]*", s))[[1]])
# lastn understands both plain "0.0176" and scientific "3.487 \times 10^{-11}".
lastn <- function(s) {
  sci <- regmatches(s, gregexpr("(-?[0-9.]+)\\s*\\\\times\\s*10\\^\\{(-?[0-9]+)\\}", s, perl = TRUE))[[1]]
  if (length(sci)) {
    z <- sci[length(sci)]
    as.numeric(sub("^\\s*(-?[0-9.]+).*", "\\1", z)) *
      10^as.numeric(sub(".*\\{(-?[0-9]+)\\}.*", "\\1", z))
  } else { v <- nums(s); v[length(v)] }
}
valof <- function(s) as.numeric(sub(".*value of (-?[0-9.]+).*", "\\1", s))
P <- 0L; Fails <- character(0)
cmp <- function(label, app, ref, tol = 1.5e-3) {
  ok <- is.finite(app) && is.finite(ref) && abs(app - ref) <= tol
  if (ok) P <<- P + 1L
  else Fails <<- c(Fails, sprintf("%-30s app=%-12s ref=%-12s", label,
                   formatC(app, 6, format="g"), formatC(ref, 6, format="g")))
}

base_params <- list(pBG=.5,p=.5,numBinTrials=15,a=1,b=6,numEvents=10,numTrials=5,favBalls=3,
  pNeg=.5,numSuccesses=2,lambda=4,alpha=2,beta=5,alphaG=3,betaG=6,betaE=5,df=10,df1=5,df2=10,
  normMean=0,normVar=1,theta1=0,theta2=5)

# For each scenario: distrib, type, the parameter overrides (app input ids),
# the reference closures, and several x-values to probe.
disc <- c("bern","bin","dunif","geom","hgeom","nbin","poi")
scen <- list()
add <- function(nm, type, par, R, xs) scen[[length(scen)+1]] <<- list(nm=nm,type=type,par=par,R=R,xs=xs)

# ---- discrete ----
add("bern","Discrete", list(pBG=.3),
    list(d=function(x)dbinom(x,1,.3),p=function(x)pbinom(x,1,.3),mean=.3,var=.3*.7,q=function(a)qbinom(a,1,.3)),
    list(pc=c(0,1),L=c(0,1),U=c(1),pair=list(c(0,1))))
add("bin","Discrete", list(p=.3,numBinTrials=20),
    list(d=function(x)dbinom(x,20,.3),p=function(x)pbinom(x,20,.3),mean=20*.3,var=20*.3*.7,q=function(a)qbinom(a,20,.3)),
    list(pc=c(0,6,20),L=c(0,6,19),U=c(1,6,20),pair=list(c(3,9),c(0,20))))
add("dunif","Discrete", list(a=2,b=9),
    list(d=function(x)dunifdisc(x,2,9),p=function(x)punifdisc(x,2,9),mean=(2+9)/2,var=((9-2+1)^2-1)/12,q=function(a)qunifdisc(a,2,9)),
    list(pc=c(2,5,9),L=c(2,5,9),U=c(2,5,9),pair=list(c(3,7))))
add("geom","Discrete", list(pBG=.4),
    list(d=function(x)dgeom(x-1,.4),p=function(x)pgeom(x-1,.4),mean=1/.4,var=.6/.4^2,q=function(a)qgeom(a,.4)+1),
    list(pc=c(1,3,6),L=c(1,3,6),U=c(1,3,6),pair=list(c(2,5))))
add("hgeom","Discrete", list(numEvents=20,numTrials=7,favBalls=8),
    list(d=function(x)dhyper(x,8,12,7),p=function(x)phyper(x,8,12,7),mean=7*8/20,var=7*(8/20)*(1-8/20)*(20-7)/(20-1),q=function(a)qhyper(a,8,12,7)),
    list(pc=c(1,4,7),L=c(1,4,7),U=c(1,4,7),pair=list(c(2,6))))
add("nbin","Discrete", list(pNeg=.4,numSuccesses=3),
    list(d=function(x)dnbinom(x-3,3,.4),p=function(x)pnbinom(x-3,3,.4),mean=3/.4,var=3*.6/.4^2,q=function(a)qnbinom(a,3,.4)+3),
    list(pc=c(3,5,9),L=c(3,5,9),U=c(3,5,9),pair=list(c(4,8))))
add("poi","Discrete", list(lambda=7),
    list(d=function(x)dpois(x,7),p=function(x)ppois(x,7),mean=7,var=7,q=function(a)qpois(a,7)),
    list(pc=c(0,5,12),L=c(0,5,12),U=c(1,5,12),pair=list(c(3,10))))
# ---- continuous ----
add("beta","Continuous", list(alpha=3,beta=2),
    list(d=function(x)dbeta(x,3,2),p=function(x)pbeta(x,3,2),mean=3/5,var=3*2/(5^2*6),q=function(a)qbeta(a,3,2),cont=TRUE),
    list(pc=c(.3,.6,.9),L=c(.3,.6,.9),U=c(.2,.5,.8),pair=list(c(.2,.7))))
add("chisq","Continuous", list(df=6),
    list(d=function(x)dchisq(x,6),p=function(x)pchisq(x,6),mean=6,var=12,q=function(a)qchisq(a,6),cont=TRUE),
    list(pc=c(2,6,11),L=c(2,6,11),U=c(2,6,11),pair=list(c(3,9))))
add("exp","Continuous", list(betaE=3),
    list(d=function(x)dexp(x,1/3),p=function(x)pexp(x,1/3),mean=3,var=9,q=function(a)qexp(a,1/3),cont=TRUE),
    list(pc=c(1,3,7),L=c(1,3,7),U=c(1,3,7),pair=list(c(2,6))))
add("f","Continuous", list(df1=8,df2=12),
    list(d=function(x)df(x,8,12),p=function(x)pf(x,8,12),mean=12/10,var=2*12^2*(8+12-2)/(8*(12-2)^2*(12-4)),q=function(a)qf(a,8,12),cont=TRUE),
    list(pc=c(.5,1,3),L=c(.5,1,3),U=c(.5,1,3),pair=list(c(.5,2.5))))
add("gam","Continuous", list(alphaG=2,betaG=4),
    list(d=function(x)dgamma(x,2,scale=4),p=function(x)pgamma(x,2,scale=4),mean=8,var=2*16,q=function(a)qgamma(a,2,scale=4),cont=TRUE),
    list(pc=c(2,8,16),L=c(2,8,16),U=c(2,8,16),pair=list(c(4,14))))
add("norm","Continuous", list(normMean=2,normVar=4),
    list(d=function(x)dnorm(x,2,2),p=function(x)pnorm(x,2,2),mean=2,var=4,q=function(a)qnorm(a,2,2),cont=TRUE),
    list(pc=c(-1,2,5),L=c(-1,2,5),U=c(-1,2,5),pair=list(c(0,4))))
add("t","Continuous", list(df=8),
    list(d=function(x)dt(x,8),p=function(x)pt(x,8),mean=0,var=8/6,q=function(a)qt(a,8),cont=TRUE),
    list(pc=c(-2,0,2),L=c(-2,0,2),U=c(-2,0,2),pair=list(c(-1.5,1.5))))
add("unif","Continuous", list(theta1=-3,theta2=4),
    list(d=function(x)dunif(x,-3,4),p=function(x)punif(x,-3,4),mean=.5,var=49/12,q=function(a)qunif(a,-3,4),cont=TRUE),
    list(pc=c(-2,0,3),L=c(-2,0,3),U=c(-2,0,3),pair=list(c(-1,2))))

for (s in scen) {
  R <- s$R; cont <- isTRUE(R$cont)
  testServer(appServer, {
    do.call(session$setInputs, c(list(distType=s$type, distrib=s$nm, percentile="pdf", probType="lowerTail"),
                                 modifyList(base_params, s$par)))
    # PDF density / prob over several x
    for (x in s$xs$pc) { session$setInputs(outType="PDF", percentile="pdf", xFixedPC=x)
      cmp(sprintf("%s:pdf(%s)", s$nm, x), lastn(strip(output$distribCalc)), R$d(x)) }
    # quantiles
    for (a in c(.1,.5,.9)) { session$setInputs(outType="PDF", percentile="quant", quantile=a)
      cmp(sprintf("%s:q(%s)", s$nm, a), valof(strip(output$percentCalc)), R$q(a)) }
    # CDF over several x
    for (x in s$xs$L) { session$setInputs(outType="CDF", xFixedL=x)
      cmp(sprintf("%s:cdf(%s)", s$nm, x), lastn(strip(output$distribCalc)), R$p(x)) }
    # lowerTail
    for (x in s$xs$L) { session$setInputs(outType="Probability", probType="lowerTail", xFixedL=x)
      cmp(sprintf("%s:lower(%s)", s$nm, x), lastn(strip(output$probCalc)), R$p(x)) }
    # upperTail
    for (x in s$xs$U) { session$setInputs(outType="Probability", probType="upperTail", xFixedU=x)
      ref <- if (cont) 1 - R$p(x) else 1 - R$p(x - 1)
      cmp(sprintf("%s:upper(%s)", s$nm, x), lastn(strip(output$probCalc)), ref) }
    # between & extreme over pairs
    for (pr in s$xs$pair) { x1 <- pr[1]; x2 <- pr[2]
      session$setInputs(outType="Probability", probType="between", x1=x1, x2=x2)
      refB <- if (cont) R$p(x2)-R$p(x1) else R$p(x2)-R$p(x1-1)
      cmp(sprintf("%s:btwn(%s,%s)", s$nm, x1, x2), lastn(strip(output$probCalc)), refB)
      session$setInputs(probType="extreme", x1=x1, x2=x2)
      refE <- if (cont) R$p(x1)+(1-R$p(x2)) else R$p(x1)+(1-R$p(x2-1))
      cmp(sprintf("%s:extr(%s,%s)", s$nm, x1, x2), lastn(strip(output$probCalc)), refE) }
    # mean & variance
    session$setInputs(outType="Mean");     cmp(sprintf("%s:mean", s$nm), lastn(strip(output$meanCalc)), R$mean)
    session$setInputs(outType="Variance"); cmp(sprintf("%s:var", s$nm),  lastn(strip(output$varCalc)),  R$var)
  })
}
cat(sprintf("\n==== EXPANDED AUDIT: %d passed, %d FAILED ====\n", P, length(Fails)))
if (length(Fails)) cat(paste0("  ", Fails, collapse="\n"), "\n")
