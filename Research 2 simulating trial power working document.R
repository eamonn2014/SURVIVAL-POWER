

# trying survival simulation
# https://stats.stackexchange.com/questions/159080/how-to-simulate-censored-data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p0 <- function(x) {formatC(x, format="f", digits=1)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p5 <- function(x) {formatC(x, format="f", digits=5)}

# lets simulate survival with fixed censoring, this is from medstats
# https://mail.google.com/mail/u/0/#search/%7Bmedstats%7D+difference+between+two+very+small+proportions/WhctKJWJCVkgRmhDdCGMqvjWqDbRFqMphWjJKDbPJGshTKwcCWtKwCtPkjnTsNDGNrttvwl

# spits out pvalues
library(survival)
sim = function(n_trt, n_ctrl, m_trt, m_ctrl, t_censoring) {
  
  # Simulate some data
  t_trt =  rexp(n_trt, log(2)/m_trt)
  t_ctrl = rexp(n_ctrl, log(2)/m_ctrl)
  t = c(t_trt, t_ctrl)
  
  # Add censoring
  died = (t <= t_censoring)
  t_after_censoring = pmin(t, t_censoring)
  
  # Fit Cox model
  gr = c(rep(1, n_trt), rep(0, n_ctrl))
  l = coxph(Surv(t_after_censoring, died) ~ gr)
  anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
}

set.seed(123) # Reproducible pseudorandom numbers
n_sim = 1000  # Number of simulations (increase this for more accuracy)
p_med = replicate(n_sim, sim(n_trt = 100, n_ctrl = 100,
                             m_trt = 7, m_ctrl = 4,
                             t_censoring = 26))



# power
p_hr = replicate(n_sim, sim(n_trt = 84, n_ctrl = 100,
                            m_trt = 4.4/0.54, m_ctrl = 4.4,
                            t_censoring = 24))
mean(p_med <= 0.05) # Estimated power using median
#> [1] 0.901
mean(p_hr <= 0.05)  # Estimated power using control median + HR
#> [1] 0.88




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# a function to simulate 2 curves
# followed by a plotly plot

n=100  # per group
# functions to plot true survival function curves red dash
weibSurv <- function(t, shape, scale)   pweibull(t, shape=shape, scale=scale, lower.tail=F)
#gSurv     <- function(t, shape, scale) pgompertz(t, shape=p,     rate = h,    lower.tail=F)

par(mfrow=c(1,2))  

# simulate weibull shape parameter p, scale = h
p=1# 1/3    # shape, if 1 this is exponential
h=0.05   # scale, smaller means longer times
# median survival
ms <- 1/h*(-log(1-0.5))^(p)

#~~~~~~~~~~~~~~~~~~~~~~~~~~
t <- h^-1*(-log(runif(n)))^p # weibull random times my code
 
## Calculate survival times from here
## https://stats.stackexchange.com/questions/65005/get-a-desired-percentage-of-censored-observations-in-a-simulation-of-cox-ph-mode
T1 <- 1/h*(-log(runif(n))  )^(p)

summary(t) ; summary(T1)

survfit <- survfit(Surv(T1) ~ 1)
plot(survfit, ylab="Survival probability", xlab="Time", 
     main=paste0("N = ",n,", Weibull, shape = ",1/p,", rate = ",h, "\n med surv=",ms))

# plot true survival curve for weibull
curve(weibSurv(x, shape=1/p, scale=1/h), from=0, to=max(t), n=length(t), 
      col='red', lwd=2, lty=2,
      ylim=c(0,1), add=TRUE)
abline(v=ms, lty=2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## t2 , lets say median survival is 10% longer

hr <- 1.75
ms2 <-ms*hr     # this is the HR
## so hazard is 
h <- log(2)/ms2

# new times
T2 <- 1/h*(-log(runif(n))  )^(p)

survfit <- survfit(Surv(T2) ~ 1)
plot(survfit, ylab="Survival probability", xlab="Time", 
     main=paste0("N = ",n,", Weibull, shape = ",1/p,", rate = ",h, "\n med surv=",ms2))

# plot true survival curve for weibull
curve(weibSurv(x, shape=1/p, scale=1/h), from=0, to=max(t), n=length(t), 
      col='red', lwd=2, lty=2,
      ylim=c(0,1), add=TRUE)
abline(v=ms2, lty=2)

par(mfrow=c(1,1))  

###administration centering

prec.c = .3
event = rbinom(n, 1, 1-prec.c) # so 1s are events

dat <- data.frame(T = c(T1,T2) ,x = c(rep(1,n), rep(0,n)), event = event )

At <- 100  # admin censoring time
## Also, all T's > 30 yrs are by definition censored and T is set to 30 yrs
dat$event <- ifelse(dat$T >= At, 0, dat$event)
dat$T <- ifelse(dat$T >= At, At, dat$T)

 
l = coxph(Surv(T, event) ~ x, dat)
l
anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f <- survfit(Surv(T, event) ~ x, dat)

require(plotly)
require(rms)
library(survminer)

p1 <- ggsurvplot(f, title = paste0("Kaplan-Meier Curve, true HR= ",hr ),
        
                 legend.title = "Trt."
                 
                 ,xlab=paste0("Time : HR=",exp(l$coefficients)[[1]], 
                              " P-value=",anova(l)$`Pr(>|Chi|)`[2])
 )

ggplotly(p1[[1]] )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# what about this 

# With at least 100 patients per group, 
# there was approximately 94% power to detect a 3-day difference 
# in median time to otorrhea cessation between Moxidex plus TT and TT only. 
# This was based on the assumption that the median time to cessation of otorrhea 
# for patients receiving Moxidex plus TT would be similar to the 4-day median time 
# to cessation of otorrhea observed in all Alcon-conducted studies of CIPRODEX.
# All power and sample size calculations were based on the assumption of an 
# exponential survival distribution for the 2 groups and the application of a 2-sided
# test at the a = 0.05 level of significance. 

## my answer?

phr = replicate(1000, w.sim2(p=1, hr=1/1.75, n=100, At=3000, ms=4, lambdaC=.06)) # n
phr <- t(phr)
mean(phr[,1] <= 0.05)      

phr = replicate(1000, w.sim2(p=1, hr=1.75, n=100, At=3000, ms=7, lambdaC=.0006)) # n
phr <- t(phr)
mean(phr[,1] <= 0.05) 

## make a function##

 

# n=100  # per group
# # functions to plot true survival function curves red dash
#  
# # simulate weibull shape parameter p, scale = h
# p=1# 1/3    # shape, if 1 this is exponential
# h=0.05   # scale, smaller means longer times
# # median survival
# At <- 100  # admin censoring time
# 
# hr <- 1.75
# prec.c = .3 ###administration centering
# _hr


sur.sim <- function(h, p, hr, n, prec.c, At, x) {
  
  ms <- 1/h*(-log(1-0.5))^(p)
  ms2 <-ms*hr     # this is the HR
 
  h <-  log(2)/ms    ## so hazard is this
  T1 <- 1/h*(-log(runif(n))  )^(p)
  
  h2 <- log(2)/ms2
  T2 <- 1/h2*(-log(runif(n))  )^(p)
   
  event = rbinom(2*n, 1, 1-prec.c) # so 1s are events
  
  dat <- data.frame(T = c(T1,T2) ,x = c(rep(1,n), rep(0,n)), event = event )
  dat$event <- ifelse(dat$T >= At, 0, dat$event)
  dat$T <-     ifelse(dat$T >= At, At, dat$T)
  
  l = coxph(Surv(T, event) ~ x, dat)
  anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test


}
phr = replicate(1000, sur.sim(h=.005, p=1, hr=1.8, n=50, prec.c=.2, At=20000))
mean(phr <= 0.05) # E

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# just print and plot a single realization!
sur.sim2 <- function(h, p, hr, n, prec.c, At, x) {
  
  ms <- 1/h*(-log(1-0.5))^(p)
  ms2 <-ms*hr     # this is the HR
  
  h <-  log(2)/ms    ## so hazard is this
  T1 <- 1/h*(-log(runif(n))  )^(p)
  
  h2 <- log(2)/ms2
  T2 <- 1/h2*(-log(runif(n))  )^(p)
  
  event = rbinom(2*n, 1, 1-prec.c) # so 1s are events
  
  dat <- data.frame(T = c(T1,T2) ,x = c(rep(1,n), rep(0,n)), event = event )
  dat$event <- ifelse(dat$T >= At, 0, dat$event)
  dat$T <-     ifelse(dat$T >= At, At, dat$T)
  
  l = coxph(Surv(T, event) ~ x, dat)
  anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
  
  f <- survfit(Surv(T, event) ~ x, dat)
   p1 <- ggsurvplot(f, title = paste0("Kaplan-Meier Curve, true HR= ",hr ),
                   
                   legend.title = "Trt."
                   
                   ,xlab=paste0("Time : HR=",exp(l$coefficients)[[1]], 
                                " P-value=",anova(l)$`Pr(>|Chi|)`[2])
  )
  
  print(ggplotly(p1[[1]] ))
  
  print(l)
  
}
# med surv = 5.5, so constant hazard log(2)/5.5 if p =1 as well 

phr = replicate(1000,sur.sim(h=log(2)/5.5, p=1, hr=1/0.8, n=350, prec.c=.25, At=20000))
mean(phr <= 0.10) # E

phr = sur.sim2(h=log(2)/5.5, p=1, hr=0.8, n=350, prec.c=.25, At=20000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# better, censoring is weibull
# https://stats.stackexchange.com/questions/159080/how-to-simulate-censored-data

weibsurv <- function(t, shape ,scale) {pweibull(t,shape=shape, scale=scale,
                                                lower.tail=FALSE)}

n=100  # per group
# functions to plot true survival function curves red dash
weibSurv <- function(t, shape, scale)   pweibull(t, shape=shape, scale=scale, lower.tail=F)

# simulate weibull shape parameter p, scale = h
p=1# 1/3    # shape, if 1 this is exponential
h=0.05   # scale, smaller means longer times


# we are told median survival is 5.5 , so hazard is log(2)/5.5
ms = 5.5
h=log(2)/ms
hr=.8

ms <- 1/h*(-log(1-0.5))^(p)
ms2 <-ms*hr     # use the HR to get hazard in othe group

T1 <- 1/h*(-log(runif(n))  )^(p)  #weibull times, p=1 so exp, lambda=h

h2 <- log(2)/ms2
T2 <- 1/h2*(-log(runif(n))  )^(p)  #weibull times, p=1 so exp, lambda=h2

# CENSORING
At = 30 # administrative censoring   

lambdaC = .04  # hazard of censoring
C1 = rweibull(n, shape=1, scale=1/lambdaC)   #censoring time
C2 = rweibull(n, shape=1, scale=1/lambdaC)   #censoring time

time1 = pmin(T1,C1)  #observed time is min of censored and true
time2 = pmin(T2,C2)  #observed time is min of censored and true

event1 = time1==T1   
event2 = time2==T2   
event = c(event1,event2)
table( event)[1] [[1]] / (n*2)

dat <- data.frame(T = c(time1,time2) ,x = c(rep(1,n), rep(0,n)), event = c(event1,event2) )

survfit <- survfit(Surv(T,event = event) ~ x, dat)

plot(survfit, ylab="Survival probability", xlab="Time", 
     main=paste0("N = ",n,", Weibull, shape = ",1/p,", rate = ",h, " med surv=",ms, 
                 "\nrate = ",h2, " med surv=",ms2, " so hr=",ms/ms2
                 ))

# plot true survival curve for weibulls
curve(weibSurv(x, shape=1/p, scale=1/h), from=0, to=max(t), n=length(t), 
      col='blue', lwd=2, lty=2,
      ylim=c(0,1), add=TRUE)
abline(v=ms, lty=2)

curve(weibSurv(x, shape=1/p, scale=1/h2), from=0, to=max(t), n=length(t), 
      col='red', lwd=2, lty=2,
      ylim=c(0,1), add=TRUE)
abline(v=ms2, lty=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

coxph(Surv(T, event)  ~ x, dat)


# make a function




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# better, censoring is weibull, function to get one realisation
# https://stats.stackexchange.com/questions/159080/how-to-simulate-censored-data

w.sim <- function(p=1, hr=.8, n=100,   At=30, ms=5.5, lambdaC=.04) {  # n per grp, p=1, scale so exponetial

    # functions to plot true survival function curves red dash
    weibSurv <- function(t, shape, scale) pweibull(t, shape=shape, scale=scale, lower.tail=F)
     
    h=log(2)/ms
    ms <- 1/h*(-log(1-0.5))^(p)
    T1 <- 1/h*(-log(runif(n))  )^(p)  #weibull times, p=1 so exp, lambda=h
    
    ms2 <-ms*hr     # use the HR to get hazard in othe group
    h2 <- log(2)/ms2
    T2 <- 1/h2*(-log(runif(n))  )^(p)  #weibull times, p=1 so exp, lambda=h2
    
    # lambdaC hazard of censoring
    C1 = rweibull(n, shape=1, scale=1/lambdaC)   #censoring time
    C2 = rweibull(n, shape=1, scale=1/lambdaC)   #censoring time
    
    time1 = pmin(T1,C1)  #observed time is min of censored and true
    time2 = pmin(T2,C2)  #observed time is min of censored and true
    
    event1 = time1==T1   
    event2 = time2==T2   
    event = c(event1,event2)
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dat <- data.frame(T = c(time1,time2) ,x = c(rep(1,n), rep(0,n)), event = c(event1,event2) )
    
    survfit <- survfit(Surv(T,event = event) ~ x, dat)
    
    plot(survfit, ylab="Survival probability", xlab="Time", 
         main=paste0("N = ",n*2,", Weibull, shape = ",1/p,", rate = ",p3(h), " med surv=",ms, 
                     "\nrate = ",p3(h2), " med surv=",ms2, " so hr=",ms/ms2
         ))
    
    # plot true survival curve for weibulls
    curve(weibSurv(x, shape=1/p, scale=1/h), from=0, to=max(t), n=length(t), 
          col='blue', lwd=2, lty=2,
          ylim=c(0,1), add=TRUE)
    abline(v=ms, lty=2)
    
    curve(weibSurv(x, shape=1/p, scale=1/h2), from=0, to=max(t), n=length(t), 
          col='red', lwd=2, lty=2,
          ylim=c(0,1), add=TRUE)
    abline(v=ms2, lty=2)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    coxph(Surv(T, event)  ~ x, dat)

}

    # p=1 means exponential haz, 
    w.sim(p=1, hr=.8, n=100,  At=30, ms=5.5, lambdaC=.04)  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # power simulation
    
    w.sim2 <- function(p=1, hr=.8, n=100, At=30, ms=5.5, lambdaC=.04) { 

      h  <- log(2)/ms                    # hazard for exponential
      ms <- 1/h*(-log(1-0.5))^(p)        # median survival
      T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
      
      ms2<- ms*hr                        # use the HR to get hazard in other group
      h2 <- log(2)/ms2
      T2 <- 1/h2*(-log(runif(n)))^(p)    # weibull times, p=1 so exp, lambda=h2
      
      # lambdaC hazard of censoring
      C1 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      C2 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      
      time1 = pmin(T1,C1)  # observed time is min of censored and true grp0
      time2 = pmin(T2,C2)  # observed time is min of censored and true grp1
      
      event1 = time1==T1   
      event2 = time2==T2   
      
      event = c(event1,event2)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dat <- data.frame(T = c(time1,time2), 
                        x = c(rep(1,n), rep(0,n)), 
                        event = c(event1,event2) )

      l = coxph(Surv(T, event) ~ x, dat)
      
      pv <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
      
      # collect
      o <- pv
      
      n <- (l$n-l$nevent)/l$n
      
      p <- exp(l$coefficients[[1]])
      
      ev <- l$nevent
      
   #   pow <- o <=.05
      
      output <- list(o,n,p,ev )
      
      return(output)

    }
    # p=1 ensure exponential
    # n is per group so x2 for total N
    # At os administrational censoring, so end of study
    # ms is median survival
    # lambdaC is the censoring hazard, play around with this
    phr = replicate(1000, w.sim2(p=1, hr=.8, n=300, At=300, ms=5.5, lambdaC=.06)) # n
    phr <- t(phr)
    
    mean(phr[,1] <= 0.05)       # power
    mean(unlist(phr[,2]))
         
      
         # ben bolker approacb
    simfun <- function(np=1, hr=.8, n=30, At=300, ms=5.5, lambdaC=.06,  seed=NULL
          
                       ) {
      
     if (!is.null(seed)) set.seed(seed)
      
      h  <- log(2)/ms                    # hazard for exponential
      ms <- 1/h*(-log(1-0.5))^(p)        # median survival
      T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
      
      ms2<- ms*hr                        # use the HR to get hazard in other group
      h2 <- log(2)/ms2
      T2 <- 1/h2*(-log(runif(n)))^(p)    # weibull times, p=1 so exp, lambda=h2
      
      # lambdaC hazard of censoring
      C1 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      C2 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      
      time1 = pmin(T1,C1)  # observed time is min of censored and true grp0
      time2 = pmin(T2,C2)  # observed time is min of censored and true grp1
      
      event1 = time1==T1   
      event2 = time2==T2   
      
      event = c(event1,event2)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dd <- data.frame(T = c(time1,time2), 
                        x = c(rep(1,n), rep(0,n)), 
                        event = c(event1,event2) )
      return(dd)
    }
    
    fitfun <- function(dd) {
      l = coxph(Surv(T, event) ~ x, dd)
    }
    
    sumfun <- function(fit) {

      l <- fit
      
      pv <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
      
      # collect
      o <- pv
      
      # replace prop of events with upper 95% CI
      n <-  (summary(l)$conf.int[4]) ## (l$n-l$nevent)/l$n
      
      p <- exp(l$coefficients[[1]])
      
      ev <- l$nevent
      
      c(o,n,p,ev)
      
    }
    
    #     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # https://rpubs.com/bbolker/lme4sims
    
    simres <- plyr::raply(100,sumfun(fit(simfun()))) 
    simres <- as.data.frame(simres)
    names(simres) <- c("P-value","Prop events","HR", "No of events")
    apply(simres,2, mean)
    
    
    simres <- plyr::raply(100,sumfun(fit(simfun(hr=2))))
    simres <- as.data.frame(simres)
    names(simres) <- c("P-value","Prop events","HR", "No of events")
    apply(simres,2, mean)
    
    # true_vals <- data.frame(X1=c("(Intercept)", paste0("ttt",letters[2:3]),"x" ),
    #                         value=c(0.05,NA,0.8,NA))
    # 
    # 
    # require(reshape)
    # ggplot(melt(simres),aes(x=X1,y=value))+geom_violin(fill="gray")+
    #   facet_wrap(~X2,scale="free") +
    #   geom_hline(data=true_vals,aes(yintercept=value),col=2,lty=2)
    
    # 1/8.24 and 1/21.68 comes from paper Simulating survival data with predefined
    # censoring rates for proportional hazards models
    
    
    # vary n and the censoring rate
    n <- seq(50,300,by=50)
    lambdaC <- c(sort(c(1/8.24, 1/21.68,(seq(.02,.06,by=0.02)))) )  #50% and 30%
    res0 <- sumfun(fitfun(simfun()))   # returns pvale;prop events; hr; number of events
    nrep <- 499
 
    # make an array to hold results
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    names(res0) <- c("pvalue","Upper95%CI","HR","Nevents")  ## event.prop
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    resarr <- array(
      dim=c(
        length(n),   nrep,
        length(lambdaC),
        length(res0) ),
      
      dimnames=list(  N.per.grp=n,
                      rep=seq(nrep),
                      cens.rate=lambdaC,
                      var=names(res0))
                     
    )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
   options(scipen = 999)
   require(plyr)
   
      for (i in seq_along(n)) {         #  sample size
        
        for (j in seq_along(lambdaC)) {   #  rate
        
        resarr[i,,j,] <- raply(nrep,
                               
                               sumfun(fitfun(simfun(hr=.8,n=  n[i],  # change hr here
                                                    lambdaC=lambdaC[j])))
                               )
       # save("resarr",file="lme4_sims.RData")
         
      }
    }
    
  melt(resarr)
    options(scipen=0, digits=7)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # resarr
    # resarr[,1,,] # show rep 1 only, dimension 2
    # resarr[,2,,] # show rep 2 only
    # resarr[1,,,] # show irst sample size level only  , dimension 1
    # resarr[,,1,] # show first censoring level only   , dimension 3
    # resarr[,,,1:4]
    
    # collapse across cens rate and not rep, not what I want  1,2,4
    # collapse each across reps as we dont mention dimension 2
    resmeans <- apply(resarr,c(1,3,4),mean,na.rm=TRUE)
    
    #Power~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P <- resarr[,,,"pvalue"]           # pull out pvalues
    pow <- P <= 0.05                     # convert to dichotomous  
    apply(pow,c(1,3),mean,na.rm=TRUE)  # calc power
    # number of events ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P <- resarr[,,,"Nevents"]  
    apply(P,c(1,3),mean,na.rm=TRUE)  #  
    # HR~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P <- resarr[,,,"HR"]  
    apply(P,c(1,3),mean,na.rm=TRUE)  #  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    P <- resarr[,,,"Upper95%CI"]  
    noninf <- P < 1.2 # non inferiority 
    apply(noninf,c(1,3),mean,na.rm=TRUE)  #  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    P <- resarr[,,,"pvalue"]           # pull out pvalues
    pow <- P <= 0.05   
    # plot power by censoring rate
    x <- melt(apply(pow,c(1,3),mean,na.rm=TRUE) )
    ggplot(x,aes(x=N.per.grp,y=value, colour=factor(cens.rate))) +
      geom_line(data =x, aes(x=N.per.grp,y=value, group=factor(cens.rate) )) +
      theme_bw() # +
    ##  theme(legend.position="none") 
      
    # plot HR with sample size
    x <- melt(apply(resarr[,,,"HR"] ,c(1,3),mean,na.rm=TRUE)  )
    ggplot(x,aes(x=N.per.grp,y=value, colour=factor(cens.rate)))+
      geom_line(data =x, aes(x=N.per.grp,y=value, group=factor(cens.rate)) ) +
      theme_bw()# +
    #  theme(legend.position="none") 
    
    
    x <- melt(apply(resarr[,,,"Nevents"] ,c(1,3),mean,na.rm=TRUE)  )
    ggplot(x,aes(x=N.per.grp,y=value, colour=factor(cens.rate)))+
      geom_line(data =x, aes(x=N.per.grp,y=value, group=factor(cens.rate)) ) +
      theme_bw() #+
    #  theme(legend.position="none") 
    
      
      
      
      
      
      
      geom_violin(fill="gray") +
      
        facet_wrap(~var,scale="free")  
    
    
    
    m <- melt(P)
    
   
    
    # cens.rate
    # N.per.grp  0.02  0.04  0.06
    # 50  0.255 0.215 0.250
    # 60  0.295 0.280 0.265
    # 70  0.260 0.260 0.285
    # 80  0.380 0.340 0.375
    # 90  0.350 0.345 0.355
    # 100 0.425 0.400 0.410
    # 
    
    
    
    
    
    
    # resci <- apply(resarr,c(1,2,5),quantile,c(0.025,0.975))
    # 
    # 
    # 
    # require(reshape)
    # 
    # ggplot(melt(simres),aes(x=var,y=value))+geom_violin(fill="gray") +
    #   facet_wrap(~var,scale="free")  +
    #   
    #   geom_hline(data=true_vals,aes(yintercept=value),col=2,lty=2)
    # 
    # 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # lets try bendix approach
    
    
    
    
    
    
     
    