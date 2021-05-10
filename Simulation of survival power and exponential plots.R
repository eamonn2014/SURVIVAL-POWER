    
  # plot exp curves , see my survival presentation code where this has been adapted from..      
  rm(list=ls())
  # https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
  formatz <- function(x){
    
    if (!is.na(x)  ) {
      
      formatC(signif(x,digits=5), digits=5,format="fg", flag="#",big.mark=",")
      
    }
    
  }
  
  formatz0 <- function(x){
    sprintf(x, fmt = '%s')  
  }
  formatz1 <- function(x){
    sprintf(x, fmt = '%#.1f')  
  }
  formatz2 <- function(x){
    sprintf(x, fmt = '%#.2f')  
  }
  formatz00 <- function(x){
    round(x,0) 
  }
  formatz4 <- function(x){
    sprintf(x, fmt = '%#.4f')  
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  lambda  <-runif(1,1,3)                       # this will be a constant hazard rate
  lambda2 <-runif(1,1,3)                       # second curve rate
  
  end <- ceiling(-(log(1-.999)/ lambda))       # for plotting 99th percentile
  s <- seq(0,end, length.out = end+1)          # reasonable x axis
  p=.5                                         # choose survival prop
  p=runif(1)
  ms <- -log(1-p)/lambda                       # survival

 
  
  (time1 <- ms )
  (mort1 <-  exp(-lambda2*  ms  ) ) # e^-lambda*t
  
  # tereneau book p62
  # In the case of exponential distributions, the reciprocal of the ratio of
  # medians (or any other quantile) gives e(b). For example, if we want to
  # detect a 50% increase in median survival time, we would set e(Beta) = 2/3. 
  (log(1-p)/log(mort1))
  lambda/lambda2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Survival function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  par(mfrow=c(2,1))
  zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale,lower.tail=F)
  
 
   
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
         main=paste0("S(t) for exponential rates ",formatz2(lambda)," (red) and ",formatz2(lambda2), " (blue). HR= ",formatz2(lambda/lambda2),".\nMortalities at time ",
                       formatz4(ms),": ",formatz2(p*100),"% and ", 
                    formatz2((1-mort1)*100 ), "% respectively" ),
        ylab='Survival probability', xlab='Time', col="red")
  abline(v=ms )  
  abline(h=1-p , col='red' , lty=2)
  
   
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end, #main=paste0("S(t), rate = ",lambda2,""),
        ylab='Survival probability', xlab='Time', col="blue", add=TRUE)
  abline(h=mort1 , col='blue' , lty=2)  # think of exp(-lambda*t) where t is the define line above
  

  text(x = end*.6, y = .7,                # Text with different color & size
       paste0("HR = log(Survival)/log(Survival) = log(",formatz2(1-p),") / log(",formatz2(mort1),")"),
       col = "#1b98e0",
       cex = 1.2)
  
  text(x = end*.6, y = .5,                # Text with different color & size
       paste0("HR= hazard rate / hazard rate = ",formatz2(lambda)," / ",formatz2(lambda2),""),
       col = "#1b98e0",
       cex = 1.2)
 # ratio of two survivals
 A <- -log(1-p)/lambda2
 B <- -log(1-p)/lambda
  
 A/B
 lambda/lambda2
  
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ## instead of survival mortality lets postulated change in the time for survival  
 
  #a 50% increase in median survival time, we would set e(3 =
 
 fact. <-  0.50 # % increase in time for survival of this means
 fact. <-  runif(1) # % increase in time for survival of this means
 #lambda <-  3 # baseline hazard (reference)
 lambda2  <-  lambda/(1+fact.)
 hr    <-  lambda2/lambda
 
 curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
       main=paste0("S(t) for exponential rates ",formatz2(lambda)," (red) and ",formatz2(lambda2), " (blue). HR= ",formatz2(hr),".\n A survival probability of ",formatz4(p)," for the reference has survival time ",
                   formatz2(-log(p)/lambda),". \nA ",formatz2(fact.*100) ,"% increase in the survival time results in a survival time of ",formatz2(-log(p)/lambda2)," (blue)"),
       ylab='Survival probability', xlab='Time', col="red")
 abline(v= -log(p)/lambda, col='darkgreen')  
 abline(h=p , col='red' , lty=2)
 abline(v= -log(p)/lambda2 , col='blue')   
 
 curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,  
       ylab='Survival probability', xlab='Time', col="blue", add=TRUE)
  
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 par(mfrow=c(1,1))
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Programs that duplicated Stata survival power and Hmisc power calcs and a published paper
# I show how to bild in accrual and follow up too!!
# Here is a link that is useful for Hmisc::cpower
# https://www.math.uzh.ch/consulting/fileadmin/user_upload/Modeling_Survival_Time.pdf
# https://duckduckgo.com/?q=survival+analysis+sample+size&atb=v206-1&iar=videos&iax=videos&ia=videos&iai=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3Dv18f-Jsqi4c
# the above is nquery video which is very good, see slide 11 nquery powerpoint

# This function is useful for creating d1 an input to Hmisc
# Example 1 nquery, we successfully duplicate it with cpower

  morti <- function ( d0=.5, hr=2/3, time=6){
    
      lambda = -log(1-d0)/time     # hazard rate 
      m = 1 - exp(-lambda*time*hr) # mortality hence subtracting from 1
      return(m)
  } 
  
  morti()
   
d0 <- 0.5
d1 <- morti()
cpower(tref=6, n=352, mc=d0, r= 100*(d0 - d1)/d0, tmin=39/4, 
       accrual=74/4, alpha=.05, pr=TRUE,
       noncomp.c = 0, noncomp.i = 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example 2, we duplicate the paperhttps://www.ncbi.nlm.nih.gov/pmc/articles/PMC3405221/
# We needed 444 patients to detect an increase in 1-year survival from 29 to
# 40% with 80% power at a 5% significance level, 
# assuming an accrual time of 52 weeks and a minimum follow-up time of 52 weeks.

d0 <- 1-.29
d1 <- 1-.4
cpower(tref=1, n=444, mc=d0, r= 100*(d0 - d1)/d0, tmin=1, 
       accrual=1, alpha=.05, pr=TRUE,
       noncomp.c = 0, noncomp.i = 0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# example 3 I duplicate the Hmisc help file example
# Simulate a simple 2-arm clinical trial with exponential survival so
# we can compare power simulations of logrank-Cox test with cpower()
# Hazard ratio is constant and patients enter the study uniformly
# with follow-up ranging from 1 to 3 years
# Drop-in probability is constant at .1 and drop-out probability is
# constant at .175.  Two-year survival of control patients in absence
# of drop-in is .8 (mortality=.2).  Note that hazard rate is -log(.8)/2
# Total sample size (both groups combined) is 1000
# % mortality reduction by intervention (if no dropin or dropout) is 25
# This corresponds to a hazard ratio of 0.7283 (computed by cpower)

cpower(tref=2, n=1000, mc=.2, r=25, accrual=2, tmin=1, 
       noncomp.c=10, noncomp.i=17.5)

# repeating it , only rounding is the difference
d0= .2
time=2
hr=0.7283
d1 <- morti(d0=d0, hr= hr, time=time)

cpower(tref=time, n=1000, mc=d0, r=100*(d0 - d1)/d0, accrual=2, tmin=1, 
       noncomp.c=10, noncomp.i=17.5)
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example from Collett, D - Modelling survival data in medical research-CRC Press (2015)
d0= .6
time=5
#hr= log(0.60)/log(0.41)
#d1 <- morti(d0=d0, hr= hr, time=time)
d1=.41

cpower(tref=time, n=400, mc=d0, r=100*(d0 - d1)/d0, accrual=1.5, tmin=2,  # p478 and p482
       noncomp.c=0, noncomp.i=0)
cpower(tref=time, n=400, mc=d0, r=100*(d0 - d1)/d0, accrual=2, tmin=2, # p483
       noncomp.c=0, noncomp.i=0)
cpower(tref=time, n=903, mc=d0, r=100*(d0 - d1)/d0, accrual=20/24, tmin=0,  # p483 no this does not match <-!!
       noncomp.c=0, noncomp.i=0) #?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stata survival book p353/354, we duplicate with cpower
# stpower exponential 0.03, hratio(0.6) power(0.9) aperiod(20) fperiod(15)

  hr   <- 0.6
  h    <- 0.03  
  time <- 1/h*(-log(1-0.5))^(1) 
  d0   <- 0.5
  d1   <- morti(d0=d0, hr= hr, time=time)

cpower(tref=time, n=190*2, mc=d0, r=100*(d0 - d1)/d0, accrual=20, tmin=15, 
       noncomp.c=0, noncomp.i=0) #

# mimicking STATA survival p344 2nd example! Yeah! No censoring here! write a function
# stpower exponential 0.03, hratio(0.6) power(0.9) fperiod(35)
# its actually the second example on p344
# stpower exponential 0.03, hratio(0.6) power(0.9) fperiod(35) loghazard detail

simfunx <- function(p=1, hr=.6, n=30, At=20, lambdaC=.018, alpha=0.05, seed=NULL ) {
  
      if (!is.null(seed)) set.seed(seed)
      
      h  <- lambdaC #                    # hazard for exponential using med surv, remeber -log(0.5) is the same 
      ms <- 1/h*(-log(1-0.5))^(p)        # lets show we know how to recreate median survival
      T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
      
      ms2<- ms*hr                        # use the HR to get hazard in other group
      h2 <- log(2)/ms2
      T2 <- 1/h2*(-log(runif(n)))^(p)    # create n weibull times, p=1 so exp, lambda=h2
      
      # lambdaC hazard of censoring
      C1 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      C2 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
      
      C1 = rep(100000,n)    # censoring time, basically no censoring due to large time I use here
      C2 = rep(100000,n)    # censoring time  basically no censoring due to large time I use here
      
      time1 = pmin(T1,C1)  # observed time is min of censored and true grp0
      time2 = pmin(T2,C2)  # observed time is min of censored and true grp1
      
      event1 = time1==T1   
      event2 = time2==T2   
      
      event = c(event1,event2)
      Ti = c(time1,time2)
      
      event <- ifelse(Ti >= At, 0, event)
      Ti <-     ifelse(Ti >= At, At, Ti)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dd <- data.frame(T= Ti, 
                       x = c(rep(1,n), rep(0,n)), 
                       event = event )
      
      l = coxph(Surv(T, event) ~ x, dd)
      
      o <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
      
      pow <- o<=alpha
      
      u <- exp(confint(l, level = .9))  [2][[1]]  # use for upper 90% conf interval for non inferiority
      
      n <- (l$n-l$nevent)/l$n  
      
      p <- exp(l$coefficients[[1]])
      
      ev <- l$nevent  # no of events
      
      h1=h
      h2=h2
      
      c(h1,h2,ev,n,p,u,o, pow)
}


simres <- plyr::raply(1000,simfunx(p=1, hr=1/.6, n=294/2, At=35, lambdaC=.03))
simres <- as.data.frame(simres)
names(simres) <- c("h1","h2","no of events", "prop.of.events","HR", "HRUpper90CI","P-value","power")
apply(simres,2, mean)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~lets try and simulate follow up and accrual!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# to do this I add runiform numbers created for unform accrual p 353
# stpower exponential 0.03, hratio(0.6) power(0.9) aperiod(20) fperiod(15)

simfunf <- function(p=1, hr=.6, n=30, acc=20, fup=5, lambdaC=.018, alpha=0.05, seed=NULL ) {
  
  if (!is.null(seed)) set.seed(seed)
  
  At <- T1 <- T2 <- D1 <- D2 <- T1a <- T2a <- time1 <- time2 <- l <- NULL
  
  At= acc + fup
  
  h  <- lambdaC #                    # hazard for exponential using med surv, remeber -log(0.5) is the same 
  ms <- 1/h*(-log(1-0.5))^(p)        # lets show we know how to recreate median survival
  T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
  
  ms2<- ms*hr                        # use the HR to get hazard in other group
  h2 <- log(2)/ms2
  T2 <- 1/h2*(-log(runif(n)))^(p)    # create n weibull times, p=1 so exp, lambda=h2
  
  # lambdaC hazard of censoring
  C1 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
  C2 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
  
  a1 <- runif(n,0,acc) # use these for random uniform accrual times
  a2 <- runif(n,0,acc)
  
  D1 = T1+a1   # add rand uniform to weibull events
  D2 = T2+a2
  
  T1a <- ifelse(D1>At, At,  T1 )  # make sure no time + accrual entry exceeds accrual + follow up
  T2a <- ifelse(D2>At, At,  T2 )  # and replace [rand uniform to weibull events] with original time to event time

  # by pass censoring, so no censoring!
  C1 = rep(100000,n)    # censoring time, basically no censoring
  C2 = rep(100000,n)    # censoring time  basically no censoring
  
  time1 = pmin(T1a,C1)  # observed time is min of censored and true grp0
  time2 = pmin(T2a,C2)  # observed time is min of censored and true grp1
  
  event1 = time1==T1a   
  event2 = time2==T2a   
  
  
  event = c(event1,event2)
  Ti = c(time1,time2)
  
  event <- ifelse(Ti >= At, 0, event)
  Ti <-     ifelse(Ti >= At, At, Ti)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dd <- data.frame(T= Ti, 
                   x = c(rep(1,n), rep(0,n)), 
                   event = event )
  
  l = coxph(Surv(T, event) ~ x, dd)
  
  o <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
  
  pow <- o<=alpha
  
  u <- exp(confint(l, level = .9))  [2][[1]]  # use for upper 90% conf interval for non inferiority
  
  n <- (l$n-l$nevent)/l$n  
  
  p <- exp(l$coefficients[[1]])
  
  ev <- l$nevent  # no of events
  
  h1=h
  h2=h2
  
  c(h1,h2,ev,n,p,u,o, pow)
}

# my god it works!!!
simres <- plyr::raply(1000,simfunf(p=1, hr=.6, n=190,  lambdaC=.018, acc=20, fup=15))
simres <- as.data.frame(simres)
names(simres) <- c("h1","h2","no of events", "prop.of.events","HR", "HRUpper90CI","P-value","power")
apply(simres,2, mean)


# repeat with cpower function!

hr=.6
h  <- .03  
time <- 1/h*(-log(1-0.5))^(1) 
d0=.5

d1 <- morti(d0=d0, hr= hr, time=time)

cpower(tref=time, n=190*2, mc=d0, r=100*(d0 - d1)/d0, accrual=20, tmin=15, 
       noncomp.c=0, noncomp.i=0) #?


#END


## Example p169 Moore's book, not complete.
 
lambda0 = 0.0673
lambda1 = 0.0336

hr=lambda0/lambda1
h  <- lambda0 
time <- 1/h*(-log(1-0.5))^(1) 
d0=.5

d1 <- morti(d0=d0, hr= hr, time=time)
 

cpower(tref=time, n=152/2, mc=d0, r=100*(d0 - d1)/d0, accrual=12, tmin=6, 
       noncomp.c=0, noncomp.i=0) #?


lambda0 = 0.0673
lambda1 = 0.0336

hr=lambda0/lambda1
h  <- lambda0 
time <- 1/h*(-log(1-0.5))^(1) 
d0=.5

d1 <- morti(d0=d0, hr= hr, time=time)

cpower(tref=time, n=152/2, mc=d0, r=100*(d0 - d1)/d0, accrual=12, tmin=6, 
       noncomp.c=0, noncomp.i=0) #?













































 

# STATA MANuAL, hmmm not matching!

# d0= 1- .36
# time=35
# hr= .6
# d1 <- 1-.36^hr #morti(d0=d0, hr= hr, time=time)
#  
# cpower(tref=time, n=147*2, mc=d0, r=100*(d0 - d1)/d0, accrual=35, tmin=0, 
#        noncomp.c=0, noncomp.i=0)
# 
# 
# # d0= 1- .36
# # time=35
# # hr= .4
# # d1 <- morti(d0=d0, hr= hr, time=time)
# 
# h1 =0.030
# h2 =0.018
# hr = h2/h1
# 
# time=log(2)/h1
# d0= .5
#  
# d1 <- morti(d0=d0, hr= hr, time=time)
# 
# 
# cpower(tref=time, n=150*2, mc=d0, r=100*(d0 - d1)/d0, accrual=35, tmin=0, 
#        noncomp.c=0, noncomp.i=0)
#  



























 

 