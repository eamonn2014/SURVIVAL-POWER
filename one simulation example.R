
  rm(list=ls())
  require(survival)

# Stata code?
# stpower exponential .0990, hratio(1.75) n(200) loghazard detail

  formatz2 <- function(x){
    sprintf(x, fmt = '%#.2f')  
  }
  
  formatz4 <- function(x){
    sprintf(x, fmt = '%#.4f')  
  }
  
  weibSurv <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale,lower.tail=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# instead of survival mortality lets postulated change in the time for survival  
# visualize the question
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # inputs
  n     <- 100                             # used later, no of patients per group
  ms1   <- 4                               # median survival null
  ms2   <- 7                               # median survival alternative
  p     <- 0.50                            # time for survival probability
  fact. <- (ms2-ms1)/ms1                   # % increase in time for survival of this means
  h     <- lambda   <-  log(2)/ms1         # baseline hazard (reference)
  h2    <- lambda2  <-  lambda/(1+fact.)   # alternate hazard
  hr    <- lambda/lambda2                  # hr compares to alternate/longer survival

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualize the question
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  end <- ceiling(-(log(1-.999)/ lambda))       # for plotting the range of x axis, calc. 99th percentile
  s   <- seq(0,end, length.out = end+1)    
  
  curve(weibSurv(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("S(t) for exponential rates ",formatz2(lambda)," (red) and ",formatz2(lambda2), " (blue). HR= ",formatz2(hr),".\n A survival probability of ",formatz4(p)," for the reference has survival time ",
                    formatz2(-log(p)/lambda),". \nA ",formatz2(fact.*100) ,"% increase in the survival time results in a survival time of ",formatz2(-log(p)/lambda2)," (blue)"),  cex.main = .8,
        ylab='Survival probability', xlab='Time', col="red")
  
  abline(v= -log(p)/lambda, col='darkgreen')  
  abline(h=p , col='red' , lty=2)
  abline(v= -log(p)/lambda2 , col='blue')   
  
  curve(weibSurv(x, shape=1, scale=1/lambda2), from=0, to=end,  
        ylab='Survival probability', xlab='Time', col="blue", add=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot many studies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  T1 <- 1/h* (-log(runif(n)))^(1)      # 1 in brackets dictates exponential dist
  T2 <- 1/h2*(-log(runif(n)))^(1)      # 1 in brackets dictates exponential dist
  
  survfit1 <- survfit(Surv(T1) ~ 1)
  plot(survfit1, ylab="Survival probability", xlab="Time",col="green", conf.int=FALSE, 
       main=paste0("Plotting ",n," studies, ",n," subjects per group, exponential rate  1 = ",formatz4(h),", rate 2 = ",formatz4(h2), "\nHR = ",formatz4(h/h2),
                   " Median survivals ",formatz4(log(2)/lambda)," and ",formatz4(log(2)/lambda2)), xlim=c(0,end))
  
  survfit2 <- survfit(Surv(T2) ~ 1)
  lines(survfit2, lwd=2, col='blue', conf.int = FALSE)  
  
  # draw 100 studies!
  for ( i in 1:n) {
    
    T1 <- 1/h* (-log(runif(n)))^(1)   # 1 in brackets dictates exponential dist
    T2 <- 1/h2*(-log(runif(n)))^(1)   # 1 in brackets dictates exponential dist
    
    survfit1 <- survfit(Surv(T1) ~ 1)
    lines(survfit1, lwd=2, col='green', conf.int = FALSE)  
    
    survfit2 <- survfit(Surv(T2) ~ 1)
    lines(survfit2, lwd=2, col='blue', conf.int = FALSE)  
    
  }
  
  # plot true survival curve, constant hazard h
  curve(weibSurv(x, shape=1, scale=1/h), from=0, to=end, n=length(T1), 
        col='black', lwd=2, lty=2,
        ylim=c(0,1), add=TRUE)
  
  # plot true survival curve, constant hazard h2
  curve(weibSurv(x, shape=1, scale=1/h2), from=0, to=end, n=length(T1), 
        col='black', lwd=2, lty=2,
        ylim=c(0,1), add=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# power simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  sim = function(h,h2,n) {
    
    # Simulate some time to event data
    T1 <- 1/h* (-log(runif(n)))^(1)   
    T2 <- 1/h2*(-log(runif(n)))^(1) 
    
    # data
    ti = c(T1, T2)
    gr = c(rep(1, n), rep(0, n))
    
    l = coxph(Surv(ti) ~ gr)    # Fit Cox model
    anova(l)$`Pr(>|Chi|)`[2]   # P-value 
  }
  
  #set.seed(123) # Reproducible pseudorandom numbers
  pv <-  replicate(1000, sim(n = 200/2, h = h, h2=h2)) 
  mean(pv <= 0.05)  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let's use Frank Harrell Hmisc cpower function
# the perspective here is change in mortality at initial time of interest rather 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  morti <- function ( d0=.5, hr=2/3, time=6){
    
    lambda = -log(1-d0)/time     # hazard rate 
    m = 1 - exp(-lambda*time*hr) # mortality hence subtracting from 1
    return(m)
  } 
  
  time <- ms1
  d0   <- p
  hr   <- 1/hr
  
  d1 <- morti(d0=d0, hr= hr, time=time)
  
  # looking at graph above just made me choose accrual time 100 with no further follow up
  Hmisc::cpower(tref=time, n=100*2, mc=d0, r=100*(d0 - d1)/d0, accrual=100, tmin=0, 
         noncomp.c=0, noncomp.i=0) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~