# Reproduce Nquery example with Hmisc::cpower

 
# Reproduce Nquery example “Using an unstratified log-rank test at the one-sided 2.5% significance level, 
# a total of 282 events would allow 92.6% power to demonstrate a 33% risk reduction (hazard ratio for RAD/placebo 
# of about 0.67, as calculated from an anticipated 50% increase in median PFS, from 6 months in placebo arm to 9
# months in the RAD001 arm). With a uniform accrual of approximately 23 patients per month over 74 weeks and 
# a minimum follow up of 39 weeks, a total of 352 patients would be required to obtain 282 PFS events, 
# assuming an exponential progression-free survival distribution with a median of 6 months in the Placebo 
# arm and of 9 months in RAD001 arm. With an estimated 10% lost to follow up patients, a total sample size of 
# 392 patients should be randomized.”

 

# This function is useful for creating d1 an input to Hmisc::cpower to find the intervention mortality at same time
   rm(list=ls())
  
   morti <- function ( d0=.5, hr=2/3, time=6){
    
    lambda = -log(1-d0)/time     # hazard rate 
    m = 1 - exp(-lambda*time*hr) # mortality hence subtracting from 1
    return(m)
  } 
  
  
  d0 <- 0.5      # control mortality at 6 months
  d1 <- morti()  # interv. mortality at 6 months
  # r is the % reduction ctrl to intervention
  
  Hmisc::cpower(tref=6, n=352, mc=d0, r= 100*(d0 - d1)/d0, tmin=39/4,  #mc is  tref-year mortality, control
         accrual=74/4, alpha=.05, pr=TRUE,
         noncomp.c = 0, noncomp.i = 0)



  rm(list=ls())
  library(survival)
  set.seed(8889)
  simfunfx <- function(p=1, hr=2/3, n=352/2, acc=74/4, fup=39/4, lambdaC= -log(.5)/6 , alpha=0.05 ) { # , seed=NULL ) {
    
    # p=1
    # hr=2/3
    # n=352/2
    # acc=74/4
    # fup=39/4
    # lambdaC= -log(.5)/6
    # alpha=0.05
    
    #
    #if (!is.null(seed)) set.seed(seed)
    
    At <- T1 <- T2 <- D1 <- D2 <- T1a <- T2a <- time1 <- time2 <- l <- NULL
    
    At= acc + fup
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    h  <- lambdaC                      # hazard for exponential using med surv, remeber -log(0.5) is the same 
    ms <- 1/h*(-log(0.5))^(p)          # lets show we know how to recreate median survival
    T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
    #T1 <- ((-log(runif(n)))/h)^(1/p) 
    #~~~~~~~~~~~check
    mean(T1) # 1/h
    1/h
    median(T1)  #expect 6
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ms2<- ms/hr                        # use the HR to get hazard in other group
    h2 <- -log(0.5)/ms2
    T2 <- 1/h2*(-log(runif(n)))^(p)    # create n weibull times, p=1 so exp, lambda=h2
    #~~~~~~check
    mean(T2)
    1/h2
    median(T2)  # expect 9
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
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
    
    event1 = time1==T1a   # ctrl
    event2 = time2==T2a   # trt
    
    
    event = c(event1,event2)  # ctrl, trt
    Ti =     c(time1,time2)
    
    # equal to means we have censored, dont really need > , = will do
    event <- ifelse(Ti >= At, 0, event) # if time >= total study time, censor event
    Ti <-     ifelse(Ti >= At, At, Ti)  # if time > total study time, set time to total study time
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dd <- data.frame(T= Ti, 
                     x = c(rep(0,n), rep(1,n)),  # change so ctrl=0 we are comparing trt/ctrl
                     event = event )
    
    l = coxph(Surv(T, event) ~ x, dd)
    
    # log rank p value
    #ll = survdiff(formula = Surv(T, event) ~ x,dd)
    #broom::glance(ll)$p.value
    
    # get score pvalue same as log rank pvalue
    summcph <- summary(l)
    o  <- summcph$sctest["pvalue"][[1]]
    
    #o <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
    
    pow <- o<=alpha
    
    u <- exp(confint(l, level = .9))  [2][[1]]  # use for upper 90% conf interval for non inferiority
    
    n <- 100*(l$nevent)/l$n  
    
    p <- exp(l$coefficients[[1]])
    
    ev <- l$nevent  # no of events
    
    L <- summary(l)
    sd. <- L$coefficients[,"se(coef)"]
    
    h1=h
    h2=h2
    
    c(h1,h2,ev,n,p,u,o, sd. , pow)
  }

 
  simres<- NULL
  simres <- plyr::raply(999,simfunfx(p=1, hr=2/3, n=352/2, acc=74/4, fup=39/4, lambdaC= -log(.5)/6 , alpha=0.05 ))
  simres <- as.data.frame(simres)
  names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events", "Proportion of events","Mean HR", "HR Upper 90CI","Mean P-value","SD log HR","Power")
  r <- (apply(simres,2, mean))
  print(r, digits=6)

  