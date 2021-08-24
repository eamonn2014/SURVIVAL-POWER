
 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# A suite of functions to visualize two survival curves assuming exponential distributions
# JUNE 2021
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


rm(list=ls())
# rounding functions
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}

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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
##THIS IS ADAPTED FROM  https://stattools.crab.org/R/Survival_Converter.html

# Converts survival proportion and hazard rate to time t. 
# Converts survival proportion at time t to a hazard rate.
# Converts survival proportion at time t to a hazard rate.

SF <- function(find, p_survival,  hazard_rate, time) {
  
  
  if (find == "Surv time") {
    
    time = -(log(p_survival)) / hazard_rate
    time = round(time, digits = 3)
    
    res<- paste0("Assuming an exponential distribution, a hazard rate of ",hazard_rate," and a a survival proportion of ",p_survival," dictates a survival time of ",time )
    
  } else if (find == "Survival probability") {
    
    survival = exp(-time * hazard_rate)
    survival = round(survival, digits = 3)
    
    res <- paste0("Assuming an exponential distribution, a hazard rate of ",hazard_rate," at time ",time," dictates a survival probability of ",survival)
    
  } else if (find == "Hazard_Rate") { 
    
    hazard = -(log(p_survival)) / time
    hazard = round(hazard, digits = 3)
    
    res <- paste0("Assuming an exponential distribution, a survival proportion of ",p_survival," at time ",time," dictates a hazard rate of ",hazard)
  }
  
  return(res)
} 


# bullet 1 find time given survival probability and hazard
SF(find="Surv time",             p_survival=.5,  hazard_rate=2) 

# 4th, find survival prob given time and hazard
SF(find="Survival probability",  hazard_rate=2, time=0.347) 

# bullet 1 and 2 find hazard given survival probability and time
SF(find="Hazard_Rate",           p_survival=.5,  time=0.347) 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 1 to plot % change in survival 
# you are given a percentile and time t, then you are given a % change in survival at time t
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@# 

# blue control
# red treatment

survplot1 <- function( CSurvProp=.4, time1=1, surv.perc.change.improvement=5 ) {  #
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # function to plot weibull distribution, shape will always be 1 for exponential
    zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale, lower.tail=F)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # control group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    p <-  CSurvProp                                      # percentile survival
    lambda <- -(log(p))/time1                            # constant hazard in control group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # now the treated group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    i <- 1 + surv.perc.change.improvement/100            # convert % to multiplication factor
    p2 <- (p*i)                                          # apply factor, hypothesized percentile survival in treatment
    p2 <- ifelse(p2>=1, 0.99, p2)                        # ensure within 0-1
    p2 <- ifelse(p2<=0, 0.01, p2)                        # ensure within 0-1
    
    lambda2 <- -log(p2)/time1                            # constant hazard in treatment grp
    
    hr <- lambda2/lambda                                 # hazard ratio, treat / ctrl
    
    surv2 <- 1 - exp(-lambda* time1* hr )                # e^-lambda*t, not used!
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plotting 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
    
    curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
          main=paste0("A % change in survival probability at fixed time (control in blue)\n Exponential rates ",
                      formatz4(lambda),         " (blue) and ",
                      formatz4(lambda2),        " (red): Hazard Ratio = ",
                      formatz4(hr), " (red/blue) \nSurvival S(t) at time ",
                      formatz2(time1),             ": ",
                      formatz2(p*100),          "% (blue) and ", 
                      formatz2((p2)*100 ), "% (red) a ",surv.perc.change.improvement, "% change from control (blue)"), 
          cex.main = .8,
          ylab='Survival probability', xlab='', col="blue", 
          sub= "Time" ,  cex.sub=.8)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    abline(h=p , col='blue' , lty=2)                 # line showing survival percentile in ctrl
    pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
    lines(c(time1 ,time1), c(0 , pp) , col='black')  # vertical line
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
    ylab='Survival probability', xlab='Time', col="red", add=TRUE)
    abline(h=p2 , col='red' , lty=2)                 #  line showing survival percentile in treat
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = end*.575, y = .95,  
         paste0("HR = log(Survival)[red] / log(Survival)[blue] = log(",
                formatz4(p2),                   ") / log(",
                formatz4(p),                 ") = ", 
                formatz4( log(p2)/log(p) ), ""),   # treat/ctrl
         col = "black", 
         cex = .8)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text(x = end*.537, y = .875,  
         paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
                formatz4(lambda2),         " / ",
                formatz4(lambda),        " = ",
                formatz4(hr), ""
         ),
         col = "black", 
         cex = .8)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
}
    

survplot1(  CSurvProp=.4, time1=10, surv.perc.change.improvement=-15 ) # %15 worse, red lower

# checking
SF(find="Surv time",             p_survival=.4,  hazard_rate=.0916) 
SF(find="Survival probability",  hazard_rate=.1079, time=10) 
SF(find="Hazard_Rate",           p_survival=.34,  time=10) 
SF(find="Hazard_Rate",           p_survival=.4,  time=10) 

survplot1(  CSurvProp=.5, time1=23, surv.perc.change.improvement= 15 )    # 15% better, red higher
     
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 2 plot. ARR given  prob survival at fixed time
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# blue control, # red treatment  

survplot2<- function( CSurvProp=.4, time1=1, ARR=5 ) {  #
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # function to plot weibull distribution, shape will always be 1 for exponential
  zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale, lower.tail=F)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <-  CSurvProp                                      # percentile survival
  lambda <- -(log(p))/time1                            # constant hazard in control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # now the treated group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p2 <- (p+ARR/100)                                        # apply factor, hypothesized percentile survival in treatment
  p2 <- ifelse(p2>=1, 0.99, p2)                        # ensure within 0-1
  p2 <- ifelse(p2<=0, 0.01, p2)                        # ensure within 0-1
  
  lambda2 <- -log(p2)/time1                            # constant hazard in treatment grp
  
  hr <- lambda2/lambda                                 # hazard ratio, treat / ctrl
  
  surv2 <- 1 - exp(-lambda* time1* hr )                # e^-lambda*t, not used!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plotting 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
  
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("A absolute change in % survival probability at fixed time (control in blue)\n Exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue) \nSurvival S(t) at time ",
                    formatz2(time1),             ": ",
                    formatz2(p*100),          "% (blue) and ", 
                    formatz2((p2)*100 ), "% (red) a ",ARR, "% absolute change in survival from control (blue)"), 
        cex.main = .8,
        ylab='Survival probability', xlab='', col="blue", 
        sub= "Time" ,  cex.sub=.8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  abline(h=p , col='blue' , lty=2)                 # line showing survival percentile in ctrl
  pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
  lines(c(time1 ,time1), c(0 , pp) , col='black')  # vertical line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
        ylab='Survival probability', xlab='Time', col="red", add=TRUE)
  abline(h=p2 , col='red' , lty=2)                 #  line showing survival percentile in treat
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.575, y = .95,  
       paste0("HR = log(Survival)[red] / log(Survival)[blue] = log(",
              formatz4(p2),                   ") / log(",
              formatz4(p),                 ") = ", 
              formatz4( log(p2)/log(p) ), ""),   # treat/ctrl
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.537, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}

survplot2(  CSurvProp=.4, time1=1, ARR=1 ) #

survplot2(  CSurvProp=.4, time1=1, ARR=-15 ) #


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 3 plot. given survival percentile, time and HR 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# blue control, red treatment

survplot3<- function( CSurvProp=.4, time1=1, HR=2 ) {  #
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # function to plot weibull distribution, shape will always be 1 for exponential
  zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale, lower.tail=F)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <-  CSurvProp                                      # percentile survival
  lambda <- -(log(p))/time1                            # constant hazard in control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # now the treated group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lambda2 <- HR*lambda                                 # constant hazard in treatment grp
  p2 <- exp(-lambda2* time1 )                          # same result as line below
  p2 <- exp(-lambda*  time1* HR )                      
  hr <- HR
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plotting 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
  
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("Effect of hazard ratio (HR) of " ,HR," on control in blue, given control survival probability of ",CSurvProp," at time ",time1, "\n Calculated exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz2(hr), " (red/blue) \nSurvival S(t) at time ",
                    formatz2(time1),             ": ",
                    formatz2(p*100),          "% (blue) and ", 
                    formatz2((p2)*100 ), "% (red)"), 
        cex.main = .8,
        ylab='Survival probability', xlab='', col="blue", 
        sub= "Time" ,  cex.sub=.8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  abline(h=p , col='blue' , lty=2)                 # line showing survival percentile in ctrl
  pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
  lines(c(time1 ,time1), c(0 , pp) , col='black')  # vertical line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
        ylab='Survival probability', xlab='Time', col="red", add=TRUE)
  abline(h=p2 , col='red' , lty=2)                 #  line showing survival percentile in treat
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.575, y = .95,  
       paste0("HR = log(Survival)[red] / log(Survival)[blue] = log(",
              formatz4(p2),                   ") / log(",
              formatz4(p),                 ") = ", 
              formatz2( log(p2)/log(p) ), ""),   # treat/ctrl
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.537, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz2(hr), ""
       ),
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}

survplot3( CSurvProp=.4, time1=1, HR=2 )  
survplot3( CSurvProp=.6, time1=9, HR=.8 )  
survplot3( CSurvProp=.5, time1=6, HR=2/3 )  


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 4  plot given surv. prob, time and % change in time
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# blue control, red treatment


survplot4 <- function( CSurvProp=.4, time1=1, surv.perc.change.improvement=50 ) {  #
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # function to plot weibull distribution, shape will always be 1 for exponential
  zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale, lower.tail=F)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <-  CSurvProp                                      # percentile survival
  lambda <- -(log(p))/time1                            # constant hazard in control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # now the treated group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- 1 + surv.perc.change.improvement/100            # convert % to multiplication factor
  time2 <- (time1*i)                                   # apply factor, hypothesized % survival change in treatment
  lambda2 <- -log(p)/time2                             # constant hazard in treatment grp
  
  hr <- lambda2/lambda                                 # hazard ratio, treat / ctrl
  
  surv2 <- 1 - exp(-lambda* time1* hr )                # e^-lambda*t, not used!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plotting 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
  
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("A % change in survival at fixed survival probability (control in blue)\n Exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue) \nTime at survival quantile ",
                    formatz2(p*100),             "%: ",
                    formatz2(time1),          " (blue) and ", 
                    formatz2(time2 ), " (red) a ",surv.perc.change.improvement, "% change with reference to control (blue)"), 
        cex.main = .8,
        ylab='Survival probability', xlab='', col="blue", 
        sub= "Time\n*In the case of exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). \nFor example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  abline(h=p , col='blue' , lty=2)                 # line showing survival percentile in ctrl
 # pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
  lines(c(time1 ,time1), c(0 , p) , col='blue', lty=2)  # vertical line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
        ylab='Survival probability', xlab='Time', col="red", add=TRUE)
  #abline(h=p2 , col='red' , lty=2)                 #  line showing survival percentile in treat
  lines(c(time2 ,time2), c(0 , p) , col='red', lty=2)
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.575, y = .95,  
       paste0("*HR = (Survival)[blue] / (Survival)[red] = ",
              formatz4(time1),                   " / ",
              formatz4(time2),                 " = ", 
              formatz4( (time1)/(time2) ), ""),   # treat/ctrl
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.584, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


survplot4(  CSurvProp=.4, time1=1, surv.perc.change.improvement=50 ) # 
survplot4(  CSurvProp=.4, time1=1, surv.perc.change.improvement=-50 ) #  
 

# Reproduce Nquery example 
# “Using an unstratified log-rank test at the one-sided 2.5% significance level, a 
# total of 282 events would allow 92.6% power to demonstrate a 33% risk reduction
# (hazard ratio for RAD/placebo of about 0.67, as calculated from an anticipated 
#   50% increase in median PFS, from 6 months in placebo arm to 9 months in the RAD001 arm).
# With a uniform accrual of approximately 23 patients per month over 74 weeks and a minimum 
# follow up of 39 weeks, a total of 352 patients would be required to obtain 282 PFS events,
# assuming an exponential progression-free survival distribution with a median of 6 months 
# in the Placebo arm and of 9 months in RAD001 arm. With an estimated 10% lost to follow up patients, 
# a total sample size of 392 patients should be randomized.”


survplot4(  CSurvProp=.5, time1=6, surv.perc.change.improvement=50 ) 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 5  plot given surv. prob, time and absolute change in time
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# blue control
# red treatment

survplot5 <- function( CSurvProp=.4, time1=1, AAR=50 ) {  #
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # function to plot weibull distribution, shape will always be 1 for exponential
  zp <- function(x, shape, scale)
    pweibull(x, shape=shape, scale=scale, lower.tail=F)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <-  CSurvProp                                      # percentile survival
  lambda <- -(log(p))/time1                            # constant hazard in control group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # now the treated group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  i <- 1 + surv.perc.change.improvement/100           # hyp time
  time2 <- (time1+AAR)                                 # apply factor, hypothesized % survival change in treatment
  lambda2 <- -log(p)/time2                             # constant hazard in treatment grp
  
  hr <- lambda2/lambda                                 # hazard ratio, treat / ctrl
  
  surv2 <- 1 - exp(-lambda* time1* hr )                # e^-lambda*t, not used!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plotting 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
  
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("An absolute change in survival at fixed survival probability (control in blue)\n Exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue) \nTime at survival quantile ",
                    formatz2(p*100),             "%: ",
                    formatz2(time1),          " (blue) and ", 
                    formatz2(time2 ), " (red) a ",AAR, " unit absolute change in survival time with reference to control (blue)"), 
        cex.main = .8,
        ylab='Survival probability', xlab='', col="blue", 
        sub= "Time\n*In the case of exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). \nFor example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  abline(h=p , col='blue' , lty=2)                 # line showing survival percentile in ctrl
  #pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
  lines(c(time1 ,time1), c(0 , p) , col='blue', lty=2)  # vertical line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
        ylab='Survival probability', xlab='Time', col="red", add=TRUE)
  #abline(h=p2 , col='red' , lty=2)                 #  line showing survival percentile in treat
  lines(c(time2 ,time2), c(0 , p) , col='red', lty=2)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.575, y = .95,  
       paste0("*HR = (Survival)[blue] / (Survival)[red] = ",
              formatz4(time1),                   " / ",
              formatz4(time2),                 " = ", 
              formatz4( (time1)/(time2) ), ""),   # treat/ctrl
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.584, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


survplot5(  CSurvProp=.5, time1=6, AAR=3 ) # 
 
  
#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@








# set.seed(333)
# simfunf <- function(p=1, hr=2/3, n=352/2, acc=74/4, fup=39/4, lambdaC= -log(.5)/6 , alpha=0.05 ) { # , seed=NULL ) {
#   
#   # p=1
#   # hr=2/3
#   # n=352/2
#   # acc=74/4
#   # fup=39/4
#   # lambdaC= -log(.5)/6
#   # alpha=0.05
#   
#   #
#   #if (!is.null(seed)) set.seed(seed)
#   
#   At <- T1 <- T2 <- D1 <- D2 <- T1a <- T2a <- time1 <- time2 <- l <- NULL
#   
#   At= acc + fup
#   
#   h  <- lambdaC #                    # hazard for exponential using med surv, remeber -log(0.5) is the same 
#   ms <- 1/h*(-log(0.5))^(1)          # lets show we know how to recreate median survival
#   T1 <- 1/h*(-log(runif(n)))^(p)     # weibull times, p=1 so exp, lambda=h
#   mean(T1)
#   
#   ms2<- ms*hr                        # use the HR to get hazard in other group
#   h2 <- -log(0.5)/ms2
#   T2 <- 1/h2*(-log(runif(n)))^(p)    # create n weibull times, p=1 so exp, lambda=h2
#   
#   mean(T2)
#   
#   # lambdaC hazard of censoring
#   C1 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
#   C2 = rweibull(n, shape=1, scale=1/lambdaC)   # censoring time
#   
#   a1 <- runif(n,0,acc) # use these for random uniform accrual times
#   a2 <- runif(n,0,acc)
#   
#   D1 = T1+a1   # add rand uniform to weibull events
#   D2 = T2+a2
#   
#   T1a <- ifelse(D1>At, At,  T1 )  # make sure no time + accrual entry exceeds accrual + follow up
#   T2a <- ifelse(D2>At, At,  T2 )  # and replace [rand uniform to weibull events] with original time to event time
#   
#   
#   # by pass censoring, so no censoring!
#   C1 = rep(100000,n)    # censoring time, basically no censoring
#   C2 = rep(100000,n)    # censoring time  basically no censoring
#   
#   time1 = pmin(T1a,C1)  # observed time is min of censored and true grp0
#   time2 = pmin(T2a,C2)  # observed time is min of censored and true grp1
#   
#   event1 = time1==T1a   
#   event2 = time2==T2a   
#   
#   
#   event = c(event1,event2)
#   Ti = c(time1,time2)
#   
#   event <- ifelse(Ti >= At, 0, event)
#   Ti <-     ifelse(Ti >= At, At, Ti)
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   dd <- data.frame(T= Ti, 
#                    x = c(rep(1,n), rep(0,n)), 
#                    event = event )
#   
#   l = coxph(Surv(T, event) ~ x, dd)
#   
#   o <- anova(l)$`Pr(>|Chi|)`[2] # P-value from likelihood ratio test
#   
#   pow <- o<=alpha
#   
#   u <- exp(confint(l, level = .9))  [2][[1]]  # use for upper 90% conf interval for non inferiority
#   
#   n <- 100*(l$nevent)/l$n  
#   
#   p <- exp(l$coefficients[[1]])
#   
#   ev <- l$nevent  # no of events
#   
#   L <- summary(l)
#   sd. <- L$coefficients[,"se(coef)"]
#   
#   h1=h
#   h2=h2
#   
#   c(h1,h2,ev,n,p,u,o, sd. , pow)
# }
# 
# require(kableExtra)
# simres<- NULL
# set.seed(6987)
# simres <- plyr::raply(9999,simfunf(p=1, hr=3/2, n=352/2, acc=74/4, fup=39/4, lambdaC= -log(.5)/6 , alpha=0.05 ))
# simres <- as.data.frame(simres)
# names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events", "Proportion of events","Mean HR", "HR Upper 90CI","Mean P-value","SD log HR","Power")
# #options(digits=3)
# r <- (apply(simres,2, mean))
# #r<-as.data.frame(r)
# #print(kable(t(r)))
# #print((t(r)))
# #options(digits=7)
# 
# 
# print(r, digits=6)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# This function is useful for creating d1 an input to Hmisc cpower to find the intervention mortality at same time

  morti <- function ( d0=.5, hr=2/3, time=6){
    
    lambda = -log(1-d0)/time     # hazard rate 
    m = 1 - exp(-lambda*time*hr) # mortality hence subtracting from 1
    return(m)
  } 
  
  
  d0 <- 0.5      # control mortality at 6 months
  d1 <- morti()  # interv. mortality at 6 months
  # r is the % reduction ctrl to intervention
  
  cpower(tref=6, n=352, mc=d0, r= 100*(d0 - d1)/d0, tmin=39/4,  #mc is  tref-year mortality, control
         accrual=74/4, alpha=.05, pr=TRUE,
         noncomp.c = 0, noncomp.i = 0)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # equivalent survival experiences
  survplot4(  CSurvProp=.5, time1=6, surv.perc.change.improvement=50 ) 
  survplot5(  CSurvProp=.5, time1=6, AAR=3 ) # 
  survplot3( CSurvProp=.5, time1=6, HR=2/3 )  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # try and strealine the funaction

  set.seed(14072021)
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
    h  <- lambdaC #                    # hazard for exponential using med surv, remeber -log(0.5) is the same 
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
  
  require(kableExtra)
  simres<- NULL
  #set.seed(6987)
  simres <- plyr::raply(19999,simfunfx(p=1, hr=2/3, n=352/2, acc=74/4, fup=39/4, lambdaC= -log(.5)/6 , alpha=0.05 ))
  simres <- as.data.frame(simres)
  names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events", "Proportion of events","Mean HR", "HR Upper 90CI","Mean P-value","SD log HR","Power")
  #options(digits=3)
  r <- (apply(simres,2, mean))
  #r<-as.data.frame(r)
  #print(kable(t(r)))
  #print((t(r)))
  #options(digits=7)
  
  
  print(r, digits=6)
  
# STATISTICS IN MEDICINE
  # Statist. Med. 2005; 24:1713–1723
  # Published online 22 February 2005 in Wiley InterScience (www.interscience.wiley.com). DOI: 10.1002/sim.2059
  # Generating survival times to simulate Cox proportional hazards models
  # Ralf Bender
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  
  # Example 7 Stata survival manual, p353 include accrual and follow up in our simulation
  # stpower exponential 0.03, hratio(0.6) power(0.9) aperiod(20) fperiod(15)
  
  simres <- plyr::raply(1000,simfunfx(p=1, hr=.6, n=190,  lambdaC=.03, acc=20, fup=15, alpha=.05))
  simres <- as.data.frame(simres)
   names(simres) <- c("hazard rate ctrl","hazard rate trt","No of events", "Proportion of events","Mean HR", "HR Upper 90CI","Mean P-value","SD log HR","Power")
  #options(digits=3)
  r <- (apply(simres,2, mean))
  #r<-as.data.frame(r)
  #print(kable(t(r)))
  #print((t(r)))
  #options(digits=7)
  
  
  print(r, digits=6)
  
 # Example 7 Stata survival manual, p353 repeat with cpower function
  
  hr=.6
  h  <- .03  
  time <- 1/h*(-log(1-0.5))^(1) 
  d0=.5
  
  d1 <- morti(d0=d0, hr= hr, time=time)
  
  cpower(tref=time, n=190*2, mc=d0, r=100*(d0 - d1)/d0, accrual=20, tmin=15, 
         noncomp.c=0, noncomp.i=0) #?
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#