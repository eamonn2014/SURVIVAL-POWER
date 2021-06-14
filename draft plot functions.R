
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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function % change in survival 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@# 
#you are given a percentile and time t, then you are given a % change in survival at time t

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
# function 2 ARR given  prob survival at fixed time
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

survplot2(  CSurvProp=.4, time1=10, ARR=1 ) #

survplot2(  CSurvProp=.4, time1=10, ARR=-15 ) #


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 3 given survival percentile, time and HR 
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
# function 4  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# blue control, red treatment


# % change in time

# blue control
# red treatment

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
  pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
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


survplot4(  CSurvProp=.4, time1=10, surv.perc.change.improvement=50 ) # 
survplot4(  CSurvProp=.4, time1=10, surv.perc.change.improvement=-50 ) #  
 

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
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# absolute change in time
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
  pp <- ifelse(p<p2,p2,p)                          # v line extends to higher curve
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
 

























