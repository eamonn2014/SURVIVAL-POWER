
# Here we have coded up functions that we call in the mod_???.R files

 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# numerical formatting functions
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
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 1 to plot % change in survival 
# you are given a percentile and time t, then you are given a % change in survival at time t
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@# 

# blue control, red treatment

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
        main=paste0("A percentage change in survival probability at a fixed time (control in blue) is postulated \n Exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue) \nSurvival S(t) at time ",
                    formatz2(time1),             ": ",
                    formatz2(p*100),          "% (blue) and ", 
                    formatz2((p2)*100 ), "% (red) a ",surv.perc.change.improvement, "% change from control (blue)"), 
        cex.main = 1.4,  xlab="", ylab="" # 0.8
       # ylab='Survival probability', xlab='', 
       , col="blue", 
      #  sub= "Time" ,  cex.sub=1.4
      )  #.8
  
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
  
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
       cex = 1.4)  #.8
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.537, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}
# survplot1(  CSurvProp=.4, time1=10, surv.perc.change.improvement=-15 ) # %15 worse, red lower
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# function 2 plot. ARR given  prob survival at fixed time
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
        main=paste0("An absolute percentage point change in survival probability at fixed time (control in blue) is postulated\n Exponential rates ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue) \nSurvival S(t) at time ",
                    formatz2(time1),             ": ",
                    formatz2(p*100),          "% (blue) and ", 
                    formatz2((p2)*100 ), "% (red) a ",ARR, "% absolute change in survival from control (blue)"), 
        cex.main = 1.4,  xlab="", ylab="" # 0.8
        # ylab='Survival probability', xlab='', 
        , col="blue", 
        #  sub= "Time" ,  cex.sub=1.4
  )  #.8
  
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
  
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
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.537, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}

#survplot2(  CSurvProp=.4, time1=1, ARR=1 ) #

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
        cex.main = 1.4,  xlab="", ylab="" # 0.8
        # ylab='Survival probability', xlab='', 
        , col="blue", 
        #  sub= "Time" ,  cex.sub=1.4
  )  #.8
  
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
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
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.537, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz2(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}

# survplot3( CSurvProp=.4, time1=1, HR=2 )  
# survplot3( CSurvProp=.6, time1=9, HR=.8 )  
# survplot3( CSurvProp=.5, time1=6, HR=2/3 )  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# end function 3 plot. given survival percentile, time and HR 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



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
        cex.main = 1.4,
       # ylab='Survival probability', xlab='', 
       col="blue", 
       xlab="", ylab="",
        sub= "*Note for exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). For example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
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
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.584, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


#survplot4(  CSurvProp=.4, time1=1, surv.perc.change.improvement=50 ) # 
#survplot4(  CSurvProp=.4, time1=1, surv.perc.change.improvement=-50 ) #  

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


#survplot4(  CSurvProp=.5, time1=6, surv.perc.change.improvement=50 ) 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# end function 4
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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
                    formatz4(hr), " (red/blue). Time at survival quantile ",
                    formatz2(p*100),             "%:\n ",
                    formatz2(time1),          " (blue) and ", 
                    formatz2(time2 ), " (red) a ",AAR, " unit absolute change in survival time with reference to control (blue)"), 
        cex.main = 1.4,
        # ylab='Survival probability', xlab='', 
        col="blue", 
        xlab="", ylab="",
        sub= "*Note for exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). For example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
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
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.584, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


#survplot5(  CSurvProp=.5, time1=6, AAR=3 ) # 

#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

 # postulate hr and see the effect on time..in progress

survplot6 <- function( CSurvProp=.4, time1=1, HR=2 ) {  #
  
  # CSurvProp=.4
  # time1=1
  # HR=2
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
  lambda2 <- lambda*HR
  #  i <- 1 + surv.perc.change.improvement/100           # hyp time
  #time2 <- (time1+AAR)                                  # apply factor, hypothesized % survival change in treatment
  #lambda2 <- -log(p)/time2                             # constant hazard in treatment grp
  
  time2 <- -(log(p))/lambda2 
  hr <- HR                                             # hazard ratio, treat / ctrl
  
 # surv2 <- 1 - exp(-lambda* time1* hr )                # e^-lambda*t, not used!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # plotting 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end <- ceiling(-(log(1-.999)/ lambda))               # for plotting out to 999th percentile
  
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0("A hazard ratio at fixed survival probability (control in blue) is postulated. Exponential rates\n ",
                    formatz4(lambda),         " (blue) and ",
                    formatz4(lambda2),        " (red): Hazard Ratio = ",
                    formatz4(hr), " (red/blue). Time at survival quantile ",
                    formatz2(p*100),             "%:\n ",
                    formatz2(time1),          " (blue) and ", 
                    formatz2(time2 ), " (red) the effect of HR of a", HR, " on survival time with reference to control (blue)"), 
        cex.main = 1.4,
        # ylab='Survival probability', xlab='', 
        col="blue", 
        xlab="", ylab="",
        sub= "*Note for exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). For example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  mtext("Time",                 side=1, line=3, col="black", cex=1.5)
  mtext("Survival probability", side=2, line=3, col="black", cex=1.5)
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
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  text(x = end*.584, y = .875,  
       paste0(expression("HR = hazard rate[red] / hazard rate[blue] = "),  # treat / ctrl
              formatz4(lambda2),         " / ",
              formatz4(lambda),        " = ",
              formatz4(hr), ""
       ),
       col = "black", 
       cex = 1.4)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}


# survplot6 ( CSurvProp=.5, time1=6, HR=2 ) # 
# survplot6 ( CSurvProp=.5, time1=6, HR=2 ) # 

#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@END@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# plot many studies at once
# function to plot weibull distribution

weibSurv <- function(x, shape, scale)
  pweibull(x, shape=shape, scale=scale,lower.tail=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://www.magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
myColours <- c("green","blue")
myColoursAlpha <- add.alpha(myColours, alpha=0.2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


survplot7 <- function( nsim   =  100 ,
                       npergroup=100,
                       tSurvNULL=4, 
                       tSurvALT =7,
                       SurvProp =0.5 ) {  #
 
  ms2 <-tSurvALT
  ms1 <-tSurvNULL
  fact. <- (ms2-ms1)/ms1                   # % increase in time for survival of this means
  h     <- lambda   <-  -log(SurvProp)/ms1         # baseline hazard (reference)
  h2    <- lambda2  <-  lambda/(1+fact.)   # alternate hazard
  hr    <- lambda/lambda2                  # hr compares to alternate/longer survival
  n <- npergroup
  N <- nsim
  
  end <- ceiling(-(log(1-.999)/ lambda))       # for plotting the range of x axis, calc. 99th percentile
 # s   <- seq(0,end, length.out = end+1)    
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot many studies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

T1 <- 1/h* (-log(runif(n)))^(1)      # 1 in brackets dictates exponential dist
T2 <- 1/h2*(-log(runif(n)))^(1)      # 1 in brackets dictates exponential dist

survfit1 <- survfit(Surv(T1) ~ 1)
plot(survfit1, ylab="Survival probability", xlab="Time", col='white', conf.int=FALSE,  # white so invisible
     main=paste0("Plotting ",N," studies, ",n," subjects per group, exponential rate ctrl (green) = ",formatz4(h),", rate intervention (blue) = ",formatz4(h2), "\nHR = ",formatz2(h2/h),
                 ", ",100*SurvProp, " percentile survival ",formatz2(-log(SurvProp)/lambda)," control in green and ",formatz2(-log(SurvProp)/lambda2)," intervention in blue"), 
     xlim=c(0,end), cex.main = 1.4)

survfit2 <- survfit(Surv(T2) ~ 1)
#lines(survfit2, lwd=2, col=myColoursAlpha[2], conf.int = FALSE)  # no line

abline(h=SurvProp , col='purple' , lty=2)  

# draw N extra studies!
for ( i in 1:(N)) {
  
  T1 <- 1/h* (-log(runif(n)))^(1)   # 1 in brackets dictates exponential dist
  T2 <- 1/h2*(-log(runif(n)))^(1)   # 1 in brackets dictates exponential dist
  
  survfit1 <- survfit(Surv(T1) ~ 1)
  lines(survfit1, lwd=.5, col=myColoursAlpha[1], conf.int = FALSE, )  
  
  survfit2 <- survfit(Surv(T2) ~ 1)
  lines(survfit2, lwd=2, col=myColoursAlpha[2], conf.int = FALSE)  
  
}

  x1 <- -log(SurvProp)/lambda
  x2 <- -log(SurvProp)/lambda2
  
  # plot true survival curve, constant hazard h
  curve(weibSurv(x, shape=1, scale=1/h), from=0, to=end, n=length(T1), 
        col='black', lwd=2, lty=2,
        ylim=c(0,1), add=TRUE)
  
  lines(c( x1,x1), c(0 , SurvProp) , col='red', lty=3)
  
 
  
  # plot true survival curve, constant hazard h2
  curve(weibSurv(x, shape=1, scale=1/h2), from=0, to=end, n=length(T1), 
        col='black', lwd=2, lty=2,
        ylim=c(0,1), add=TRUE)
 # abline(v=x2 , col='purple' , lty=2)  
  lines(c(x2 ,x2), c(0 , SurvProp) , col='red', lty=3)

  
  # abline(h=.5 , col='grey' , lty=4)  
  # lines(c(tSurvNULL ,tSurvNULL), c(0 , .5) , col='grey', lty=4)
  # lines(c(tSurvALT  ,tSurvALT),  c(0 , .5) , col='grey', lty=4)
 
  

}

#survplot7()
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@end



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# This function is useful for creating d1 an input to Hmisc cpower to find the intervention mortality at same time

morti <- function ( d0=.5, hr=2/3, time=6){
  
  lambda = -log(1-d0)/time     # hazard rate 
  m = 1 - exp(-lambda*time*hr) # mortality hence subtracting from 1
  return(m)
} 


#d0 <- 0.5      # control mortality at 6 months
#d1 <- morti()  # interv. mortality at 6 months
# r is the % reduction ctrl to intervention

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# my function to approximate Hmisc::cpower

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
  
  u <- exp(confint(l, level = .9))  [2][[1]]  # use for upper 90% conf interval for non inferiority?
  
  n <- 100*(l$nevent)/l$n  
  
  p <- exp(l$coefficients[[1]])
  
  ev <- l$nevent  # no of events
  
  L <- summary(l)
  sd. <- L$coefficients[,"se(coef)"]
  
  h1=h
  h2=h2
  
  c(h1,h2,ev,n,p,u,o, sd. , pow)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 2 functions to plot exponential distributions needing 2 hazards and a survival prop 

# functions  to get means and SDs, not used
# mymean <- function(x) {mean(x, na.rm=T)}
# mysd <- function(x) {sd(x, na.rm=T)}

zp <- function(x, shape, scale)
  pweibull(x, shape=shape, scale=scale,lower.tail=F)

# draw curves using hazards and surv prop of control
# I wrote the code in lower chunks but
    survP <- function(Ihaz= runif(1,1,1.5), Chaz=runif(1,2,2.5), CSurvProp=runif(1) ) {
      
      lambda  <- Ihaz            # numerator
      lambda2 <- Chaz            # denominator
      hr <- lambda/lambda2
      
      # for plotting 
      end <- ceiling(-(log(1-.999)/ lambda))       # for plotting 99th percentile
      s <- seq(0,end, length.out = end+1)          # reasonable x axis
      
      p <- CSurvProp                                # random survival prop
      time <-ms <- -log(1-p)/lambda                # survival time  for survival prop p
      
      # (time1 <- ms )
      mort1 <-  exp(-lambda2* time )  # e^-lambda*t
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Survival function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      zp <- function(x, shape, scale)
        pweibull(x, shape=shape, scale=scale,lower.tail=F)
      #~~~~~~~~~~~~~~~~~~
      curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
            main=paste0("A change in survival probability at fixed time\n Exponential rates ",
                        formatz2(lambda),         " (intervention blue) and ",
                        formatz2(lambda2),        " (control red): HR= ",
                        formatz2(hr), " (blue/red) \nMortalities (1-S(t)) at time ",
                        formatz4(time),             ": ",
                        formatz2(p*100),          "% (blue) and ", 
                        formatz2((1-mort1)*100 ), "% (red)" ), 
            cex.main = .8,
            ylab='Survival probability', xlab='', col="blue", 
            sub= "Time" ,  cex.sub=.8)
      #~~~~~~~~~~~~~~~~~~
      abline(h=1-p , col='blue' , lty=2)
      abline(h=mort1 , col='red' , lty=2)     
      lines(c(time ,time), c(0 , 1-p) , col='black')
      #~~~~~~~~~~~~~~~~~~
      curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,     
            ylab='Survival probability', xlab='Time', col="red", add=TRUE)
      #~~~~~~~~~~~~~~~~~~
      text(x = end*.6, y = .95,  
           paste0("HR = log(Survival)[blue] / log(Survival)[red] = log(",
                  formatz2(1-p),                   ") / log(",
                  formatz2(mort1),                 ") = ", 
                  formatz2( log(1-p)/log(mort1) ), ""),
           col = "black", 
           cex = .8)
      #~~~~~~~~~~~~~~~~~~
      text(x = end*.537, y = .875,  
           paste0(expression("HR = hazard rate[blue] / hazard rate[red] = "),
                  formatz2(lambda),         " / ",
                  formatz2(lambda2),        " = ",
                  formatz2(hr), ""
           ),
           col = "black", 
           cex = .8)
    }


SurvP2 <- function( lambda=0.07701635, lambda2=0.11552453,  p=.5) {
  
  # worst survival blue curve
  hr <- lambda/lambda2
  
  # for plotting 
  end <- ceiling(-(log(1-.999)/ lambda))       # for plotting 99th percentile
  s <- seq(0,end, length.out = end+1)          # reasonable x axis
  
  time <-ms <- -log(1-p)/lambda                # survival time  for survival prop p
  mort1 <-  exp(-lambda2* time )               # e^-lambda*t
  
  p          <- mort1  
  ref.time   <- time 
  trt.time   <- -log(p)/lambda
  fact.      <- (-log(p)/lambda)  / (-log(p)/lambda2) -1
  #~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda), from=0, to=end, 
        main=paste0(  "A change in survival time S(t) at fixed survival probability:\nA survival probability of ",
                      formatz4(p),         " for the red control curve has survival time ",
                      formatz2( ref.time), ". \nA ",
                      formatz2(fact.*100), "% increase in the survival time results in a survival time of ",
                      formatz2(trt.time),  " (intervention red)\nExponential rates ",
                      formatz2(lambda),    " (blue) and ",
                      formatz2(lambda2),   " (red). HR= ",
                      formatz2(hr),        " (blue/red)"
        ),  
        cex.main = .8,
        ylab='Survival probability',xlab='', 
        col="blue",
        sub= "Time\nIn the case of exponential distributions, the reciprocal of the ratio of medians (or any other quantile) gives e(b). \nFor example, if we want to detect a 50% increase in median survival time, we would set e(Beta) = 2/3" , cex.sub=.8)
  #~~~~~~~~~~~~~~~~~~
  text(x = end*.5, y = .95,  
       paste0("HR at survival quantile ",
              formatz4(p),                     ", ratio of survival times: red / blue = ",
              formatz2(ref.time),              " / ",
              formatz2(trt.time),              " = ", 
              formatz2( ref.time/trt.time ),   ""),
       col = "black",
       cex = .8)
  #~~~~~~~~~~~~~~~~~~
  text(x = end*.52, y = .875,  
       paste0(expression("HR = hazard rate[blue] / hazard rate[red] = "),
              formatz2(lambda)," / ",formatz2(lambda2)," = ",
              formatz2(lambda/lambda2),""
       ),
       col = "black",
       cex = .8)
  #~~~~~~~~~~~~~~~~~~~
  abline(h=p , col='black' , lty=2)
  lines(c(-log(p)/lambda2 , -log(p)/lambda2), c(0 , p) , col='red')
  lines(c(-log(p)/lambda  , -log(p)/lambda),  c(0 , p) , col='blue')
  #~~~~~~~~~~~~~~~~~~~
  curve(zp(x, shape=1, scale=1/lambda2), from=0, to=end,  
        ylab='Survival probability', xlab='Time', col="red", add=TRUE)
}

# survP( 0.07701635,0.11552453,0.5)
# SurvP2()
