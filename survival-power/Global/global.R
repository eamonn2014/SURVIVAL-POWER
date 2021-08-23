
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