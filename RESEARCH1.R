#https://eurekastatistics.com/generating-random-survival-times-from-any-hazard-function/
require(survival)
require(rms)
require(ggplot2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inverse = function(fn, min_x, max_x){
  # Returns the inverse of a function for a given range.
  # E.g. inverse(sin, 0, pi/2)(sin(pi/4)) equals pi/4 because 0 <= pi/4 <= pi/2
  fn_inv = function(y){
    uniroot((function(x){fn(x) - y}), lower=min_x, upper=max_x)[1]$root
  }
  return(Vectorize(fn_inv))
}

integrate_from_0 = function(fn, t){
  int_fn = function(t) integrate(fn, 0, t)
  result = sapply(t, int_fn)
  value  = unlist(result["value",])
  msg    = unlist(result["message",])
  value[which(msg != "OK")] = NA
  return(value)
}

random_survival_times = function(hazard_fn, n, max_time=10000){
  # Given a hazard function, returns n random time-to-event observations.
  cumulative_density_fn = function(t) 1 - exp(-integrate_from_0(hazard_fn, t))
  inverse_cumulative_density_fn = inverse(cumulative_density_fn, 0, max_time)
  return(inverse_cumulative_density_fn(runif(n)))
} 

log(4/7)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot some curves
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1st curve
median.time<-4
lambda<-log(2)/median.time
n =100
hazard_fn = function(t) rep(lambda, length(t))
survival_times = random_survival_times(hazard_fn, n) 
survfit(Surv(survival_times) ~ 1)

require("survival")
plot(survfit(Surv(survival_times, rep(1, length(survival_times)))~1), xlab="Time", ylab="Survival Probability",
     main="Sampled and Expected Survival Curves for h(t) = lambda")
neg_exp_fn = function(x){exp(-lambda * x)}
curve(expr=neg_exp_fn, from=0, to=max(survival_times), add=TRUE, col="red", lwd=2)

##2nd curve
median.time1<-7
lambda1<-log(2)/median.time1
hazard_fn = function(t) rep(lambda1, length(t))
survival_times2 = random_survival_times(hazard_fn, n) 
survfit(Surv(survival_times2) ~ 1)

require("survival")
plot(survfit(Surv(survival_times2, rep(1, length(survival_times2)))~1), xlab="Time", ylab="Survival Probability",
     main="Sampled and Expected Survival Curves for h(t) = lambda1")
neg_exp_fn = function(x){exp(-lambda1 * x)}
curve(expr=neg_exp_fn, from=0, to=max(survival_times2), add=TRUE, col="red", lwd=2)

##dataframe
d <- as.data.frame(cbind(grp = gl(2, n, labels = c("Control", "Treat"))  , 
                         time=c(survival_times,survival_times2 )))


#analyse, cox score test pvalue
logR <- survdiff(Surv(time ) ~ grp,data=d)
(coxphobject<-coxph(Surv(time)~grp,d))
summcph <- summary(coxphobject)
summcph$sctest


#expected HR
median.time/ median.time1
log(median.time/ median.time1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot together
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


require(rms)
f2 <- npsurv(Surv(time ) ~ grp, d)

x <- c("#e41a1c","#377eb8","#4daf4a","#984ea3")

survplot(f2,  n.risk=TRUE, levels.only=T, conf.int=T,
         aehaz=TRUE,
         conf=c("bands"), 
         col.fill=gray(seq(.95, .75, length=4)),
         #col.fill= c(rgb(0,0,0,0.1)), 
         type=c("kaplan-meier"),
         lty=1,  col=x, xlab="Months", abbrev.label=T, 
         label.curves = list(keys = "lines"), #bty='n',
         y.n.risk= 0, cex.n.risk=.6, time.inc=2)


#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### make a function to test this statement...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With at least 100 patients per group, 
# there was approximately 94% power to detect a 3-day difference 
# in median time to XXXX cessation between MMMMM plus TT and TT only. 
# This was based on the assumption that the median time to cessation of XXXXX 
# for patients receiving MMMMM plus TT would be similar to the 4-day median time 
# to cessation of XXXX observed in all studies of CCC
# All power and sample size calculations were based on the assumption of an 
# exponential survival distribution for the 2 groups and the application of a 2-sided
# test at the a = 0.05 level of significance. 


expsim <- function(n=100, lambda1 =log(2)/4, lambda2=log(2)/7) {
  
  # 1st curve
  # lambda1 
  hazard_fn = function(t) rep(lambda1, length(t))
  survival_times = random_survival_times(hazard_fn, n) 
  
  # 2nd curve
  # lambda2 
  hazard_fn = function(t) rep(lambda2, length(t))
  survival_times2 = random_survival_times(hazard_fn, n) 
  
  # dataframe
  d <- as.data.frame(cbind(grp = gl(2, n, labels = c("Control", "Treat"))  , 
                           time=c(survival_times,survival_times2 )))
  
  # analyse, score test for Cox
  # logR <- survdiff(Surv(time ) ~ grp,data=d)
  coxphobject <- coxph(Surv(time)~grp,d)
  summcph <- summary(coxphobject)
  return((summcph$sctest[3][[1]]))
  
}


x <-  replicate(299 , expsim(n=25)) # n is in each group
mean(x<0.05)

## good approx to this?
# m expected total number of events over both groups.
powerSurvEpi::powerCT.default0(k = 1, m = 50, RR = 4/7, alpha = 0.05)

# this seems to agree, put power from the above
coef<- log(4/7)
d = (qnorm(.975) + qnorm(.49))^2 /(.5*.5*coef^2)
d


######################################################################################
######################################################################################
######################################################################################

expsim2 <- function(n=25, lambda1 =log(2)/4, lambda2=log(2)/7, lambda3=log(2)/1) {
  
  # 1st curve
  #lambda1 
  hazard_fn = function(t) rep(lambda1, length(t))
  survival_times = random_survival_times(hazard_fn, n) 
  
  ##2nd curve
  #lambda2 
  hazard_fn = function(t) rep(lambda2, length(t))
  survival_times2 = random_survival_times(hazard_fn, n) 
  
  ##dataframe
  d <- as.data.frame(cbind(grp = gl(2, n, labels = c("Control", "Treat"))  , 
                           time=c(survival_times,survival_times2 )))
  
  #analyse
  #logR <- survdiff(Surv(time ) ~ grp,data=d)
  coxphobject <- coxph(Surv(time)~grp,d)
  summcph <- summary(coxphobject)
  pv1 <- summcph$sctest[3][[1]]
  
  
  ##3nd curve
  hazard_fn = function(t) rep(lambda3, length(t))
  survival_times3 = random_survival_times(hazard_fn, n) 
  
  ##dataframe
  d <- as.data.frame(cbind(grp = gl(2, n, labels = c("Control", "Treat"))  , 
                           time=c(survival_times,survival_times3 )))
  
  #analyse
  #logR <- survdiff(Surv(time ) ~ grp,data=d)
  coxphobject <- coxph(Surv(time)~grp,d)
  summcph2 <- summary(coxphobject)
  pv2 <- summcph2$sctest[3][[1]]
  pv2
  return(min(pv1, pv2))
  
}

x <-  replicate(50  , expsim2())
mean(x<0.05)
##################