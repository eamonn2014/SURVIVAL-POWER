
# https://stattools.crab.org/R/Survival_Converter.html

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
 SF(find="Survival probability",  hazard_rate=.2, time=5) 
 
 # bullet 1 and 2 find hazard given survival probability and time
 SF(find="Hazard_Rate",           p_survival=.3,  time=2) 
 
 
 