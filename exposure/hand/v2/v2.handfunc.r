library(triangle);
library(truncnorm);

#########################################################################
# Surface area distributions...
#########################################################################

# Area of hand touching surface (cm2)
# source ????
area.hand <- function(void) rtruncnorm(n=1,mean=3,sd=0.25,a=0,b=Inf);

# Surface area of object placed in mouth (cm2)
# source ????
area.object.in.mouth <-function(void) rexp(n=1,rate=0.1)

# Fraction of surface area of hand placed in mouth, per hand
# source ????
fraction.hand.in.mouth <- function(void) rbeta(n=1,shape1=3.7,shape2=25)/2;

#########################################################################
# Transfer distributions...
#########################################################################

# Transfer coefficient- surface/floor (dust) to hand
# source ????
att.floor.hand <- function(void) rtriangle(n=1,a=0.01,b=0.03,c=0.02);
det.floor.hand <- function(void) rtriangle(n=1,a=0.5,b=0.95,c=0.75);

# transfer coefficient- surface to hand guesstimate as threshold (Rodes 2001)
# source ????
att.dirt.hand <- function(void) rtriangle(n=1,a=0.1,b=0.5,c=0.3)
det.dirt.hand <- function(void) rtriangle(n=1,a=0.5,b=0.95,c=0.75)

# bacterial (log???) CFU reduction by handwashing with soap
# source ????
logred.bact.handwash <- function(void) rbeta(n=1,shape1=3.01,shape2=1.91); 

# bacterial fraction reduction by handwashing w/o soap; pr(removal)
# source ????
det.bact.handwash <- function(soap=FALSE){
  if(soap) return(rtruncnorm(n=1,mean=0.945,sd=0.032,a=0,b=1));
  return(rtruncnorm(n=1,mean=0.951,sd=0.044,a=0,b=1));
}

# bacterial fraction removed by bathing; pr(removal)
# source ????
det.bact.bathe <- function(void) rtriangle(n=1,a=0.3,b=0.9);  

# particulate fraction removed by handwashing; pr(removal)
# source ????
det.part.handwash <- function(void) runif(n=1,min=0.8,max=1.0);  #check this!

# particulate fraction removed by bathing; pr(removal)
# source ????
det.part.bathe <- function(void) runif(n=1,min=0.3,max=0.9);  #check this!

# Particulate fraction removed by hand-mouth contact
# source ????
det.part.hand.mouth <- function(void) rbeta(n=1,shape1=2,shape2=8);

# Bacterial fraction removed by hand-mouth contact; pr(removal)
# mean 33.97% Rusin w/ range from other trans eff
# source ????
det.bact.hand.mouth <- function(void) rtriangle(n=1,a=0.01,b=0.40,c=0.33);

# Adherence of soil to hand (assuming soil is unlimited outside) #mg/cm2
# source ????
adherence.soil.hand <- function(void) rlnorm(n=1,meanlog=0.11,sdlog=2.0)

# Adherence of dust particles should= conc*transfer efficiency but
## only a couple floor swabs, so use guesstimate assuming a third the adh of soil
# source ????
adherence.dust.hand <- function(void) rlnorm(n=1,meanlog=0.11,sdlog=2.0)/3
## only a couple floor swabs, so use guesstimate assuming similar to
# soil distr shifted left
#rlnorm(n=num,meanlog=0.02,sdlog=1.0)                

# Adherence of water to skin when dipping hand in water
# guesstimate using EPA source ????
adherence.water.hand <- function(void) runif(n=1,min=2.14,max=4.99) #1/cm3

#########################################################################
# Frequency distributions...
#########################################################################

# Frequency of hand-surface contact
# guesstimate using Freeman- per min source ????
freq.hand.surface <- function(void)
  rweibull(n=1,shape=1.85,scale=145)/60;

# Freq of hand contact w/ mouth indoor/outdoor during active time (#/min)
# source ????
freq.hand.mouth <- function(outdoor=FALSE){
  if(!outdoor) return(rweibull(n=1,shape=0.91,scale=18.79)/60);
  if(outdoor)  return(rweibull(n=num,shape=0.98,scale=13.76)/60);
}

# Freq of object contact w/ mouth indoor/outdoor (#/min)
# source ????
freq.object.mouth <- function(outdoor=FALSE){
  if(!outdoor) return(rweibull(n=1,shape=1.39,scale=15.54)/60);
  if(outdoor)  return(rweibull(n=num,shape=0.93,scale=8.58)/60);
}

