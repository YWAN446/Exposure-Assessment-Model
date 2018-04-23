mainversion <- version;
basedir <- "~/stat/sanipath/environ/indicators/";
version<-"v10"
fileda <- paste(basedir,version,"/",version,".data.r",sep="");
source(fileda);
filemc <- paste(basedir,version,"/",version,".mcmc.rda",sep="");
cat("loading environmental parameter estimates ",version,"\n");
load(filemc);
env.r <- exp(indic.parameters$ec.logr);
env.lambda <- exp(indic.parameters$ec.loglambda);
# en.r <- exp(indic.parameters$en.logr);
# en.lambda <- exp(indic.parameters$en.loglambda);
# cm.r <- exp(indic.parameters$cm.logr);
# cm.lambda <- exp(indic.parameters$cm.loglambda);
indic.parameters <- NULL;

version <- mainversion;

# comb.vec = c(neighbourhood,env.smp.tp)
# neighbourhood may be 1:5 (5 = NA)
# env.smp.tp = environment type/sample type/sample attribute combination (1:84)

env.conc <- function(comb.vec){
  size <- 1
  n.comb<-nrow(env.r);
  env.iter<-ncol(env.r);
  k.iter <- sample((1:env.iter),1);
  conc <-  rgamma(n=size, shape=env.r[comb.vec[2],k.iter], 
			  scale=env.lambda[comb.vec[1],comb.vec[2],k.iter]);
  return(conc);
} 

# compartments <-c("dirt","floor","off grnd","SWATA","drain");
# behaviours <- c("play/sit","sleep","handw","bathe","defec","eat");

expos.by.state <- function(k.neighb,k.comp,k.behav,dur,n.hand){
  if(k.comp==1 & k.behav==1){                     # household particulate
      res <- flo.mou.contact(n.hand,dur,c(k.neighb,12),dirt=TRUE);
    return(res);
  }
  if(k.comp==2 & k.behav==1){                     # household swabs
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,27),dirt=FALSE);
    return(res);
  }
  if(k.comp==3 & k.behav==1){
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,28),dirt=FALSE);
    return(res);       
    # household swabs (dry other) For now, we use the same environmental samples as concrete floor, we will separate them in the future!                                                            
  }
  if((k.comp==4 | k.comp==5) & k.behav==1){
    res <- hand.drain(env.conc(c(k.neighb,53)));         # public drain near hh sm.env
    return(c(res,0));
  }
  if(k.behav==2){
    res <- sleep.mouthing(n.hand,dur)
    return(c(res));                          # no exposure
  }
  if(k.behav==3){
    res <- hand.wash(n.hand);
    return(c(res,0));                             # no exposure
  }
  if(k.behav==4){
    res <- hand.bathe(n.hand);
    return(c(res,0));                             # no exposure
  }
  if(k.behav==5){                                     # latrine septage
    res<-ifelse(rbinom(n=1,size=1,prob=0.05)==1,hand.defec(n.hand,env.conc(c(k.neighb,31))),n.hand)         # 5% probability???
    return(c(res,0));
  }
  if(k.behav==6){   # farm food, market food, household food, street vended food
    intk.prod <- env.conc(choose.comb(prod.food(k.neighb))) *
                 serving(prod=TRUE) #* fd.prodsmpl(k.neighb);
    intk.vend <- env.conc(choose.comb(vend.food(k.neighb))) *
                 serving(prod=FALSE) #* fd.vendsmpl(k.neighb);
    return(c(n.hand,intk.prod + intk.vend));
  }
}

choose.comb <- function(comb.vec){
  nitem <- nrow(comb.vec);
  pvec <- rep(1,nitem)/nitem; # equal probabilities for each item in comb.vec
  sel <- which(rmultinom(n=1,size=1,prob=pvec)==1);
  return(comb.vec[sel,]);
}

vend.food <- function(k.neighb){
  return(rbind(c(k.neighb,1),           # fish???
               c(k.neighb,63)));  # salad
               #c(5,63);        # salad
}

prod.food <- function(k.neighb){
  return(rbind(c(k.neighb,3),    # lettuce
               c(k.neighb,4),    # tomatoes
               c(k.neighb,7),    # mango
               c(k.neighb,8),    # chili peppers
               # c(k.neighb,10),   # cabbage ???
               c(k.neighb,83),   # lettuce
               c(k.neighb,84))); # spring onion
               # c(5,3),           # lettuce
               # c(5,4),           # tomatoes
               # c(5,7),           # mango
               # c(5,10),          # cabbage
               # c(5,8),           # chili peppers
               # c(5,83),          # lettuce
               # c(5,84)));        # spring onion
}

drinking.water <- function(k.neighb){
  return(rbind(c(k.neighb,13),   # public tap
               c(k.neighb,17))); # stored water
               # c(5,13),          # public tap
               # c(5,17),
               # c(5,16)));        # stachet
}

expos.dw <- function(k.neighb,k.age,tot.dur){
  multiplier <- tot.dur/(24*60);
  multiplier <- ifelse(multiplier < 0.6,multiplier,round(multiplier));
  tapwater <- is.tap(k.neighb); # household large water samples (4 (large) or 5(small)?)
  if(tapwater){
    freq <- multiplier*dw.tapsmpl(k.age); # daily consumption
    dose <- freq*cup*env.conc(choose.comb(drinking.water(k.neighb)));
    return(c(dose,0));
  }
  if(!tapwater){
    freq <- multiplier*dw.sacsmpl(k.age); # daily consumption
    dose <- freq*sachet*env.conc(c(5,16)); #*area.hand(); why Peter multiply area? # sachet
    return(c(0,dose));
  }
}
