exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.period.seq(k.neighb,3,2,tot.dur);
  n.states <- ncol(states);
  expos <- array(NA,dim=c(23,n.states));
  comp <- states[1,];
  behav <- states[2,];
  dur <- states[3,];
  n.hand <- c(0,0,0,0,0,0);
  n.food <- c(0,0,0,0,0,0);
  p.exclu.breast.feed <- ifelse(k.age==1,0.3388,ifelse(k.age==2,0.007,0))
  exclu.breast.feed<-rbinom(1,1,p.exclu.breast.feed)
  for(k.state in 1:n.states){
    expos[1:3,k.state] <- states[,k.state];
    res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                          states[3,k.state],n.hand,k.age,exclu.breast.feed);
    if(length(res)==18){
      n.hand <- res[1:6];
      expos[4:9,k.state] <- n.hand;
      expos[10:15,k.state] <- res[13:18];
      expos[16:21,k.state] <- res[7:12];
    }
  }
  expos[22:23,n.states] <- expos.dw(k.neighb,k.age,tot.dur);
  return(expos);
}

gen.period.seq<-function(k.neighb,start.comp,start.behav,tot.dur) {
  act <- gen.behav(k.neighb,start.comp,start.behav);
  time <- act[3];
  prev.comp <- act[1];
  prev.behav <- act[2];
  while(time <= tot.dur) {
    new.state <- gen.behav(k.neighb,prev.comp,prev.behav);
    time <- time + new.state[3];
    prev.comp <- new.state[1];
    prev.behav <- new.state[2];
    act <- cbind(act,new.state);
  }
  return(act);
}

expos.by.state<-function(k.neighb,k.comp,k.behav,dur,n.hand,k.age,exclu.breast.feed){
  res<-c();
  if(k.comp==1 & k.behav==1){                     # household particulate
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,12),k.comp,dirt=TRUE);
    return(c(res,c(0,0,0,0,0,0)));
  }
  if(k.comp==2 & k.behav==1){                     # household swabs
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,27),k.comp,dirt=FALSE);
    return(c(res,c(0,0,0,0,0,0)));
  }
  if(k.comp==3 & k.behav==1){
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,28),k.comp,dirt=FALSE);
    return(c(res,c(0,0,0,0,0,0)));       
    # household swabs (dry other) For now, we use the same environmental samples as concrete floor, we will separate them in the future!                                                            
  }
  if((k.comp==4 | k.comp==5) & k.behav==1){
    res <- hand.drain(env.conc(c(k.neighb,53)),n.hand);         # public drain near hh sm.env
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));
  }
  if(k.behav==2){
    res <- sleep.mouthing(n.hand,dur)
    return(c(res,c(0,0,0,0,0,0)));                          # no exposure
  }
  if(k.behav==3){
    res <- hand.wash(n.hand);
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));                             # no exposure
  }
  if(k.behav==4){
    res <- hand.bathe(n.hand);
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));                             # no exposure
  }
  if(k.comp==1 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,12)),k.comp,dirt=TRUE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));
  }
  if(k.comp==2 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,27)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));
  }
  if(k.comp==3 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,28)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));
  }
  if((k.comp==4 | k.comp==5) & k.behav==5){
    res <- n.hand;
    res <- ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.drain(env.conc(c(k.neighb,53)),res)); 
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0)));
  }  
  if(k.behav==6){# farm food, market food, household food, street vended food
#    p.exclu.breast.feed <- ifelse(k.age==1,0.3388,ifelse(k.age==2,0.007,0))
    p.breast.feed <- ifelse(k.age==1,rbeta(1,79,46),ifelse(k.age==2,rbeta(1,85,114),rbeta(1,10,90)))
    p.eat.pro <- ifelse(k.age==1,rbeta(1,31,16),ifelse(k.age==2,rbeta(1,72,43),rbeta(1,74,17))) #bought food like pepper, tomatos and raw produce
    n.food<-c();
    ing<-c();
    res<-c();
#    exclu.breast.feed<-rbinom(1,1,p.exclu.breast.feed)
    breast.feed<-rbinom(1,1,p.breast.feed)
    eat.pro<-rbinom(1,1,p.eat.pro)
    if (exclu.breast.feed==1){
      num.food <- 0; #assume no intake for breast feeding;
      res<-c(n.hand,c(0,0,0,0,0,0),c(0,0,0,0,0,num.food))
    }
    if (exclu.breast.feed==0 & breast.feed==1){
      num.food <- 0; #assume no intake for breast feeding;
      res<-c(n.hand,c(0,0,0,0,0,0),c(0,0,0,0,0,num.food))
    }
    if (exclu.breast.feed==0 & breast.feed==0 & eat.pro==1){
      num.food <- round(env.conc(choose.comb(prod.food(k.neighb))) *
              serving(prod=TRUE)) #* fd.prodsmpl(k.neighb);
      n.food <- c(0,0,0,0,0,num.food);
      res<-hand.produce.contact(n.hand,n.food);
    }
    if (exclu.breast.feed==0 & breast.feed==0 & eat.pro==0){
      num.food <- round(env.conc(choose.comb(vend.food(k.neighb))) *
        serving(prod=FALSE)) #* fd.vendsmpl(k.neighb); #actually is prepared food including salad and fish
      n.food <- c(0,0,0,0,0,num.food);
      res <- hand.food.contact(n.hand,n.food);
    }
    return(res);
  }
}

hand.eat<-function(void) rbinom(1,1,rbeta(1,254,46)) #??? need to input number of eat by hands and number of eat by fork.

hand.produce.contact<-function(n.hand,n.food){
  p.att <- att.floor.hand();
  p.det <- det.floor.hand(); # assume all area of one hand touch the food and all produce are eaten by hands
  ing <- c(0,0,0,0,0,0);
  state1 <- c();
  state1 <- next.state(c(n.food,n.hand),c(p.att,p.det));
  n.hand <- state1[7:12];
  p.att <- att.floor.hand();
  p.det <- det.floor.hand();
  state2 <-c ();
  state2 <- next.state(c(c(0,0,0,0,0,0),n.hand),c(p.att,p.det));
  n.food <- state2[1:6];
  n.hand <- state2[7:12];
  ing <- n.food;
  return(c(n.hand,ing,n.food));
}

hand.food.contact<-function(n.hand,n.food){
  if (hand.eat()==1){
  p.att <- att.floor.hand();
  p.det <- det.floor.hand(); # assume all area of one hand touch the food, part of food were eaten by hands.
  ing <- c(0,0,0,0,0,0);
  state1 <- c();
  state1 <- next.state(c(n.food,n.hand),c(p.att,p.det));
  n.hand <- state1[7:12];
  n.food <- state1[1:6];
  }
  ing <- n.food;
  return(c(n.hand,ing,n.food));
}

flo.mou.contact<-function(n.hand,dur,comb.vec,comp,dirt=FALSE){
  ing <- c(0,0,0,0,0,0);
  n.hnd <- n.hand;
  time <- 0;
  t.cnt <- exp(-freq.hand.surface());
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
  while(time + min(t.cnt,t.mth) < dur){
    time <- time + min(t.cnt,t.mth);
    if(t.mth < t.cnt){
      t.cnt<-t.cnt-t.mth;
      t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
      trans <- hand.mouth(n.hnd);
      ing <- ing + trans;
      n.hnd<- n.hnd - trans;
      # cat("ingested: ", ing,"\n");
      trans<- c(0,0,0,0,0,0);
    }else{
      t.mth<-t.mth-t.cnt;
      t.cnt <- exp(-freq.hand.surface());
      n.hnd <- hand.floor(n.hnd,env.conc(comb.vec),comp,dirt);
      # cat("hand: ",n.hnd," ");
    }
  }
  return(c(n.hnd,ing));
}

hand.mouth<-function(n.hnd){
  p.att <- det.bact.hand.mouth()*fraction.hand.in.mouth();
  p.det <- 0;
  res <- next.state(c(n.hnd,0,0,0,0,0,0),c(p.att,p.det));
  return(res[7:12]);
}

next.state <- function(num.vec,p.vec){
  n.had <- c(0,0,0,0,0,0)
  n.intk <- c(0,0,0,0,0,0)
  for (i in 1:6){
    att <- ifelse(num.vec[i]>1e8, round(num.vec[i]*p.vec[1]),
                  rbinom(n=1,size=num.vec[i],prob=p.vec[1]));
    det <- ifelse(num.vec[i+6]>1e8, round(num.vec[i+6]*p.vec[2]),
                  rbinom(n=1,size=num.vec[i+6],prob=p.vec[2]));
    n.had[i] <- num.vec[i]-att+det;
    n.intk[i] <- num.vec[i+6]+att-det;
  }
  return(c(n.had,n.intk));
}

hand.floor <- function(n.hnd,conc,comp,dirt){
  h.area <- area.hand();
  n.srf <- num.surface(conc,h.area);
  p.att <- att.floor.hand();
  p.det <- det.floor.hand()*h.area/55; # 55 cm2 1 palm area
  # n.event <- 1 + rpois(n=1,lambda=2);
  # res <- sim.1(n.srf+n.hnd,n.hnd,p.att,p.det,n.event);
  # return(res[n.event,2]);h.area <- area.hand();
  if (comp==1) n.surf <- c(0,n.srf,0,0,0,0);
  if (comp==2) n.surf <- c(0,0,0,n.srf,0,0);
  if (comp==3) n.surf <- c(0,0,n.srf,0,0,0);
  if (comp==4) n.surf <- c(n.srf,0,0,0,0,0);
  if (comp==5) n.surf <- c(n.srf,0,0,0,0,0);
  res <- next.state(c(n.surf,n.hnd),c(p.att,p.det));
  if(dirt){
    n.srf <- num.surface(conc*adherence.soil.hand(),h.area);
    p.att <- 1; #For soil change unit from mg to cm2;
    n.surf <- c(0,n.srf,0,0,0,0);
    res_d <- next.state(c(n.surf,n.hnd),c(p.att,p.det));
    res[2] <- res_d[2];
    res[8] <- res_d[8];
  }
  return(res[7:12]);
}

det.floor.hand <- function(void) rtriangle(n=1,a=0.15,b=0.35,c=0.25)

hand.drain <- function(conc,n.hnd){
  lam <- adherence.water.hand()*conc;
  if(lam > 1e8) n.hnd[1]<-n.hnd[1]+round(lam)
  else n.hnd[1]<-n.hnd[1]+rpois(n=1,lambda=lam);
  return(c(n.hnd))
}

sleep.mouthing <- function(n.hand,dur,dirt=FALSE){
  n.hnd <- n.hand;
  ing <- c(0,0,0,0,0,0);
  time <- 0;
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));# The frequency of mouth could be different between when child is sleeping and when child is playing, for now use the same value; 
  while (time+t.mth < dur){
    time<-time+t.mth;
    trans<-hand.mouth(n.hnd);
    n.hnd<-n.hnd-trans;
    ing<-ing+trans;
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    trans<-c(0,0,0,0,0,0);
  }
  return(c(n.hnd,ing))
}

#hand.wash <- function(n.before){
#  p.det<-c(0,0,0,0,0,0);
#  res<-c();
#  for (i in 1:6){
#    p.det[i] <- ifelse(n.before[i]!=0,det.bact.handwash(n.before[i]),0);
#    res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det[i]))[i];
#    res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det[i]))[i+6];
#  }
#  return(res[7:12]);
#}

#hand.bathe <- function(n.before){
#  p.det<-c(0,0,0,0,0,0);
#  res<-c();
#  for (i in 1:6){
#    p.det[i] <- ifelse(n.before[i]!=0,det.bact.bathe(n.before[i]),0);
#    res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det[i]))[i];
#    res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det[i]))[i+6];
#  }
#  return(res[7:12]);
#}

hand.defec <- function(n.before,conc){
  lam <- num.surface(conc*adherence.water.hand(),area.hand());
  n.feces <- ifelse(lam > 1e8, round(lam),rpois(n=1,lambda=lam));
  p.att <- 0.95;
  p.det <- 0
  res <- next.state(c(0,0,0,0,n.feces,0,n.before),c(p.att,p.det));
  return(res[7:12]);
}

#det.bact.handwash<-function(n.before,soap=FALSE){
#  p=10^(-rnorm(n=1,mean=(log10(n.before)*0.4),sd=log10(n.before)*0.025));
#  soap <- hand.soap();
#  if(soap) p=10^(-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05));
#  if (p>1) p=runif(n=1,min=0.05,max=0.95);
#  return(1-p);
#}

#det.bact.bathe<-function(n.before,soap=FALSE){
#  p=10^(-rnorm(n=1,mean=(log10(n.before)*0.4),sd=log10(n.before)*0.025));
#  soap <- bath.soap();
#  if(soap) p=10^(-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05));
#  if (p>1) p=runif(n=1,min=0.05,max=0.95);
#  return(1-p);
#}

#new handwashing function 031615
det.bact.handwash<-function(void){
  soap <- hand.soap();
  dur <- 10+rgamma(1,shape=2.5,scale=10); #arbitrary number >10s and mean 35s
  if (soap==TRUE) r<- rnorm(1,-2.1795,0.5132)+rnorm(1,1.1922,0.1436)*log(dur);
  if (soap==FALSE) r<- rnorm(1,-0.6836,0.6036)+rnorm(1,0.8001,0.1394)*log(dur);
  if (r<0) r<-0.1;
  return(1-0.1^r);
}

det.bact.bathe<-function(void){
  soap <- hand.soap();
  dur <- 60+rgamma(1,shape=4,scale=60); #arbitrary number >60s and mean 5 min
  if (soap==TRUE) r<- rnorm(1,-2.1795,0.5132)+rnorm(1,1.1922,0.1436)*log(dur);
  if (soap==FALSE) r<- rnorm(1,-0.6836,0.6036)+rnorm(1,0.8001,0.1394)*log(dur);
  if (r<0) r<-0.1;
  return(1-0.1^r);
}

hand.wash <- function(n.before){
  res<-c();
  p.det <- ifelse(sum(n.before)>1000,det.bact.handwash(),
                     ifelse(sum(n.before)!=0,rtruncnorm(1,0,0.99,0.5,0.2),0));#made up numbers
  res<-next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))
  #for (i in 1:6){
  #  res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i];
  #  res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i+6];
  #}
  return(res[7:12]);
}

hand.bathe <- function(n.before){
  res<-c();
  p.det <- ifelse(sum(n.before)>1000,det.bact.bathe(),
                     ifelse(sum(n.before)!=0,rtruncnorm(1,0,0.99,0.5,0.2),0));
  res<-next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))
  #for (i in 1:6){
  #  res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i];
  #  res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i+6];
  #}
  return(res[7:12]);
}

serving <- function(prod=TRUE){
  if(prod)  return(rgamma(n=1,shape=2.5,scale=40)*1000);   #mg/serving;
  if(!prod) return(rnorm(1,100,15)*1000);   #mg/serving;
}

expos.dw <- function(k.neighb,k.age,tot.dur){
  multiplier <- tot.dur/(24*60);
  multiplier <- ifelse(multiplier < 0.6,multiplier,round(multiplier));
  tapwater <- is.tap(k.neighb); # household large water samples (4 (large) or 5(small)?)
  if(tapwater){
    freq <- multiplier*dw.tapsmpl(k.age); # daily consumption
    prop <- freq*cup*env.conc(choose.comb(drinking.water(k.neighb)));
    dose <- rpois(1,prop);
    return(c(dose,0));
  }
  if(!tapwater){
    freq <- multiplier*dw.sacsmpl(k.age); # daily consumption
    prop <- freq*sachet*env.conc(c(k.neighb,16)); #*area.hand(); why Peter multiply area? # sachet
    dose <- rpois(1,prop);
    return(c(0,dose));
  }
}

#try new sachet and piped water function to modify the 
#######################################################################
env.conc1<-function(comb.vec){
  conc <-  rgamma(n=1, shape=exp(mean(log(env.r[comb.vec[2],]))), 
                  scale=exp(mean(log(env.lambda[comb.vec[1],comb.vec[2],]))));
  return(conc);
}

expos.dw <- function(k.neighb,k.age,tot.dur){
  multiplier <- tot.dur/(24*60);
  multiplier <- ifelse(multiplier < 0.6,multiplier,round(multiplier));
  tapwater <- is.tap(k.neighb); # household large water samples (4 (large) or 5(small)?)
  if(tapwater){
    freq <- multiplier*dw.tapsmpl(k.age); # daily consumption
    prop <- freq*cup*env.conc1(choose.comb(drinking.water(k.neighb)));
    dose <- rpois(1,prop);
    return(c(dose,0));
  }
  if(!tapwater){
    freq <- multiplier*dw.sacsmpl(k.age); # daily consumption
    prop <- freq*sachet*env.conc1(c(k.neighb,16)); #*area.hand(); why Peter multiply area? # sachet
    dose <- rpois(1,prop);
    return(c(0,dose));
  }
}
###########################################################################
