#new function for sensitivity analysis;
exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.period.seq(k.neighb,3,2,tot.dur);
  n.states <- ncol(states);
  expos <- array(NA,dim=c(41,n.states));
  comp <- states[1,];
  behav <- states[2,];
  dur <- states[3,];
  n.hand <- c(0,0,0,0,0,0);
  n.food <- c(0,0,0,0,0,0);
  p.exclu.breast.feed <- ifelse(k.age==1,0.3388,ifelse(k.age==2,0.007,0));
  exclu.breast.feed<-rbinom(1,1,p.exclu.breast.feed);
  for(k.state in 1:n.states){
    expos[1:3,k.state] <- states[,k.state];
    res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                          states[3,k.state],n.hand,k.age,exclu.breast.feed);
    if(length(res)==32){
      n.hand <- res[1:6];
      expos[4:9,k.state] <- n.hand;
      expos[10:15,k.state] <- res[13:18];
      expos[16:21,k.state] <- res[7:12];
      expos[24:37,k.state] <- res[19:32]
    }
  }
  res.dw<-expos.dw(k.neighb,k.age,tot.dur);
  expos[22:23,n.states] <- res.dw[1:2];
  expos[38:41,n.states] <- res.dw[3:6];
  return(expos);
}

expos.by.state<-function(k.neighb,k.comp,k.behav,dur,n.hand,k.age,exclu.breast.feed){
  res<-c();
  if(k.comp==1 & k.behav==1){                     # household particulate
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,12),k.comp,dirt=TRUE);
    hand.com.area<-AUC(c(0,res[[2]][,1],dur),c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])),method="step");
    hand.com.mean<-hand.com.area/dur
    hand.com.max<-max(c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])))
    return(c(res[[1]],c(0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if(k.comp==2 & k.behav==1){                     # household swabs
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,27),k.comp,dirt=FALSE);
    hand.com.area<-AUC(c(0,res[[2]][,1],dur),c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])),method="step");
    hand.com.mean<-hand.com.area/dur
    hand.com.max<-max(c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])))
    return(c(res[[1]],c(0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if(k.comp==3 & k.behav==1){
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,28),k.comp,dirt=FALSE);
    hand.com.area<-AUC(c(0,res[[2]][,1],dur),c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])),method="step");
    hand.com.mean<-hand.com.area/dur
    hand.com.max<-max(c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])))
    return(c(res[[1]],c(0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));       
    # household swabs (dry other) For now, we use the same environmental samples as concrete floor, we will separate them in the future!                                                            
  }
  if((k.comp==4 | k.comp==5) & k.behav==1){
    res <- hand.drain(env.conc(c(k.neighb,53)),n.hand);         # public drain near hh sm.env
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if(k.behav==2){
    res <- sleep.mouthing(n.hand,dur);
    hand.com.area<-AUC(c(0,res[[2]][,1],dur),c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])),method="step");
    hand.com.mean<-hand.com.area/dur
    hand.com.max<-max(c(sum(n.hand),res[[2]][,2],sum(res[[1]][1:6])))
    return(c(res[[1]],c(0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));                          # no exposure
  }
  if(k.behav==3){
    HW.soap<-NA;
    HW.dur<-NA;
    res <- hand.wash(n.hand);
    HW.soap<-res[7]
    HW.dur<-res[8]
    hand.com.mean<-sum(res[1:6]);
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-hand.com.mean;
    return(c(res[1:6],c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(HW.soap,HW.dur,0,0,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));                             # no exposure
  }
  if(k.behav==4){
    bathe.soap<-NA;
    bathe.dur<-NA;
    res <- hand.bathe(n.hand);
    bathe.soap<-res[7]
    bathe.dur<-res[8]
    hand.com.mean<-sum(res[1:6]);
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-hand.com.mean;
    return(c(res[1:6],c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,bathe.soap,bathe.dur,0,0,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));                             # no exposure
  }
  if(k.comp==1 & k.behav==5){   # latrine septage
    res <- n.hand;
    touch.defec<-NA;
    touch.feces<-NA;
    touch.defec<-rbinom(n=1,size=1,prob=rbeta(1,24,18));
    touch.feces<-rbinom(n=1,size=1,prob=rbeta(1,5,41));
    if (k.age==3 & touch.defec==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,12)),k.comp,dirt=TRUE);
      }
    }
    res[5]<-ifelse(touch.feces==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])# 5% probability???
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,touch.defec,touch.feces,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if(k.comp==2 & k.behav==5){   # latrine septage
    res <- n.hand;
    touch.defec<-NA;
    touch.feces<-NA;
    touch.defec<-rbinom(n=1,size=1,prob=rbeta(1,24,18));
    touch.feces<-rbinom(n=1,size=1,prob=rbeta(1,5,41));
    if (k.age==3 & touch.defec==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,27)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(touch.feces==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])   # 5% probability???
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,touch.defec,touch.feces,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if(k.comp==3 & k.behav==5){   # latrine septage
    res <- n.hand;
    touch.defec<-NA;
    touch.feces<-NA;
    touch.defec<-rbinom(n=1,size=1,prob=rbeta(1,24,18));
    touch.feces<-rbinom(n=1,size=1,prob=rbeta(1,5,41));
    if (k.age==3 & touch.defec==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,28)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(touch.feces==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,touch.defec,touch.feces,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
  if((k.comp==4 | k.comp==5) & k.behav==5){
    res <- n.hand;
    touch.defec<-NA;
    touch.feces<-NA;
    touch.defec<-rbinom(n=1,size=1,prob=rbeta(1,5,41));
    touch.feces<-rbinom(n=1,size=1,prob=rbeta(1,5,41));
    res <- ifelse(touch.defec==1,hand.drain(env.conc(c(k.neighb,53)),res)); 
    res[5]<-ifelse(touch.feces==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,0,0,0,touch.defec,touch.feces,0,0,0,0,0),c(hand.com.mean,hand.com.area,hand.com.max)));
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
    food.type.breast<-ifelse(exclu.breast.feed==1 | breast.feed==1,1,0)
    food.type.pro<-ifelse(exclu.breast.feed==1 | breast.feed==1,0,ifelse(eat.pro==1,1,0))
    food.type.vendor<-ifelse(exclu.breast.feed==1 | breast.feed==1,0,ifelse(eat.pro==1,0,1))
    weight.food<-NA;
    eat.by.hand<-NA;
    if (exclu.breast.feed==1){
      num.food <- 0; #assume no intake for breast feeding;
      res<-c(n.hand,c(0,0,0,0,0,0),c(0,0,0,0,0,num.food))
    }
    if (exclu.breast.feed==0 & breast.feed==1){
      num.food <- 0; #assume no intake for breast feeding;
      res<-c(n.hand,c(0,0,0,0,0,0),c(0,0,0,0,0,num.food))
    }
    if (exclu.breast.feed==0 & breast.feed==0 & eat.pro==1){
      weight.food<-serving(prod=TRUE)
      num.food <- round(env.conc(choose.comb(prod.food(k.neighb))) *
                          weight.food) #* fd.prodsmpl(k.neighb);
      n.food <- c(0,0,0,0,0,num.food);
      res<-hand.produce.contact(n.hand,n.food);
      eat.by.hand<-1;
    }
    if (exclu.breast.feed==0 & breast.feed==0 & eat.pro==0){
      weight.food<-serving(prod=FALSE)
      num.food <- round(env.conc(choose.comb(vend.food(k.neighb))) *
                          weight.food) #* fd.vendsmpl(k.neighb); #actually is prepared food including salad and fish
      n.food <- c(0,0,0,0,0,num.food);
      res0 <- hand.food.contact(n.hand,n.food);
      res<-res0[1:18]
      eat.by.hand<-res0[19]
    }
    hand.com.mean<-(sum(res[1:6])+sum(n.hand))/2;
    hand.com.area<-hand.com.mean*dur;
    hand.com.max<-max(sum(res[1:6]),sum(n.hand));
    return(c(res,c(0,0,0,0,0,0,food.type.breast,food.type.pro,food.type.vendor,weight.food,eat.by.hand),c(hand.com.mean,hand.com.area,hand.com.max)));
  }
}

hand.wash<-function(n.before){
  res<-c();
  det.handwash<-det.bact.handwash()
  p.det <- ifelse(sum(n.before)>1000,det.handwash[1],
                  ifelse(sum(n.before)!=0,rtruncnorm(1,0,0.99,0.5,0.2),0));#made up numbers
  res<-next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))
  #for (i in 1:6){
  #  res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i];
  #  res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i+6];
  #}
  return(c(res[7:12],det.handwash[2:3]));
}

det.bact.handwash<-function(void){
  soap <- hand.soap();
  dur <- 10+rgamma(1,shape=2.5,scale=10); #arbitrary number >10s and mean 35s
  if (soap==TRUE) r<- rnorm(1,-2.1795,0.5132)+rnorm(1,1.1922,0.1436)*log(dur);
  if (soap==FALSE) r<- rnorm(1,-0.6836,0.6036)+rnorm(1,0.8001,0.1394)*log(dur);
  if (r<0) r<-0.1;
  return(c(1-0.1^r,soap,dur));
}

hand.bathe<-function(n.before){
  res<-c();
  det.bathe<-det.bact.bathe();
  p.det <- ifelse(sum(n.before)>1000,det.bathe[1],
                  ifelse(sum(n.before)!=0,rtruncnorm(1,0,0.99,0.5,0.2),0));
  res<-next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))
  #for (i in 1:6){
  #  res[i] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i];
  #  res[i+6] <- next.state(c(0,0,0,0,0,0,n.before),c(0,p.det))[i+6];
  #}
  return(c(res[7:12],det.bathe[2:3]));
}

det.bact.bathe<-function(void){
  soap <- hand.soap();
  dur <- 60+rgamma(1,shape=4,scale=60); #arbitrary number >60s and mean 5 min
  if (soap==TRUE) r<- rnorm(1,-2.1795,0.5132)+rnorm(1,1.1922,0.1436)*log(dur);
  if (soap==FALSE) r<- rnorm(1,-0.6836,0.6036)+rnorm(1,0.8001,0.1394)*log(dur);
  if (r<0) r<-0.1;
  return(c(1-0.1^r,soap,dur));
}

expos.dw<-function(k.neighb,k.age,tot.dur){
  multiplier <- tot.dur/(24*60);
  multiplier <- ifelse(multiplier < 0.6,multiplier,round(multiplier));
  tapwater <- is.tap(k.neighb); # household large water samples (4 (large) or 5(small)?)
  if(tapwater){
    freq <- multiplier*dw.tapsmpl(k.age); # daily consumption
    prop <- freq*cup*env.conc1(choose.comb(drinking.water(k.neighb)));
    dose <- rpois(1,prop);
    return(c(dose,0,tapwater,1-tapwater,freq*cup,0));
  }
  if(!tapwater){
    freq <- multiplier*dw.sacsmpl(k.age); # daily consumption
    prop <- freq*sachet*env.conc1(c(k.neighb,16)); #*area.hand(); why Peter multiply area? # sachet
    dose <- rpois(1,prop);
    return(c(0,dose,tapwater,1-tapwater,0,freq*sachet));
  }
}

hand.food.contact<-function(n.hand,n.food){
  eat.by.hand<-hand.eat()
  if (eat.by.hand==1){
    p.att <- att.floor.hand();
    p.det <- det.floor.hand(); # assume all area of one hand touch the food, part of food were eaten by hands.
    ing <- c(0,0,0,0,0,0);
    state1 <- c();
    state1 <- next.state(c(n.food,n.hand),c(p.att,p.det));
    n.hand <- state1[7:12];
    n.food <- state1[1:6];
  }
  ing <- n.food;
  return(c(n.hand,ing,n.food,eat.by.hand));
}

flo.mou.contact<-function(n.hand,dur,comb.vec,comp,dirt=FALSE){
  hand.pathogen1<-c()
  intake.pathogen1<-c()
  hand.pathogen2<-c()
  intake.pathogen2<-c()
  ing <- c(0,0,0,0,0,0);
  n.hnd <- n.hand;
  time <- 0;
  t.cnt <- exp(-freq.hand.surface());
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
  ht.iter<-0;
  hm.iter<-0;
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
      hm.iter=hm.iter+1;
      ht.iter=ht.iter+1;
      intake.pathogen1[hm.iter]<-time;
      intake.pathogen2[hm.iter]<-sum(ing);
      hand.pathogen1[ht.iter]<-time;
      hand.pathogen2[ht.iter]<-sum(n.hnd);
    }else{
      t.mth<-t.mth-t.cnt;
      t.cnt <- exp(-freq.hand.surface());
      n.hnd <- hand.floor(n.hnd,env.conc(comb.vec),comp,dirt);
      # cat("hand: ",n.hnd," ");
      ht.iter=ht.iter+1;
      hand.pathogen1[ht.iter]<-time;
      hand.pathogen2[ht.iter]<-sum(n.hnd);
    }
  }
  hand.pathogen<-cbind(hand.pathogen1,hand.pathogen2);
  intake.pathogen<-cbind(intake.pathogen1,intake.pathogen2);
  return(list(c(n.hnd,ing),hand.pathogen,intake.pathogen));
}

sleep.mouthing<-function(n.hand,dur,dirt=FALSE){
  hand.pathogen1<-c()
  intake.pathogen1<-c()
  hand.pathogen2<-c()
  intake.pathogen2<-c()
  n.hnd <- n.hand;
  ing <- c(0,0,0,0,0,0);
  time <- 0;
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));# The frequency of mouth could be different between when child is sleeping and when child is playing, for now use the same value; 
  ht.iter<-0;
  hm.iter<-0;
  while (time+t.mth < dur){
    time<-time+t.mth;
    trans<-hand.mouth(n.hnd);
    n.hnd<-n.hnd-trans;
    ing<-ing+trans;
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    trans<-c(0,0,0,0,0,0);
    hm.iter=hm.iter+1;
    ht.iter=ht.iter+1;
    intake.pathogen1[hm.iter]<-time;
    intake.pathogen2[hm.iter]<-sum(ing);
    hand.pathogen1[ht.iter]<-time;
    hand.pathogen2[ht.iter]<-sum(n.hnd);
  }
  hand.pathogen<-cbind(hand.pathogen1,hand.pathogen2);
  intake.pathogen<-cbind(intake.pathogen1,intake.pathogen2);
  return(list(c(n.hnd,ing),hand.pathogen,intake.pathogen))
}
