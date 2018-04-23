exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.period.seq(k.neighb,3,2,tot.dur);
  n.states <- ncol(states);
  expos <- array(NA,dim=c(16,n.states));
  comp <- states[1,];
  behav <- states[2,];
  dur <- states[3,];
  n.hand <- c(0,0,0,0,0);
  for(k.state in 1:n.states){
    expos[1:3,k.state] <- states[,k.state];
    res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                          states[3,k.state],n.hand);
    if(length(res)==11){
      n.hand <- res[1:5];
      expos[4:8,k.state] <- n.hand;
      expos[9:14,k.state] <- res[6:11];
    }
  }
  expos[15:16,n.states] <- expos.dw(k.neighb,k.age,tot.dur);
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

expos.by.state<-function(k.neighb,k.comp,k.behav,dur,n.hand){
  res<-c();
  if(k.comp==1 & k.behav==1){                     # household particulate
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,12),k.comp,dirt=TRUE);
    return(c(res,0));
  }
  if(k.comp==2 & k.behav==1){                     # household swabs
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,27),k.comp,dirt=FALSE);
    return(c(res,0));
  }
  if(k.comp==3 & k.behav==1){
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,28),k.comp,dirt=FALSE);
    return(c(res,0));       
    # household swabs (dry other) For now, we use the same environmental samples as concrete floor, we will separate them in the future!                                                            
  }
  if((k.comp==4 | k.comp==5) & k.behav==1){
    res <- hand.drain(env.conc(c(k.neighb,53)),n.hand);         # public drain near hh sm.env
    return(c(res,0,0,0,0,0,0));
  }
  if(k.behav==2){
    res <- sleep.mouthing(n.hand,dur)
    return(c(res,0));                          # no exposure
  }
  if(k.behav==3){
    res <- hand.wash(n.hand);
    return(c(res,0,0,0,0,0,0));                             # no exposure
  }
  if(k.behav==4){
    res <- hand.bathe(n.hand);
    return(c(res,0,0,0,0,0,0));                             # no exposure
  }
  if(k.behav==5){   # latrine septage
    res <-n.hand;
    res[5]<-ifelse(rbinom(n=1,size=1,prob=0.05)==1,hand.defec(n.hand,env.conc(c(k.neighb,31)))[5],n.hand[5])        # 5% probability???
    return(c(res,0,0,0,0,0,0));
  }
  if(k.behav==6){   # farm food, market food, household food, street vended food
    intk.prod <- env.conc(choose.comb(prod.food(k.neighb))) *
      serving(prod=TRUE) #* fd.prodsmpl(k.neighb);
    intk.vend <- env.conc(choose.comb(vend.food(k.neighb))) *
      serving(prod=FALSE) #* fd.vendsmpl(k.neighb);
    return(c(n.hand,0,0,0,0,0,intk.prod + intk.vend));
  }
}

flo.mou.contact<-function(n.hand,dur,comb.vec,comp,dirt=FALSE){
  ing <- c(0,0,0,0,0);
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
      trans<-0;
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
  res <- next.state(c(n.hnd,0,0,0,0,0),c(p.att,p.det));
  return(res[6:10]);
}

next.state <- function(num.vec,p.vec){
  n.had <- c(0,0,0,0,0)
  n.intk <- c(0,0,0,0,0)
  for (i in 1:5){
  att <- ifelse(num.vec[i]>1e8, round(num.vec[i]*p.vec[1]),
                rbinom(n=1,size=num.vec[i],prob=p.vec[1]));
  det <- ifelse(num.vec[i+5]>1e8, round(num.vec[i+5]*p.vec[2]),
                rbinom(n=1,size=num.vec[i+5],prob=p.vec[2]));
  n.had[i] <- num.vec[i]-att+det;
  n.intk[i] <- num.vec[i+5]+att-det;
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
  # return(res[n.event,2]);
  if (comp==1) n.surf <- c(0,n.srf,0,0,0);
  if (comp==2) n.surf <- c(0,0,0,n.srf,0);
  if (comp==3) n.surf <- c(0,0,n.srf,0,0);
  if (comp==4) n.surf <- c(n.srf,0,0,0,0);
  if (comp==5) n.surf <- c(n.srf,0,0,0,0);
  res <- next.state(c(n.surf,n.hnd),c(p.att,p.det));
  if(dirt){
    n.srf <- num.surface(conc*adherence.soil.hand(),h.area);
    p.att <- 1; #For soil change unit from mg to cm2;
    n.surf <- c(0,n.srf,0,0,0);
    res[2] <- next.state(c(n.surf,n.hnd),c(p.att,p.det))[2];
    res[7] <- next.state(c(n.surf,n.hnd),c(p.att,p.det))[7];
  }
  return(res[6:10]);
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
  ing <- c(0,0,0,0,0);
  time <- 0;
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));# The frequency of mouth could be different between when child is sleeping and when child is playing, for now use the same value; 
  while (time+t.mth < dur){
    time<-time+t.mth;
    trans<-hand.mouth(n.hnd);
    n.hnd<-n.hnd-trans;
    ing<-ing+trans;
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    trans<-c(0,0,0,0,0);
  }
  return(c(n.hnd,ing))
}

hand.wash <- function(n.before){
  p.det<-c(0,0,0,0,0);
  res<-c();
  for (i in 1:5){
  p.det[i] <- ifelse(n.before[i]!=0,det.bact.handwash(n.before[i]),0);
  res[i] <- next.state(c(0,0,0,0,0,n.before),c(0,p.det[i]))[i];
  res[i+5] <- next.state(c(0,0,0,0,0,n.before),c(0,p.det[i]))[i+5];
  }
  return(res[6:10]);
}

hand.bathe <- function(n.before){
  p.det<-c(0,0,0,0,0);
  res<-c();
  for (i in 1:5){
    p.det[i] <- ifelse(n.before[i]!=0,det.bact.bathe(n.before[i]),0);
    res[i] <- next.state(c(0,0,0,0,0,n.before),c(0,p.det[i]))[i];
    res[i+5] <- next.state(c(0,0,0,0,0,n.before),c(0,p.det[i]))[i+5];
  }
  return(res[6:10]);
}

hand.defec <- function(n.before,conc){
  lam <- num.surface(conc*adherence.water.hand(),area.hand());
  n.feces <- ifelse(lam > 1e8, round(lam),rpois(n=1,lambda=lam));
  p.att <- 0.95;
  p.det <- 0
  res <- next.state(c(0,0,0,0,n.feces,n.before),c(p.att,p.det));
  return(res[6:10]);
}
