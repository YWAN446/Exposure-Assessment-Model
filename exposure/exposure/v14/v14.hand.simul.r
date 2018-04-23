library(stringr);
setwd("~/stat/sanipath/exposure/exposure/v14")
version <- "v14"

source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")
source("~/stat/sanipath/Andrew Document/Function to break down intake_032216.r")

child <- 1;
tot.pop <- 1;
neighbourhoods <- c("Alajo","Bukom","Old Fadama","Shiabu");

n.chain=1
n.iter <- 1*n.chain;
n.neighb <- 4;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

#k.neighb=1;
#k.age=1
num.mc <- 1;
max.states <- 10000;

flo.mou.contact<-function(n.hand,dur,comb.vec,comp,dirt=FALSE){
  ing <- c(0,0,0,0,0,0);
  n.hnd <- n.hand;
  time <- 0;
  i<-1
  n.hnd.seq<-c()
  n.ing.seq<-c()
  time.seq<-c()
  t.cnt <- exp(-freq.hand.surface());
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
  while(time + min(t.cnt,t.mth) < dur){
    time.seq[i]<-min(t.cnt,t.mth)
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
    n.hnd.seq[i]<-sum(n.hnd)
    n.ing.seq[i]<-sum(ing)
    i=i+1
  }
  return(list(n.hnd.seq,n.ing.seq,time.seq));
}

expos.by.state<-function(k.neighb,k.comp,k.behav,dur,n.hand,k.age,exclu.breast.feed){
  if(k.comp==1 & k.behav==1){                     # household particulate
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,12),k.comp,dirt=TRUE);
    return(res);
  }
  if(k.comp==2 & k.behav==1){                     # household swabs
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,27),k.comp,dirt=FALSE);
    return(res);
  }
  if(k.comp==3 & k.behav==1){
    res <- flo.mou.contact(n.hand,dur,c(k.neighb,28),k.comp,dirt=FALSE);
    return(res);      
    # household swabs (dry other) For now, we use the same environmental samples as concrete floor, we will separate them in the future!                                                            
  }
  if((k.comp==4 | k.comp==5) & k.behav==1){
    res <- hand.drain(env.conc(c(k.neighb,53)),n.hand);         # public drain near hh sm.env
    return(list(sum(res[1:6]),sum(res[7:12])));
  }
  if(k.behav==2){
    res <- sleep.mouthing(n.hand,dur)
    return(list(sum(res[1:6]),sum(res[7:12])));                        # no exposure
  }
  if(k.behav==3){
    res <- hand.wash(n.hand);
    return(list(sum(res[1:6]),0));                            # no exposure
  }
  if(k.behav==4){
    res <- hand.bathe(n.hand);
    return(list(sum(res[1:6]),0));                             # no exposure
  }
  if(k.comp==1 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,12)),k.comp,dirt=TRUE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(list(sum(res[1:6]),0)); 
  }
  if(k.comp==2 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,27)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(list(sum(res[1:6]),0)); 
  }
  if(k.comp==3 & k.behav==5){   # latrine septage
    res <- n.hand;
    if (k.age==3 & rbinom(n=1,size=1,prob=rbeta(1,24,18))==1) {
      for (i in 1:round(runif(1,1,10))){
        res <- hand.floor(res,env.conc(c(k.neighb,28)),k.comp,dirt=FALSE);
      }
    }
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])        # 5% probability???
    return(list(sum(res[1:6]),0)); 
  }
  if((k.comp==4 | k.comp==5) & k.behav==5){
    res <- n.hand;
    res <- ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.drain(env.conc(c(k.neighb,53)),res)); 
    res[5]<-ifelse(rbinom(n=1,size=1,prob=rbeta(1,5,41))==1,hand.defec(res,env.conc(c(k.neighb,31)))[5],res[5])
    return(list(sum(res[1:6]),0)); 
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
    return(list(sum(res[1:6]),sum(res[7:12]))); 
  }
}

hand.sample <- c()
cat(str_pad("Iteration:",10,side="right")," ");
for (iter in 1:3000){
  cat(str_pad(as.character(iter),5,side="left"));
  int.total<- c()
  hand.seq <- c()
  time.seq <- c()
  
  states <- gen.period.seq(k.neighb,3,2,14*60)
  n.states <- ncol(states);
  n.hand<-c(0,0,0,0,0,0)
  n.intake<-c(0,0,0,0,0,0)
  p.exclu.breast.feed <- ifelse(k.age==1,0.3388,ifelse(k.age==2,0.007,0))
  exclu.breast.feed<-rbinom(1,1,p.exclu.breast.feed)
  for (k.state in 1:n.states){
    if (states[2,k.state]==1 & states[1,k.state] %in% c(1,2,3)){
      res.list <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                                 states[3,k.state],n.hand,k.age,exclu.breast.feed);
      hand.seq<-c(hand.seq,res.list[[1]])
      int.total<-c(int.total,res.list[[2]])
      time.seq<-c(time.seq,res.list[[3]])
    } else {
      res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                            states[3,k.state],n.hand,k.age,exclu.breast.feed);
      hand.seq<-c(hand.seq,res[[1]])
      int.total<-c(int.total,res[[2]])
      time.seq<-c(time.seq,states[3,k.state])
    }
  }
  
  t.seq<-c()
  for (i in 1:length(time.seq)){
    if (i==1) {t.seq[i]=time.seq[i]}
    if (i>1) {t.seq[i]=t.seq[i-1]+time.seq[i]}
  }
  
  tot.time <- sum(time.seq)
  hand.sample[iter]<-sample(hand.seq,size=1,prob=time.seq/tot.time)
  cat("\b\b\b\b\b");
}


