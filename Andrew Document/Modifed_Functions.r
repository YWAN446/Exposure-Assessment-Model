#floor.contact <- function(n.hand,dur,comb.vec,dirt=FALSE){
#  ing <- 0;
#  n.hnd <- n.hand;
#  time <- 0;
#  t.record<-matrix(0,nrow=100,ncol=6);
#  i<-1;
#  t.cnt <- exp(-freq.hand.surface());
#  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
#  while(time + min(t.cnt,t.mth) < dur){
#    t.record[i,1]<-time;
#    t.record[i,2]<-t.cnt;
#    t.record[i,3]<-t.mth;
#    t.record[i,4]<-n.hnd;
#    t.record[i,5]<-ing;
#    time <- time + min(t.cnt,t.mth);
#    if(t.mth < t.cnt){
#      t.cnt<-t.cnt-t.mth;
#      t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
#      trans <- hand.mouth(n.hnd);
#      ing <- ing + trans;
#      n.hnd<- n.hnd - trans;
#      # cat("ingested: ", ing,"\n");
#      t.record[i,6]<-trans;
#      trans<-0;
#    }else{
#      t.mth<-t.mth-t.cnt;
#      t.cnt <- exp(-freq.hand.surface());
#      n.hnd <- hand.floor(n.hnd,1000000000,dirt);
      #env.conc(comb.vec)<-1000000000;
      # cat("hand: ",n.hnd," ");
#    }
#  i=i+1;
#  }
#  t.record[i,1]<-time;
#  t.record[i,2]<-t.cnt;
#  t.record[i,3]<-t.mth;
#  t.record[i,4]<-n.hnd;
#  t.record[i,5]<-ing;
#  t.record[i,6]<-trans;
#  print(t.record);
#  return(c(n.hnd,ing));
#}

flo.mou.contact <- function(n.hand,dur,comb.vec,dirt=FALSE){
  ing <- 0;
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
      n.hnd <- hand.floor(n.hnd,env.conc(comb.vec),dirt);
      # cat("hand: ",n.hnd," ");
    }
  }
  return(c(n.hnd,ing));
}

sleep.mouthing <- function(n.hand,dur,dirt=FALSE){
  n.hnd <- n.hand;
  ing<-0;
  time <- 0;
#  t.record<-matrix(0,nrow=100,ncol=5);
#  i<-0;
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));# The frequency of mouth could be different between when child is sleeping and when child is playing, for now use the same value; 
  while (time+t.mth < dur){
    time<-time+t.mth;
    trans<-hand.mouth(n.hnd);
    n.hnd<-n.hnd-trans;
    ing<-ing+trans;
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
#    t.record[i,1]<-time;
#    t.record[i,2]<-t.mth;
#    t.record[i,3]<-n.hnd;
#    t.record[i,4]<-ing;
#    t.record[i,5]<-trans;
    trans<-0;
#    i<-i+1;
  }
#  print(t.record)
  return(c(n.hnd,ing))
}

#sleep.mouthing(10000000,5)

num.surface<-function(conc,area){
  lam <- area*conc;
  if(lam > 1e8) return(round(lam));
  return(rpois(n=1,lambda=lam));
}

hand.floor<-function(n.hnd,conc,dirt){
  h.area <- area.hand();
  n.srf <- num.surface(conc,h.area);
  p.att <- att.floor.hand();
  p.det <- det.floor.hand()*h.area/55; # 55 cm2 1 palm area
  if(dirt){
    n.srf <- num.surface(conc*adherence.soil.hand(),h.area);
    p.att <- 1; #For soil change unit from mg to cm2;
    p.det <- det.dirt.hand();
  }
  # n.event <- 1 + rpois(n=1,lambda=2);
  # res <- sim.1(n.srf+n.hnd,n.hnd,p.att,p.det,n.event);
  # return(res[n.event,2]);
  res <- next.state(c(n.srf,n.hnd),c(p.att,p.det));
  return(res[2]);
}

area.hand<-function(void) exp(rtruncnorm(n=1,mean=2.75,sd=0.75,a=0,b=4))
att.floor.hand<-function(void) rtriangle(n=1,a=0.01,b=0.19,c=0.1)
det.floor.hand<-function(void) rtriangle(n=1,a=0.25,b=0.75,c=0.5)
det.dirt.hand<-function(void) rtriangle(n=1,a=0.1,b=0.3,c=0.2)

exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.period.seq(k.neighb,3,2,tot.dur);
  n.states <- ncol(states);
  expos <- array(NA,dim=c(8,n.states));
  comp <- states[1,];
  behav <- states[2,];
  dur <- states[3,];
  n.hand <- 0;
  for(k.state in 1:n.states){
    expos[1:3,k.state] <- states[,k.state];
    res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                          states[3,k.state],n.hand);
    if(length(res)==2){
      n.hand <- res[1];
      expos[4,k.state] <- n.hand;
      if(states[2,k.state]==6){
        expos[6,k.state] <- res[2];
      }else{
        expos[5,k.state] <- res[2];
      }
    }
  }
  expos[7:8,n.states] <- expos.dw(k.neighb,k.age,tot.dur);
  return(expos);
}

hand.wash<-function(n.before){
  p.det <- ifelse(n.before!=0,det.bact.handwash(n.before),0);
  res <- next.state(c(0,n.before),c(0,p.det));
  return(res[2]);
}

hand.bathe<-function(n.before){
  p.det <- ifelse(n.before!=0,det.bact.bathe(n.before),0);
  res <- next.state(c(0,n.before),c(0,p.det));
  return(res[2]);
}

#det.bact.handwash<-function(n.before,soap=FALSE){
#  p=10^(log10(n.before)-rnorm(n=1,mean=(log10(n.before)*0.45-0.6),sd=log10(n.before)*0.025))/n.before;
#  soap <- hand.soap();
#  if(soap) p=10^(log10(n.before)-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05))/n.before;
#  if (p>1) p=runif(n=1,min=0.05,max=0.95);
#  return(1-p);
#}

det.bact.handwash<-function(n.before,soap=FALSE){
  p=10^(-rnorm(n=1,mean=(log10(n.before)*0.45-0.6),sd=log10(n.before)*0.025));
  soap <- hand.soap();
  if(soap) p=10^(-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05));
  if (p>1) p=runif(n=1,min=0.05,max=0.95);
  return(1-p);
}

#det.bact.bathe<-function(n.before,soap=FALSE){
#  p=10^(log10(n.before)-rnorm(n=1,mean=(log10(n.before)*0.45-0.6),sd=log10(n.before)*0.025))/n.before;
#  soap <- bath.soap();
#  if(soap) p=10^(log10(n.before)-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05))/n.before;
#  if (p>1) p=runif(n=1,min=0.05,max=0.95);
#  return(1-p);
#}

det.bact.bathe<-function(n.before,soap=FALSE){
  p=10^(-rnorm(n=1,mean=(log10(n.before)*0.45-0.6),sd=log10(n.before)*0.025));
  soap <- bath.soap();
  if(soap) p=10^(-rnorm(n=1,mean=log10(n.before)*0.5,sd=log10(n.before)*0.05));
  if (p>1) p=runif(n=1,min=0.05,max=0.95);
  return(1-p);
}

adherence.water.hand<-function(void) area.hand()*rnorm(n=1,mean=5.4,sd=0.5)/1000

#assume prior distribution of p is unif(0,1), likelihood is binomial distribution, then the posterior distribution of p is beta(y+1,n-y+1);
hand.soap<-function(void) rbinom(1,1,rbeta(1,5,17))  #data: 4 with soap, 16 w/o soap for handwash;
bath.soap<-function(void) rbinom(1,1,rbeta(1,45,19)) #data: 44 with soap, 18 w/o soap for bathing;

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

hand.wash<-function(n.before){
  p.det <- ifelse(n.before!=0,det.bact.handwash(),0);
  res <- next.state(c(0,n.before),c(0,p.det));
  if (res[2]<=10) res[2]<-round(runif(1,1,10));
  return(res[2]);
}

hand.bathe<-function(n.before){
  p.det <- ifelse(n.before!=0,det.bact.bathe(),0);
  res <- next.state(c(0,n.before),c(0,p.det));
  if (res[2]<=10) res[2]<-round(runif(1,1,10));
  return(res[2]);
}

