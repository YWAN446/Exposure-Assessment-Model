
hand.mouth.sleep<-function(n.hnd){
  p.att <- det.bact.hand.mouth()*fraction.hand.in.mouth();
  p.det <- 0;
  res <- next.state(c(n.hnd,0),c(p.att,p.det));
  return(res[2]);
}

#assume only mouthing when children is sleeping and 
sleep.mouthing<-function(n.hand,dur){
  ing <- 0;
  n.hnd <- n.hand;
  time <- 0;
  while (time <= dur){
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE)); # This frequency should change in the future
    time <- time + t.mth;
    if (time<=dur){
    trans <- hand.mouth.sleep(n.hnd)
    ing <- ing + trans;
      # cat("ingested: ", ing,"\n");
    n.hnd<-n.hnd-trans;
    };
  }
  return(c(n.hnd,ing));
}

floor.contact <- function(n.hand,dur,comb.vec,dirt=FALSE){
  ing <- 0;
  n.hnd <- n.hand;
  time <- 0;
  while(time < dur){
    t.cnt <- exp(-freq.hand.surface());
    t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    time <- time + min(t.cnt,t.mth);
    if(t.mth < t.cnt){
      trans <- hand.mouth(n.hnd)
      ing <- ing + trans;
      n.hnd<- n.hnd - trans;
      # cat("ingested: ", ing,"\n");
    }else{
      n.hnd <- hand.floor(n.hnd,env.conc(comb.vec),dirt);
      # cat("hand: ",n.hnd," ");
    }
  }
  return(c(n.hnd,ing));
}