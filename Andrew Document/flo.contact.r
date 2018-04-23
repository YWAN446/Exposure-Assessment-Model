flo.mou.contact <- function(n.hand,dur,conc.surface,dirt=FALSE){
  hand.pathogen<-matrix(0,nrow=1000,ncol=2);
  intake.pathogen<-matrix(0,nrow=1000,ncol=2);
  #t.record1<-matrix(0,nrow=1000,ncol=6);
  #t.record2<-matrix(0,nrow=1000,ncol=6);
  n.hnd <- n.hand;
  intake<-0;
  time_t <- 0;
  time_m <- 0
  time_last_touch<-0;
  time_last_mouth<-0;
  time_unit<-dur/1000;
  t.cnt <- exp(-freq.hand.surface());
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
  for (i in 1:1000){
    #t.record1[i,1]<-t.cnt;
    #t.record1[i,2]<-t.mth;
    #t.record1[i,3]<-time_t;
    #t.record1[i,4]<-time_m;   
    #t.record1[i,5]<-time_last_touch;
    #t.record1[i,6]<-time_last_mouth;
    if (time_t-time_last_touch<t.cnt){
      hand.pathogen[i,1]<-time_t;
      hand.pathogen[i,2]<-n.hnd;     
    }
    if (time_t-time_last_touch>=t.cnt){
      n.hnd<-hand.floor(n.hnd,conc.surface,dirt);
      hand.pathogen[i,1]<-time_t;
      hand.pathogen[i,2]<-n.hnd;
      time_last_touch<-time_t;
      t.cnt <- exp(-freq.hand.surface());
    }
    
    if (time_m-time_last_mouth<t.mth){
      intake.pathogen[i,1]<-time_m;
      intake.pathogen[i,2]<-intake;     
    }
    if (time_m-time_last_mouth>=t.mth){
      trans<-hand.mouth(n.hnd);
      intake<-intake+trans;
      n.hnd<-n.hnd-trans;
      trans<-0;
      intake.pathogen[i,1]<-time_m;
      intake.pathogen[i,2]<-intake;
      hand.pathogen[i,2]<-n.hnd;
      time_last_mouth<-time_m;
      t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    }
    time_t<-time_t+time_unit;
    time_m<-time_m+time_unit;
    #t.record2[i,1]<-t.cnt;
    #t.record2[i,2]<-t.mth;
    #t.record2[i,3]<-time_t;
    #t.record2[i,4]<-time_m;   
    #t.record2[i,5]<-time_last_touch;
    #t.record2[i,6]<-time_last_mouth;
  }
  par(mfrow=c(2,1));
  plot(hand.pathogen[,1],hand.pathogen[,2],type="S",xlab="time",ylab="Contamination on the hands (CFU)",col="red");
  plot(intake.pathogen[,1],intake.pathogen[,2],type="S",xlab="time",ylab="Intake (CFU)",col="blue");
  par(mfrow=c(1,1));
  plot(hand.pathogen[,1],hand.pathogen[,2],type="S",xlab="time",ylab="Contamination(CFU)",col="red",ylim=range(0,max(hand.pathogen[,2])));
  lines(intake.pathogen[,1],intake.pathogen[,2],type="S",col="blue")
  legend("topleft", c("On Hands", "Intake"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"),box.col=NA ,bg = "transparent")
  return(c(n.hnd,intake))
  #return(cbind(hand.pathogen,intake.pathogen,t.record1,t.record2))
}

flo.mou.contact(0,10,1000000000)


sleep.mou.contact <- function(n.hand,dur,dirt=FALSE){
  hand.pathogen<-matrix(0,nrow=1000,ncol=2);
  intake.pathogen<-matrix(0,nrow=1000,ncol=2);
  n.hnd <- n.hand;
  intake<-0;
  time_t <- 0;
  time_m <- 0;
  time_last_mouth<-0;
  time_unit<-dur/1000;
  t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));# The frequency of mouth could be different between when child is sleeping and when child is playing, for now use the same value; 
  for (i in 1:1000){  
    if (time_m-time_last_mouth<t.mth){
      intake.pathogen[i,1]<-time_m;
      intake.pathogen[i,2]<-intake;
      hand.pathogen[i,1]<-time_t;
      hand.pathogen[i,2]<-n.hnd;
    }
    if (time_m-time_last_mouth>=t.mth){
      trans<-hand.mouth(n.hnd);
      intake<-intake+trans;
      n.hnd<-n.hnd-trans;
      trans<-0;
      intake.pathogen[i,1]<-time_m;
      intake.pathogen[i,2]<-intake;
      hand.pathogen[i,1]<-time_t;
      hand.pathogen[i,2]<-n.hnd;
      time_last_mouth<-time_m;
      t.mth <- exp(-freq.hand.mouth(outdoor=FALSE));
    }
    time_m<-time_m+time_unit;
    time_t<-time_t+time_unit;
  }
  par(mfrow=c(2,1));
  plot(hand.pathogen[,1],hand.pathogen[,2],type="S",xlab="time",ylab="Contamination on the hands (CFU)",col="red");
  plot(intake.pathogen[,1],intake.pathogen[,2],type="S",xlab="time",ylab="Intake (CFU)",col="blue");
  par(mfrow=c(1,1));
  plot(hand.pathogen[,1],hand.pathogen[,2],type="S",xlab="time",ylab="Contamination(CFU)",col="red",ylim=range(0,max(hand.pathogen[,2])));
  lines(intake.pathogen[,1],intake.pathogen[,2],type="S",col="blue")
  legend("topright", c("On Hands", "Intake"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"),box.col=NA ,bg = "transparent")
  return(c(n.hnd,intake))
}

sleep.mou.contact(10000000,5)
