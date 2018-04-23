library(stringr);
setwd("~/stat/sanipath/exposure/exposure/v14")
version <- "v14"

source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")
source("~/stat/sanipath/Andrew Document/Function to break down intake_032216.r")

child <- 1;
tot.pop <- 10000;
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");

n.chain=3
n.iter <- 1000*n.chain;
n.neighb <- 4;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

num.mc <- 10000;
max.states <- 500;
hand.dra <- array(NA,dim=c(n.neighb,num.mc));
hand.dirt <- array(NA,dim=c(n.neighb,num.mc));
hand.offgr <- array(NA,dim=c(n.neighb,num.mc));
hand.flo <- array(NA,dim=c(n.neighb,num.mc));
hand.septage <- array(NA,dim=c(n.neighb,num.mc));
hand.produce <- array(NA,dim=c(n.neighb,num.mc));
food.dra <- array(NA,dim=c(n.neighb,num.mc));
food.dirt <- array(NA,dim=c(n.neighb,num.mc));
food.offgr <- array(NA,dim=c(n.neighb,num.mc));
food.flo <- array(NA,dim=c(n.neighb,num.mc));
food.septage <- array(NA,dim=c(n.neighb,num.mc));
food.produce <- array(NA,dim=c(n.neighb,num.mc));
int.drain <- array(NA,dim=c(n.neighb,num.mc));
int.dirt <- array(NA,dim=c(n.neighb,num.mc));
int.offgr <- array(NA,dim=c(n.neighb,num.mc));
int.flo <- array(NA,dim=c(n.neighb,num.mc));
int.septage <- array(NA,dim=c(n.neighb,num.mc));
int.produce <- array(NA,dim=c(n.neighb,num.mc));
dw <- array(NA,dim=c(n.neighb,2,num.mc));
int.total<-array(NA,dim=c(n.neighb,num.mc));
hand.seq <- array(NA,dim=c(n.neighb,8,max.states,num.mc));
dir.indir.produce <- array(NA,dim=c(n.neighb,2,num.mc));
edge <- array(0,dim=c(144,3,num.mc,n.neighb));
for(k.neighb in 1:n.neighb){
  cat(str_pad(neighbourhoods[k.neighb],10,side="right")," ");
  for(k.mc in 1:num.mc){
    cat(str_pad(as.character(k.mc),5,side="left"));
    single <- exp.by.cat(k.neighb,k.age,14*60);
    n.states <- ncol(single);
    hand.seq[k.neighb,,1:n.states,k.mc] <- single[c(1,2,4,5,6,7,8,9),]
    for(k.comp in 1:n.comp){
      for(k.behav in 1:n.behav){
        sel <- (single[1,]==k.comp & single[2,]==k.behav);
        if(length(sel)>0){
          hand.dra[k.neighb,k.mc] <- mean(single[4,],na.rm=TRUE);
          hand.dirt[k.neighb,k.mc] <- mean(single[5,],na.rm=TRUE);
          hand.offgr[k.neighb,k.mc] <- mean(single[6,],na.rm=TRUE);
          hand.flo[k.neighb,k.mc] <- mean(single[7,],na.rm=TRUE);
          hand.septage[k.neighb,k.mc] <- mean(single[8,],na.rm=TRUE);
          hand.produce[k.neighb,k.mc] <- mean(single[9,],na.rm=TRUE);
          food.dra[k.neighb,k.mc] <- mean(single[10,],na.rm=TRUE);
          food.dirt[k.neighb,k.mc] <- mean(single[11,],na.rm=TRUE);
          food.offgr[k.neighb,k.mc] <- mean(single[12,],na.rm=TRUE);
          food.flo[k.neighb,k.mc] <- mean(single[13,],na.rm=TRUE);
          food.septage[k.neighb,k.mc] <- mean(single[14,],na.rm=TRUE);
          food.produce[k.neighb,k.mc] <- mean(single[15,],na.rm=TRUE);          
          int.drain[k.neighb,k.mc] <- sum(single[16,],na.rm=TRUE);
          int.dirt[k.neighb,k.mc] <- sum(single[17,],na.rm=TRUE);
          int.offgr[k.neighb,k.mc] <- sum(single[18,],na.rm=TRUE);
          int.flo[k.neighb,k.mc] <- sum(single[19,],na.rm=TRUE);
          int.septage[k.neighb,k.mc] <- sum(single[20,],na.rm=TRUE);
          int.produce[k.neighb,k.mc] <- sum(single[21,],na.rm=TRUE);
          dir.indir.produce[k.neighb,1,k.mc] <- sum(single[21,which(single[2,]==6)])
          dir.indir.produce[k.neighb,2,k.mc] <- sum(single[21,which(single[2,]!=6)])
        }
      }
    }
    dw[k.neighb,1:2,k.mc] <- single[22:23,ncol(single)];
    int.total[k.neighb,k.mc] <- int.drain[k.neighb,k.mc]+int.dirt[k.neighb,k.mc]+int.offgr[k.neighb,k.mc]+int.flo[k.neighb,k.mc]+int.septage[k.neighb,k.mc]+
      int.produce[k.neighb,k.mc]+dw[k.neighb,1,k.mc]+dw[k.neighb,2,k.mc];
    cat("\b\b\b\b\b");
  }
  cat("\n");
}

assign(paste("HH.",k.age,sep=""),list(hand.dra,hand.dirt,hand.offgr,hand.flo,hand.septage,hand.produce,food.dra,food.dirt,food.offgr,food.flo,food.septage,food.produce,
                                      int.drain,int.dirt,int.offgr,int.flo,int.septage,int.produce,dw,int.total,dir.indir.produce))
filesave <- paste("HH.dir.indir",ifelse(k.age==1,"0-1",ifelse(k.age==2,"1-2","2-5")),".rda",sep="")

if (k.age==1) {
  save(HH.1,file=filesave,ascii=TRUE);
}
if (k.age==2) {
  save(HH.2,file=filesave,ascii=TRUE);
}
if (k.age==3) {
  save(HH.3,file=filesave,ascii=TRUE);
}