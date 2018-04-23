library(stringr);

version <- "v6"

source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")

child <- 1;
tot.pop <- 1000;
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");

n.iter <- 1000;
n.neighb <- 4;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

num.mc <- 1000;
max.states <- 500;
hm <- array(NA,dim=c(n.neighb,n.comp,n.behav,num.mc));
fd <- array(NA,dim=c(n.neighb,n.comp,n.behav,num.mc));
dw <- array(NA,dim=c(n.neighb,2,num.mc));
hand <- array(NA,dim=c(n.neighb,n.comp,n.behav,num.mc));
hand.seq <- array(NA,dim=c(n.neighb,3,max.states,num.mc));
for(k.neighb in 1:n.neighb){
  cat(str_pad(neighbourhoods[k.neighb],10,side="right")," ");
  for(k.mc in 1:num.mc){
    cat(str_pad(as.character(k.mc),5,side="left"));
    single <- exp.by.cat(k.neighb,k.age,14*60);
    n.states <- ncol(single);
    hand.seq[k.neighb,,1:n.states,k.mc] <- single[c(1,2,4),]
    for(k.comp in 1:n.comp){
      for(k.behav in 1:n.behav){
        sel <- (single[1,]==k.comp & single[2,]==k.behav);
        if(length(sel)>0){
          hand[k.neighb,k.comp,k.behav,k.mc] <- mean(single[4,sel],na.rm=TRUE);
          hm[k.neighb,k.comp,k.behav,k.mc] <- sum(single[5,sel],na.rm=TRUE);
          fd[k.neighb,k.comp,k.behav,k.mc] <- sum(single[6,sel],na.rm=TRUE);
        }
      }
    }
    dw[k.neighb,1:2,k.mc] <- single[7:8,ncol(single)];
    cat("\b\b\b\b\b");
  }
  cat("\n");
}
