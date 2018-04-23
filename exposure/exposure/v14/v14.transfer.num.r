#Hand contamination validation;
library(stringr);
setwd("~/stat/sanipath/exposure/exposure/v14")
version <- "v14"

source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")
source("~/stat/sanipath/Andrew Document/Function to break down intake_032216.r")

child <- 1;
tot.pop <- 1;
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");

n.chain=3
n.iter <- 1*n.chain;
n.neighb <- 4;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

num.mc <- 1000;
max.states <- 200;
hand.seq <- array(NA,dim=c(n.neighb,9,max.states,num.mc));
net.mat<-array(NA,dim=c(n.neighb,num.mc,12,12,max.states))
net.move.mat<-array(NA,dim=c(n.neighb,num.mc,6,max.states))
edge <- array(0,dim=c(144,3,num.mc,n.neighb));
for(k.neighb in 1:n.neighb){
  cat(str_pad(neighbourhoods[k.neighb],10,side="right")," ");
  for(k.mc in 1:num.mc){
    cat(str_pad(as.character(k.mc),5,side="left"));
    single <- exp.by.cat(k.neighb,k.age,14*60);
    n.states <- ncol(single);
    hand.seq[k.neighb,,1:n.states,k.mc] <- single[c(1,2,3,4,5,6,7,8,9),]
    
    cat("\b\b\b\b\b");
    
    ####network data extract######;    
    n.comb <- length(single[1,]);
    net.array <- array(0, dim=c(12,12,n.comb));
    net.move <- array(0, dim=c(2,n.comb));
    k.iter <- 1;
    k.comp <- single[1,k.iter];
    k.behav <- single[2,k.iter];
    net.move[1,k.iter] <- k.comp;
    net.move[2,k.iter] <- k.behav;
    #play/sit;
    if (k.comp==1 & k.behav==1) {
      net.array[4,2,k.iter] <- single[5,k.iter]+single[17,k.iter];
      net.array[2,1,k.iter] <- single[17,k.iter];
    }
    if (k.comp==2 & k.behav==1) {
      net.array[5,2,k.iter] <- single[7,k.iter]+single[19,k.iter];
      net.array[2,1,k.iter] <- single[19,k.iter];
    }
    if (k.comp==3 & k.behav==1) {
      net.array[6,2,k.iter] <- single[6,k.iter]+single[18,k.iter];
      net.array[2,1,k.iter] <- single[18,k.iter];
    }
    if ((k.comp==4 | k.comp==5) & k.behav==1) {
      net.array[7,2,k.iter] <- single[4,k.iter]+single[16,k.iter];
    }
    
    #defecate;
    if (k.comp==1 & k.behav==5) {
      net.array[4,2,k.iter] <- single[5,k.iter];
      net.array[8,2,k.iter] <- single[8,k.iter];
    }
    if (k.comp==2 & k.behav==5) {
      net.array[5,2,k.iter] <- single[7,k.iter];
      net.array[8,2,k.iter] <- single[8,k.iter];
    }
    if (k.comp==3 & k.behav==5) {
      net.array[6,2,k.iter] <- single[6,k.iter];
      net.array[8,2,k.iter] <- single[8,k.iter];
    }
    if ((k.comp==4 | k.comp==5) & k.behav==5) {
      net.array[7,2,k.iter] <- single[4,k.iter];
      net.array[8,2,k.iter] <- single[8,k.iter];
    }
    
    #eating;
    if (k.behav==6) {
      net.array[3,2,k.iter] <- single[9,k.iter];
      net.array[3,1,k.iter] <- single[21,k.iter];  
    }
    
    for (k.iter in 2:n.comb){
      k.comp <- single[1,k.iter];
      k.behav <- single[2,k.iter];
      net.move[1,k.iter] <- k.comp;
      net.move[2,k.iter] <- k.behav;
      #play/sit
      if (k.comp==1 & k.behav==1) {
        net.array[4,2,k.iter] <- single[5,k.iter]-single[5,k.iter-1]+sum(single[16:21,k.iter]);
        net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
      }
      if (k.comp==2 & k.behav==1) {
        net.array[5,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1]+sum(single[16:21,k.iter]);
        net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
      }
      if (k.comp==3 & k.behav==1) {
        net.array[6,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1]+sum(single[16:21,k.iter]);
        net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
      }
      if ((k.comp==4 | k.comp==5) & k.behav==1) {
        net.array[7,2,k.iter] <- single[4,k.iter]-single[4,k.iter-1];
      }
      
      #sleeping
      if (k.behav==2) {
        net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
      }
      #HW
      if (k.behav==3) {
        net.array[2,9,k.iter] <- sum(single[4:9,k.iter-1])-sum(single[4:9,k.iter]);
      }
      #bathe
      if (k.behav==4) {
        net.array[2,10,k.iter] <- sum(single[4:9,k.iter-1])-sum(single[4:9,k.iter]);
      }
      #defecate;
      if (k.comp==1 & k.behav==5) {
        net.array[4,2,k.iter] <- single[5,k.iter]-single[5,k.iter-1];
        net.array[8,2,k.iter] <- single[8,k.iter];
      }
      if (k.comp==2 & k.behav==5) {
        net.array[5,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1];
        net.array[8,2,k.iter] <- single[8,k.iter];
      }
      if (k.comp==3 & k.behav==5) {
        net.array[6,2,k.iter] <- single[6,k.iter]-single[6,k.iter-1];
        net.array[8,2,k.iter] <- single[8,k.iter];
      }
      if ((k.comp==4 | k.comp==5) & k.behav==5) {
        net.array[7,2,k.iter] <- single[4,k.iter];
        net.array[8,2,k.iter] <- single[8,k.iter];
      }
      #eating;
      if (k.behav==6) {
        net.array[3,2,k.iter] <- single[9,k.iter]-single[9,k.iter-1];
        net.array[3,1,k.iter] <- sum(single[16:21,k.iter]);
        net.array[2,3,k.iter] <- sum(single[4:8,k.iter-1])-sum(single[4:8,k.iter]);
      }
      if (k.iter==n.comb) {
        net.array[11,1,k.iter] <- single[22,k.iter];
        net.array[12,1,k.iter] <- single[23,k.iter];
      }
    }
    
    mat.labels <- c("Mouth","Hand","Food","Soil","Floor","Off-ground","Drain","Septage","HW","Bath","Tap water","Sachet water");
    
    net.mat[k.neighb,k.mc,,,1:dim(net.array)[3]]<-net.array[,,1:dim(net.array)[3]]
    net.move.mat[k.neighb,k.mc,1:2,1:dim(net.move)[2]]<-net.move[,1:dim(net.move)[2]]
  }
  cat("\n");
}

for (k.neighb in 1:4){
  for (i in 1:num.mc){
    net.move.mat[k.neighb,i,3,]<-c(NA,net.move.mat[k.neighb,i,1,-max.states]) #previous compartment
    net.move.mat[k.neighb,i,4,]<-c(net.move.mat[k.neighb,i,1,-1],NA) #following compartment
    net.move.mat[k.neighb,i,5,]<-c(NA,net.move.mat[k.neighb,i,2,-max.states]) #previous behavior
    net.move.mat[k.neighb,i,6,]<-c(net.move.mat[k.neighb,i,2,-1],NA) #following behavior
  }
}

na_0_rm<-function(a){
  b<-as.vector(a)
  return(b[!is.na(b) & b!=0])
}

label.neighb<-c("alajo","bukom","old-fadama","shiabu")
label.note<-mat.labels <- c("Mouth","Hand","Food","Soil","Floor","Off-ground","Drain","Septage","HW","Bath","Tap water","Sachet water")
ages <- c("0-1","1-2","2-5")
save(net.mat,file=paste("./output/net.mat",ages[k.age],".rda",sep=""))
save(net.move.mat,file=paste("./output/net.move.mat",ages[k.age],".rda",sep=""))
#load("./output/net.mat2-5.rda")
#load("./output/net.move.mat2-5.rda")
pdf(file=paste("./output/num.trans",ages[k.age],".pdf",sep=""))
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:12){
    for (k in 1:12){
      if (length(log10(na_0_rm(net.mat[i,,j,k,])))!=0){
        hist(log10(na_0_rm(net.mat[i,,j,k,])),main=paste(label.neighb[i]," ",label.note[j],"=>",label.note[k]),xlab="log10 microbes transferred")
      }
    }
  }
}
dev.off()
pdf(file=paste("./output/num.trans.","Bukom.","2-5",".pdf",sep=""))
par(mfrow=c(4,3))
i=2
for (j in 1:12){
  for (k in 1:12){
    if (length(log10(na_0_rm(net.mat[i,,j,k,])))!=0){
      hist(log10(na_0_rm(net.mat[i,,j,k,])),main=paste(label.note[j],"=>",label.note[k],", N=",length(log10(na_0_rm(net.mat[i,,j,k,])))),yaxt="n",freq=FALSE,xlab="log10 microbes transferred",xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,by=1))
      axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
    }
  }
}
dev.off()

pdf(file=paste("./output/num.trans.HWb4.",ages[k.age],".pdf",sep=""))
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:12){
    for (k in 1:12){
      if (length(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,3,]==3)])))!=0){
        hist(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,3,]==3)])),main=paste("HW before ",label.neighb[i],"\n",label.note[j],"=>",label.note[k]),xlab="log10 microbes transferred")
      }
    }
  }
}
dev.off()

pdf(file=paste("./output/num.trans.noHWb4.",ages[k.age],".pdf",sep=""))
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:12){
    for (k in 1:12){
      if (length(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,3,]!=3)])))!=0){
        hist(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,3,]!=3)])),main=paste("No HW before ",label.neighb[i],"\n",label.note[j],"=>",label.note[k]),xlab="log10 microbes transferred")
      }
    }
  }
}
dev.off()

pdf(file=paste("./output/num.trans.HWaf.",ages[k.age],".pdf",sep=""))
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:12){
    for (k in 1:12){
      if (length(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,4,]==3)])))!=0){
        hist(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,4,]==3)])),main=paste("HW after ",label.neighb[i],"\n",label.note[j],"=>",label.note[k]),xlab="log10 microbes transferred")
      }
    }
  }
}
dev.off()

pdf(file=paste("./output/num.trans.noHWaf.",ages[k.age],".pdf",sep=""))
par(mfrow=c(4,4))
for (i in 1:4){
  for (j in 1:12){
    for (k in 1:12){
      if (length(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,4,]!=3)])))!=0){
        hist(log10(na_0_rm(net.mat[i,,j,k,][which(net.move.mat[1,,4,]!=3)])),main=paste("No HW after ",label.neighb[i],"\n",label.note[j],"=>",label.note[k]),xlab="log10 microbes transferred")
      }
    }
  }
}
dev.off()
