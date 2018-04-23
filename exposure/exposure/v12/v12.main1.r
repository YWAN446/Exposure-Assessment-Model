library(stringr);
library(DescTools);
library(igraph);
setwd("~/stat/sanipath/exposure/exposure/v12")
version <- "v12"

source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")
source("~/stat/sanipath/Andrew Document/Function to break down intake_030715.r")
source('~/stat/sanipath/exposure/exposure/v12/NewFunctionForSensitivityAnalysis.r')

child <- 1;
tot.pop <- 1000;
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");

n.iter <- 1000;
n.neighb <- 4;
#n.neighb <- 4;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

num.mc <- 5000;
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
edge <- array(0,dim=c(144,3,num.mc,n.neighb));
corr.data <- array(0,dim=c(num.mc,33,n.neighb));
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
        }
      }
    }
    dw[k.neighb,1:2,k.mc] <- single[22:23,ncol(single)];
    int.total[k.neighb,k.mc] <- int.drain[k.neighb,k.mc]+int.dirt[k.neighb,k.mc]+int.offgr[k.neighb,k.mc]+int.flo[k.neighb,k.mc]+int.septage[k.neighb,k.mc]+
      int.produce[k.neighb,k.mc]+dw[k.neighb,1,k.mc]+dw[k.neighb,2,k.mc];
    cat("\b\b\b\b\b");
    
    mean.HW.dur<-mean(single[25,which(single[2,]==3)],na.rm=TRUE)
    mean.bathe.dur<-mean(single[27,which(single[2,]==4)],na.rm=TRUE)
    freq.HW<-length(which(single[2,]==3))
    freq.bathe<-length(which(single[2,]==4))
    freq.HW.soap<-length(which(single[24,]==1 & single[2,]==3))
    freq.bathe.soap<-length(which(single[26,]==1 & single[2,]==4))
    freq.defec<-length(which(single[2,]==5))
    freq.defec.touch<-length(which(single[28,]==1 & single[2,]==5))
    freq.defec.feces<-length(which(single[29,]==1 & single[2,]==5))
    freq.eating<-length(which(single[2,]==6))
    freq.eating.breast<-length(which(single[30,]==1 & single[2,]==6))
    freq.eating.pro<-length(which(single[31,]==1 & single[2,]==6))
    freq.eating.vendor<-length(which(single[32,]==1 & single[2,]==6))
    weight.eating<-mean(single[33,which(single[2,]==6)],na.rm=TRUE)
    by.hand.eating<-length(which(single[34,]==1 & single[2,]==6))
    com.hand.area<-sum(single[35,])
    com.hand.max<-max(single[37,])
    amount.tap<-single[40,ncol(single)]
    amount.sachet<-single[41,ncol(single)]
    freq.drain.swata<-length(which(single[1,]==4 | single[1,]==5))
    freq.swata<-length(which(single[1,]==4))
    freq.drain<-length(which(single[1,]==5))
    dur.dirt<-sum(single[3,which(single[1,]==1)])
    dur.floor<-sum(single[3,which(single[1,]==2)])
    dur.offgr<-sum(single[3,which(single[1,]==3)])
    
####network data extract######;
    #between.hand<-c();
    #w.between<-c();
    #degree.hand<-c();
    n.comb <- length(single[1,]);
    net.array <- array(0, dim=c(12,12,n.comb));
    k.iter <- 1;
    k.comp <- single[1,k.iter];
    k.behav <- single[2,k.iter];
    time <- single[3,];
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
      mat.labels <- c("Mouth","Hand","Food","Soil","Floor","Off-ground","drain","DF","HW","Bath","Tap water","Sachet water");
      edge <- array(0,dim=c(144*n.comb,4))
      for (t in 1:n.comb){
        for (i in 1:12){
          for (j in 1:12){
            edge[(t-1)*144+(i-1)*12+j,4] <- sum(as.numeric(time[1:t]));
            edge[(t-1)*144+(i-1)*12+j,1] <- mat.labels[i];
            edge[(t-1)*144+(i-1)*12+j,2] <- mat.labels[j];
            edge[(t-1)*144+(i-1)*12+j,3] <- net.array[i,j,t];
          }
        }
      }  
      edge1 <- edge[which(edge[,3]!=0),];
      edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3],time=edge1[,4]);
      edge.net<-graph.data.frame(edge2, directed=T);
      
      g <- graph.edgelist(as.matrix(edge2[,c(1,2)]),directed=T)
      E(g)$time <- as.numeric(as.vector(edge2[,4]))
      E(g)$conc <- as.numeric(as.vector(edge2[,3]))
      replace_end<-as.vector(edge2[which(E(g)$conc<0),1]);
      replace_start<-as.vector(edge2[which(E(g)$conc<0),2]);
      v.start<-as.vector(edge2[,1]);
      v.end<-as.vector(edge2[,2]);
      v.start[which(E(g)$conc<0)]<-replace_start;
      v.end[which(E(g)$conc<0)]<-replace_end;
      edge3<-data.frame(start=v.start,end=v.end, size=edge1[,3],time=edge1[,4]);
      g <- graph.edgelist(as.matrix(edge3[,c(1,2)]),directed=T)
      between.hand<-betweenness(g)["Hand"];
      degree.hand<-degree(g)["Hand"];
      degree.food<-degree(g)["Food"];
      degree.mouth<-degree(g)["Mouth"];
      degree.HW<-degree(g)["HW"];
      degree.bathe<-degree(g)["Bath"];
      w.between.hand<-betweenness(g,weights=abs(as.numeric(as.vector(edge2[,3]))))["Hand"];
  
      corr.data[k.mc,1,k.neighb]<-mean.HW.dur
      corr.data[k.mc,2,k.neighb]<-mean.bathe.dur
      corr.data[k.mc,3,k.neighb]<-freq.HW
      corr.data[k.mc,4,k.neighb]<-freq.bathe
      corr.data[k.mc,5,k.neighb]<-freq.HW.soap
      corr.data[k.mc,6,k.neighb]<-freq.bathe.soap
      corr.data[k.mc,7,k.neighb]<-freq.defec
      corr.data[k.mc,8,k.neighb]<-freq.defec.touch
      corr.data[k.mc,9,k.neighb]<-freq.defec.feces
      corr.data[k.mc,10,k.neighb]<-freq.eating
      corr.data[k.mc,11,k.neighb]<-freq.eating.breast
      corr.data[k.mc,12,k.neighb]<-freq.eating.pro
      corr.data[k.mc,13,k.neighb]<-freq.eating.vendor
      corr.data[k.mc,14,k.neighb]<-weight.eating
      corr.data[k.mc,15,k.neighb]<-by.hand.eating
      corr.data[k.mc,16,k.neighb]<-com.hand.area
      corr.data[k.mc,17,k.neighb]<-com.hand.max
      corr.data[k.mc,18,k.neighb]<-amount.tap
      corr.data[k.mc,19,k.neighb]<-amount.sachet
      corr.data[k.mc,20,k.neighb]<-freq.drain.swata
      corr.data[k.mc,21,k.neighb]<-freq.swata
      corr.data[k.mc,22,k.neighb]<-freq.drain
      corr.data[k.mc,23,k.neighb]<-dur.dirt
      corr.data[k.mc,24,k.neighb]<-dur.floor
      corr.data[k.mc,25,k.neighb]<-dur.offgr
      corr.data[k.mc,26,k.neighb]<-between.hand
      corr.data[k.mc,27,k.neighb]<-degree.hand
      corr.data[k.mc,28,k.neighb]<-degree.food
      corr.data[k.mc,29,k.neighb]<-degree.mouth
      corr.data[k.mc,30,k.neighb]<-degree.HW
      corr.data[k.mc,31,k.neighb]<-degree.bathe
      corr.data[k.mc,32,k.neighb]<-w.between.hand
      corr.data[k.mc,33,k.neighb]<-int.total[k.neighb,k.mc]
  }
}

corr.trace<-array(NA,dim=c(num.mc,33,n.neighb));

for (k.neighb in 1:n.neighb){
  for (k.mc in 1:num.mc){
    for (k.factor in 1:32){
      corr.trace[k.mc,k.factor,k.neighb]<-cor(corr.data[1:k.mc,33,k.neighb],corr.data[1:k.mc,k.factor,k.neighb])
    }
  }
}

if (k.age==1) {
  save(corr.data,file="~/stat/sanipath/exposure/exposure/v12/output/corr_data_1",ascii=TRUE);
  save(corr.trace,file="~/stat/sanipath/exposure/exposure/v12/output/corr_trace_1",ascii=TRUE);
}
if (k.age==2) {
  save(corr.data,file="~/stat/sanipath/exposure/exposure/v12/output/corr_data_2",ascii=TRUE);
  save(corr.trace,file="~/stat/sanipath/exposure/exposure/v12/output/corr_trace_2",ascii=TRUE);
}
if (k.age==3) {
  save(corr.data,file="~/stat/sanipath/exposure/exposure/v12/output/corr_data_3",ascii=TRUE);
  save(corr.trace,file="~/stat/sanipath/exposure/exposure/v12/output/corr_trace_3",ascii=TRUE);
}