library(stringr);
library(igraph);
setwd("~/stat/sanipath/exposure/exposure/v9/")
load("Mat1.rda")
load("Mat2.rda")
load("Mat3.rda")

#source("~/stat/sanipath/exposure/exposure/v6/v6.load.r")
version <- "v9"
source("~/stat/sanipath/Andrew Document/Modifed_Functions.r")
source("~/stat/sanipath/Andrew Document/Function to break down intake_030715.r")

child <- 1;
tot.pop <- 1000;
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");

n.iter <- 1000;
n.neighb <- 2;
n.comp <- 5;
n.behav <- 6;

r <- hh.r;
lambda.mc <- hh.lambda;

num.mc <- 1000;
max.states <- 500;
Mat<-list(single)
#table(single[2,])

#save(single,file="single1.rda",ascii=TRUE);


#different k.age should use different v6.load.r
n.neighb=4
num.mc=1000
for(k.neighb in 1:n.neighb){
  for(k.mc in 1:num.mc){
    k.mat<-(k.neighb-1)*1000+k.mc
    Mat[[k.mat]] <- exp.by.cat(k.neighb,k.age,14*60);
  }
}



between<-c();
w.between<-c();
for (k in 1:4000){
  single<-Mat[[k]]
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
  between[k]<-betweenness(g)["Hand"];
  w.between[k]<-betweenness(g,weights=abs(as.numeric(as.vector(edge2[,3]))))["Hand"];
}

#Mat1<-Mat
#between1<-between
#save(Mat3,between3,file="Mat3.rda",ascii=TRUE);
#save(Mat2,between2,file="Mat2.rda",ascii=TRUE);
#save(Mat1,between1,file="Mat1.rda",ascii=TRUE);

options(digits=4)
#age 0-1
mean(between1[1:1000]) #alajo
mean(between1[1001:2000]) #bukom
mean(between1[2001:3000]) #old-fadama
mean(between1[3001:4000]) #shiabu

#age 1-2
mean(between2[1:1000]) #alajo
mean(between2[1001:2000]) #bukom
mean(between2[2001:3000]) #old-fadama
mean(between2[3001:4000]) #shiabu

#age 2-5
mean(between3[1:1000]) #alajo
mean(between3[1001:2000]) #bukom
mean(between3[2001:3000]) #old-fadama
mean(between3[3001:4000]) #shiabu

#by age group
mean(between1[1:4000]) #0-1
mean(between2[1:4000]) #1-2
mean(between3[1:4000]) #2-5

#by neighborhood
mean(c(between1[1:1000],between2[1:1000],between3[1:1000])) #alajo
mean(c(between1[1001:2000],between2[1001:2000],between3[1001:2000])) #bukom
mean(c(between1[2001:3000],between2[2001:3000],between3[2001:3000])) #old-fadama
mean(c(between1[3001:4000],between2[3001:4000],between3[3001:4000])) #shiabu

#total
mean(c(between1[1:4000],between2[1:4000],between3[1:4000]))