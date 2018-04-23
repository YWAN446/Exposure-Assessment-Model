setwd("~/stat/sanipath/exposure/exposure/v9/")
load("HH0-1.rda")
load("HH1-2.rda")
load("HH2-5.rda")
load("Net.HH0-1.rda")
load("Net.HH1-2.rda")
load("Net.HH2-5.rda")

for (k.age in 1:3){
  if (k.age==1){
    int.dirt1<-HH.1[[14]]
    int.flo1<-HH.1[[16]]
    int.offgr1<-HH.1[[15]]
    int.drain1<-HH.1[[13]]
    int.septage1<-HH.1[[17]]
    int.produce1<-HH.1[[18]]
    dw1<-HH.1[[19]]
    int.total1<-HH.1[[20]]
  }
  if (k.age==2){
    int.dirt2<-HH.2[[14]]
    int.flo2<-HH.2[[16]]
    int.offgr2<-HH.2[[15]]
    int.drain2<-HH.2[[13]]
    int.septage2<-HH.2[[17]]
    int.produce2<-HH.2[[18]]
    dw2<-HH.2[[19]]
    int.total2<-HH.2[[20]]
  }
  if (k.age==3){
    int.dirt3<-HH.3[[14]]
    int.flo3<-HH.3[[16]]
    int.offgr3<-HH.3[[15]]
    int.drain3<-HH.3[[13]]
    int.septage3<-HH.3[[17]]
    int.produce3<-HH.3[[18]]
    dw3<-HH.3[[19]]
    int.total3<-HH.3[[20]]
  }
}

count<-function(num,limit,neighb,age){
  count1<-which(num>limit)
  count2<-count1[1:length(count1)-1]
  count3<-count1[2:length(count1)]
  reexp<-count3-count2-1
  table(reexp)
  hist(reexp,breaks=rep(0:max(reexp),each=2)+c(-0.5,+0.5),ylim=c(0,1),main=paste(neighb,age,", exposure>",limit,breaks=""))
}

par(mfrow=c(2,2))
count(int.total1[1,],1e5,"Alajo","0-1")
count(int.total1[2,],1e5,"Bukom","0-1")
count(int.total1[3,],1e5,"Old-Fadama","0-1")
count(int.total1[4,],1e5,"Shiabu","0-1")

count(int.total2[1,],1e5,"Alajo","1-2")
count(int.total2[2,],1e5,"Bukom","1-2")
count(int.total2[3,],1e5,"Old-Fadama","1-2")
count(int.total2[4,],1e5,"Shiabu","1-2")

count(int.total3[1,],1e5,"Alajo","2-5")
count(int.total3[2,],1e5,"Bukom","2-5")
count(int.total3[3,],1e5,"Old-Fadama","2-5")
count(int.total3[4,],1e5,"Shiabu","2-5")

count(int.total1[1,],1e6,"Alajo","0-1")
count(int.total1[2,],1e6,"Bukom","0-1")
count(int.total1[3,],1e6,"Old-Fadama","0-1")
count(int.total1[4,],1e6,"Shiabu","0-1")

count(int.total2[1,],1e6,"Alajo","1-2")
count(int.total2[2,],1e6,"Bukom","1-2")
count(int.total2[3,],1e6,"Old-Fadama","1-2")
count(int.total2[4,],1e6,"Shiabu","1-2")

count(int.total3[1,],1e6,"Alajo","2-5")
count(int.total3[2,],1e6,"Bukom","2-5")
count(int.total3[3,],1e6,"Old-Fadama","2-5")
count(int.total3[4,],1e6,"Shiabu","2-5")

count(int.total1[1,],1e7,"Alajo","0-1")
count(int.total1[2,],1e7,"Bukom","0-1")
count(int.total1[3,],1e7,"Old-Fadama","0-1")
count(int.total1[4,],1e7,"Shiabu","0-1")

count(int.total2[1,],1e7,"Alajo","1-2")
count(int.total2[2,],1e7,"Bukom","1-2")
count(int.total2[3,],1e7,"Old-Fadama","1-2")
count(int.total2[4,],1e7,"Shiabu","1-2")

count(int.total3[1,],1e7,"Alajo","2-5")
count(int.total3[2,],1e7,"Bukom","2-5")
count(int.total3[3,],1e7,"Old-Fadama","2-5")
count(int.total3[4,],1e7,"Shiabu","2-5")