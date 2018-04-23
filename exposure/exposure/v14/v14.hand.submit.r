source("~/stat/sanipath/exposure/exposure/v14/v6.load0.r")
k.neighb=1;
k.age=1
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n1a1 <- hand.sample

k.neighb=2;
k.age=1
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n2a1 <- hand.sample

k.neighb=3;
k.age=1
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n3a1 <- hand.sample

k.neighb=4;
k.age=1
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n4a1 <- hand.sample

source("~/stat/sanipath/exposure/exposure/v14/v6.load1.r")
k.neighb=1;
k.age=2
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n1a2 <- hand.sample

k.neighb=2;
k.age=2
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n2a2 <- hand.sample

k.neighb=3;
k.age=2
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n3a2 <- hand.sample

k.neighb=4;
k.age=2
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n4a2 <- hand.sample

source("~/stat/sanipath/exposure/exposure/v14/v6.load2.r")
k.neighb=1;
k.age=3
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n1a3 <- hand.sample

k.neighb=2;
k.age=3
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n2a3 <- hand.sample

k.neighb=3;
k.age=3
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n3a3 <- hand.sample

k.neighb=4;
k.age=4
source("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.r")
hand.n4a3 <- hand.sample

hand<-list(hand.n1a1,hand.n2a1,hand.n3a1,hand.n4a1,hand.n1a2,hand.n2a2,hand.n3a2,hand.n4a2,hand.n1a3,hand.n2a3,hand.n3a3,hand.n4a3)
save(hand,file="hand_simul.rda")

pdf("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul.pdf")
neighborhoods <- c("Alajo","Bukom","Old Fadama","Shiabu");
ages <- c("0-1","1-2","2-5")
par(mfrow=c(2,2))
hist(log10(hand.n1a1),freq=FALSE,main=paste(neighborhoods[1],ages[1]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n2a1),freq=FALSE,main=paste(neighborhoods[2],ages[1]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n3a1),freq=FALSE,main=paste(neighborhoods[3],ages[1]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n4a1),freq=FALSE,main=paste(neighborhoods[4],ages[1]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n1a2),freq=FALSE,main=paste(neighborhoods[1],ages[2]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n2a2),freq=FALSE,main=paste(neighborhoods[2],ages[2]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n3a2),freq=FALSE,main=paste(neighborhoods[3],ages[2]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n4a2),freq=FALSE,main=paste(neighborhoods[4],ages[2]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n1a3),freq=FALSE,main=paste(neighborhoods[1],ages[3]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n2a3),freq=FALSE,main=paste(neighborhoods[2],ages[3]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n3a3),freq=FALSE,main=paste(neighborhoods[3],ages[3]),xlim=c(0,16),breaks=seq(0,16,1))
hist(log10(hand.n4a3),freq=FALSE,main=paste(neighborhoods[4],ages[3]),xlim=c(0,16),breaks=seq(0,16,1))

dev.off()

#raw data
load("hand_simul.rda")
hand.n1a1 <- hand[[1]]
hand.n2a1 <- hand[[2]]
hand.n3a1 <- hand[[3]]
hand.n4a1 <- hand[[4]]
hand.n1a2 <- hand[[5]]
hand.n2a2 <- hand[[6]]
hand.n3a2 <- hand[[7]]
hand.n4a2 <- hand[[8]]
hand.n1a3 <- hand[[9]]
hand.n2a3 <- hand[[10]]
hand.n3a3 <- hand[[11]]
hand.n4a3 <- hand[[12]]


neighborhoods <- c("Alajo","Bukom","Old Fadama","Shiabu");
ages <- c("0-1","1-2","2-5")
pdf("~/stat/sanipath/exposure/exposure/v14/v14.hand.simul01.pdf")
par(mar=c(4,4,2,1))
par(mfcol=c(5,2))
hist(sanidata$ec.lnconc[which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5")],yaxt="n",
     main=paste("Total, N=",length(which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5")),sep=""),
     breaks=seq(0,20,by=1),freq=FALSE,xlim=c(0,20),ylim=c(0,0.5),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(sanidata$ec.lnconc[which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="alajo")],yaxt="n",
     main=paste(neighborhoods[1],", N=",length(which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="alajo")),sep=""),
     breaks=seq(0,20,by=1),freq=FALSE,xlim=c(0,20),ylim=c(0,0.5),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(sanidata$ec.lnconc[which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="bukom")],yaxt="n",
     main=paste(neighborhoods[2],", N=",length(which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="bukom")),sep=""),
     breaks=seq(0,20,by=1),freq=FALSE,xlim=c(0,20),ylim=c(0,0.5),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(sanidata$ec.lnconc[which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="old fadama")],yaxt="n",
     main=paste(neighborhoods[3],", N=",length(which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="old fadama")),sep=""),
     breaks=seq(0,20,by=1),freq=FALSE,xlim=c(0,20),ylim=c(0,0.5),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(sanidata$ec.lnconc[which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="shiabu")],yaxt="n",
     main=paste(neighborhoods[4],", N=",length(which(sanidata$samtype=="handrinse" & sanidata$hr.who=="child under 5" & sanidata$neighbor=="shiabu")),sep=""),
     breaks=seq(0,20,by=1),freq=FALSE,xlim=c(0,20),ylim=c(0,0.5),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(log10(c(hand.n1a1,hand.n1a2,hand.n1a3,hand.n2a1,hand.n2a2,hand.n2a3,hand.n3a1,hand.n3a2,hand.n3a3,hand.n4a1,hand.n4a2,hand.n4a3)),yaxt="n",freq=FALSE,main=paste("Total, N=36000",sep=""),
     xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,1),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(log10(c(hand.n1a1,hand.n1a2,hand.n1a3)),yaxt="n",freq=FALSE,main=paste(neighborhoods[1],", N=9000",sep=""),xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,1),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(log10(c(hand.n2a1,hand.n2a2,hand.n2a3)),yaxt="n",freq=FALSE,main=paste(neighborhoods[2],", N=9000",sep=""),xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,1),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(log10(c(hand.n3a1,hand.n3a2,hand.n3a3)),yaxt="n",freq=FALSE,main=paste(neighborhoods[3],", N=9000",sep=""),xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,1),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
hist(log10(c(hand.n4a1,hand.n4a2,hand.n4a3)),yaxt="n",freq=FALSE,main=paste(neighborhoods[4],", N=9000",sep=""),xlim=c(0,20),ylim=c(0,0.5),breaks=seq(0,20,1),xlab=expression(paste("log10 ",italic("E.coli")," concentration (CFU/pair of hands)")))
axis(2,at=c(0,0.25,0.5),labels=c("0","25%","50%"),las=2)
dev.off()


