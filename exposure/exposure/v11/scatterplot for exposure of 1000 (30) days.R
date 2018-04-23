library(lattice);
library(sfsmisc);
setwd("~/stat/sanipath/exposure/exposure/v9/")
load("HH0-1.rda")
load("HH1-2.rda")
load("HH2-5.rda")
load("Net.HH0-1.rda")
load("Net.HH1-2.rda")
load("Net.HH2-5.rda")

neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");
ages<-c("0-1","1-2","2-5");
labels <- c("dirt","floor","offgr","drain",
            "septage","food","tap water","sachet water","total");

frac0 <- function(mc) return(1-length(mc[mc>0])/length(mc));
non0 <- function(mc){
  tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
}

boxquan <- function(x){
  stats <- c(as.numeric(quantile(x,probs=c(0.05,0.25),na.rm=TRUE)),
             # log10(mean(10^x,na.rm=TRUE)),
             quantile(x,0.5,na.rm=TRUE),
             as.numeric(quantile(x,probs=c(0.75,0.95),na.rm=TRUE)));
  n  <- length(which(!is.na(x)));
  # conf <- c( 1.58*(stats[4]-stats[2])/sqrt(n),
  #           -1.58*(stats[4]-stats[2])/sqrt(n)) +
  #         as.numeric(quantile(x,probs=0.5,na.rm=TRUE));
  conf <- c(stats[5],stats[1]);
  out <- x[x < stats[1] | x > stats[5]]; # outliers; not used
  return(list("stats"=stats,"n"=n,"conf"=conf,"out"=out));
}

mergequan <- function(xmat,names){
  stats <- c(); n <- c(); conf <- c(); out <- c(); group <- c();
  for(k in 1:ncol(xmat)){
    tmp   <- boxquan(xmat[,k]);
    stats <- cbind(stats,tmp$stats);
    n     <- c(n,tmp$n);
    conf  <- cbind(conf,tmp$conf);
    out   <- c(out,tmp$out);
    group <- c(group,rep(k,length(tmp$out)));
  }
  return(list("stats"=stats,"n"=n,"conf"=conf,
              "out"=out,"group"=group,"names"=names));
}


n.neighb <- 4;
num.mc <- 1000;
setwd("~/stat/sanipath/exposure/exposure/v11/")
##############################################################################;
pdf(paste("./output/","exposure","-path",".pdf",sep=""),width=12);
for (k.age in 1:3){
  if (k.age==1){
    int.dirt<-HH.1[[14]]
    int.flo<-HH.1[[16]]
    int.offgr<-HH.1[[15]]
    int.drain<-HH.1[[13]]
    int.septage<-HH.1[[17]]
    int.produce<-HH.1[[18]]
    dw<-HH.1[[19]]
    int.total<-HH.1[[20]]
  }
  if (k.age==2){
    int.dirt<-HH.2[[14]]
    int.flo<-HH.2[[16]]
    int.offgr<-HH.2[[15]]
    int.drain<-HH.2[[13]]
    int.septage<-HH.2[[17]]
    int.produce<-HH.2[[18]]
    dw<-HH.2[[19]]
    int.total<-HH.2[[20]]
  }
  if (k.age==3){
    int.dirt<-HH.3[[14]]
    int.flo<-HH.3[[16]]
    int.offgr<-HH.3[[15]]
    int.drain<-HH.3[[13]]
    int.septage<-HH.3[[17]]
    int.produce<-HH.3[[18]]
    dw<-HH.3[[19]]
    int.total<-HH.3[[20]]
  }
  par(mfrow=c(1,1));
  for(k.neighb in 1:n.neighb){
  expo<-matrix(0,nrow=8000,ncol=3)
  expo[1:1000,1]<-as.numeric(int.dirt[k.neighb,])
  expo[1001:2000,1]<-as.numeric(int.flo[k.neighb,])
  expo[2001:3000,1]<-as.numeric(int.offgr[k.neighb,])
  expo[3001:4000,1]<-as.numeric(int.drain[k.neighb,])
  expo[4001:5000,1]<-as.numeric(int.septage[k.neighb,])
  expo[5001:6000,1]<-as.numeric(int.produce[k.neighb,])
  expo[6001:7000,1]<-as.numeric(dw[k.neighb,1,])
  expo[7001:8000,1]<-as.numeric(dw[k.neighb,2,])
  expo[1:8000,2]<-rep(1:1000,8)
  #"dirt","flo","offgr","drain","septage","produce","tap","sachet"
  expo[1:8000,3]<-rep(1:8, each=1000)
  
  
  plot(expo[,2],log10(expo[,1]),col=rep(c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"),each=1000),
       main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),cex=0.5,xlab="Day",ylab="log10(dose)",ylim=c(0,20),pch=19)
  legend("topright",      # location and inset
         bty="n", cex=1,              # suppress legend box, shrink text 50%
         title="Pathway",
         c("soil","floor","offgr","drain",
           "DF","food","tap water","sachet"), pch=19,
         col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))
  #table(expo[which(expo[,1]>100000),3])
  }
}
dev.off();

##############################################################################;

plot.stacked <- function(x,y, ylab="", xlab="", main="", ncol=1, xlim=range(x, na.rm=T), ylim=c(0, 1.2*max(rowSums(y), na.rm=T)), border = NULL, col=rainbow(length(y[1,]))){
  
  plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, xaxs="i", yaxs="i", xlim=xlim, t="n", main=main)
  bottom=0*y[,1]
  for(i in 1:length(y[1,])){
    top=rowSums(as.matrix(y[,1:i]))
    polygon(c(x, rev(x)), c(top, rev(bottom)), border=border, col=col[i])
    bottom=top
  }
  abline(h=seq(0,200000, 10000), lty=3, col="grey")
  legend("topleft", rev(colnames(y)), ncol=ncol, inset = 0, fill=rev(col), bty="n", cex=0.8, col=col)
  box()
}

pdf(paste("./output/","exposure","-path-stream",".pdf",sep=""),width=12);
for (k.age in 1:3){
  if (k.age==1){
    int.dirt<-HH.1[[14]]
    int.flo<-HH.1[[16]]
    int.offgr<-HH.1[[15]]
    int.drain<-HH.1[[13]]
    int.septage<-HH.1[[17]]
    int.produce<-HH.1[[18]]
    dw<-HH.1[[19]]
    int.total<-HH.1[[20]]
  }
  if (k.age==2){
    int.dirt<-HH.2[[14]]
    int.flo<-HH.2[[16]]
    int.offgr<-HH.2[[15]]
    int.drain<-HH.2[[13]]
    int.septage<-HH.2[[17]]
    int.produce<-HH.2[[18]]
    dw<-HH.2[[19]]
    int.total<-HH.2[[20]]
  }
  if (k.age==3){
    int.dirt<-HH.3[[14]]
    int.flo<-HH.3[[16]]
    int.offgr<-HH.3[[15]]
    int.drain<-HH.3[[13]]
    int.septage<-HH.3[[17]]
    int.produce<-HH.3[[18]]
    dw<-HH.3[[19]]
    int.total<-HH.3[[20]]
  }
for(k.neighb in 1:n.neighb){
  expo.path<-matrix(0,nrow=1000,ncol=8)
  expo.path[1:1000,1]<-as.numeric(int.dirt[k.neighb,])
  expo.path[1:1000,2]<-as.numeric(int.flo[k.neighb,])
  expo.path[1:1000,3]<-as.numeric(int.offgr[k.neighb,])
  expo.path[1:1000,4]<-as.numeric(int.drain[k.neighb,])
  expo.path[1:1000,5]<-as.numeric(int.septage[k.neighb,])
  expo.path[1:1000,6]<-as.numeric(int.produce[k.neighb,])
  expo.path[1:1000,7]<-as.numeric(dw[k.neighb,1,])
  expo.path[1:1000,8]<-as.numeric(dw[k.neighb,2,])
  expo.path[which(expo.path==0)]=1
  colnames(expo.path)<-c("soil","floor","offgr","drain",
                         "DF","food","tap water","sachet")
par(mar=c(0,0,2,0), bty="n")
#plot.stream(1:1000,log10(expo.path), axes=FALSE, center=TRUE,xlim=c(0, 100), xaxs="i", spar=0.2, frac.rand=0.1, col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"), border=1, lwd=0.1)
plot.stacked(1:1000,log10(expo.path),col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"),
             main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),xlab="Day",ylab="log10(dose)")
}}
dev.off();

library(devtools)
source_url('https://gist.github.com/menugget/7864454/raw/f698da873766347d837865eecfa726cdf52a6c40/plot.stream.4.R')








for (k.age in 1:3){
  if (k.age==1){
    int.dirt<-HH.1[[14]]
    int.flo<-HH.1[[16]]
    int.offgr<-HH.1[[15]]
    int.drain<-HH.1[[13]]
    int.septage<-HH.1[[17]]
    int.produce<-HH.1[[18]]
    dw<-HH.1[[19]]
    int.total<-HH.1[[20]]
  }
  if (k.age==2){
    int.dirt<-HH.2[[14]]
    int.flo<-HH.2[[16]]
    int.offgr<-HH.2[[15]]
    int.drain<-HH.2[[13]]
    int.septage<-HH.2[[17]]
    int.produce<-HH.2[[18]]
    dw<-HH.2[[19]]
    int.total<-HH.2[[20]]
  }
  if (k.age==3){
    int.dirt<-HH.3[[14]]
    int.flo<-HH.3[[16]]
    int.offgr<-HH.3[[15]]
    int.drain<-HH.3[[13]]
    int.septage<-HH.3[[17]]
    int.produce<-HH.3[[18]]
    dw<-HH.3[[19]]
    int.total<-HH.3[[20]]
  }
  par(mfrow=c(1,1));
  for(k.neighb in 1:n.neighb){
    expo<-matrix(0,nrow=9000,ncol=3)
    expo[1:1000,1]<-as.numeric(int.dirt[k.neighb,])
    expo[1001:2000,1]<-as.numeric(int.flo[k.neighb,])
    expo[2001:3000,1]<-as.numeric(int.offgr[k.neighb,])
    expo[3001:4000,1]<-as.numeric(int.drain[k.neighb,])
    expo[4001:5000,1]<-as.numeric(int.septage[k.neighb,])
    expo[5001:6000,1]<-as.numeric(int.produce[k.neighb,])
    expo[6001:7000,1]<-as.numeric(dw[k.neighb,1,])
    expo[7001:8000,1]<-as.numeric(dw[k.neighb,2,])
    expo[8001:9000,1]<-expo[1:1000,1]+expo[1001:2000,1]+expo[2001:3000,1]+expo[3001:4000,1]+expo[4001:5000,1]+expo[5001:6000,1]+expo[6001:7000,1]+expo[7001:8000,1]
    expo[1:9000,2]<-rep(1:1000,9)
    #"dirt","flo","offgr","drain","septage","produce","tap","sachet","total"
    expo[1:9000,3]<-rep(1:9, each=1000)
    
    days=30
    vec<-rep(c(1:days),8)+rep(c(0,1000,2000,3000,4000,5000,6000,7000),each=days)
    plot(expo[vec,2],log10(expo[vec,1]),col=rep(c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"),each=days),
         main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),cex=0.8,xlab="Day",ylab="log10(dose)",ylim=c(0,20),pch=19)
    legend("topright",      # location and inset
           bty="n", cex=1,              # suppress legend box, shrink text 50%
           title="Pathway",
           c("soil","floor","offgr","drain",
             "DF","food","tap water","sachet"), pch=19,
           col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))
    #table(expo[which(expo[,1]>100000),3])
    
    end<-8000+days
    vec<-c(8001:end)
    alpha=1.55e-1
    N50=2.11e6
    y=1-(1+(expo[vec,1]*(2^(1/alpha)-1)/N50))^(-alpha)
    plot(expo[vec,2],log10(expo[vec,1]),col="black",
         main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),cex=y*3,xlab="Day",ylab="log10(dose)",ylim=c(0,20),pch=19)
    legend("topright",      # location and inset
           bty="n", cex=1,              # suppress legend box, shrink text 50%
           c("total exposure"), pch=19,
           col="black")
  }
}

logd=c(0:2000)/100
par(fig=c(0,0.8,0,0.8), new=FALSE)
end<-8000+days
vec<-c(8001:end)
alpha=1.55e-1
N50=2.11e6
stdy=1-(1+((10^logd)*(2^(1/alpha)-1)/N50))^(-alpha)
y=1-(1+(expo[vec,1]*(2^(1/alpha)-1)/N50))^(-alpha)
plot(expo[vec,2],log10(expo[vec,1]),col="black",
     #main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),
     cex=y*3,xlab="Day",ylab="log10(dose)",ylim=c(0,20),pch=19)
#legend("topright",      # location and inset
#       bty="n", cex=1,              # suppress legend box, shrink text 50%
#       c("total exposure"), pch=19,
#       col="black")
par(fig=c(0,0.8,0.55,1), new=TRUE)
barplot(log10(expo[vec,1]),beside=TRUE,ylim=c(0,20))
par(fig=c(0.675,1,0,0.8),new=TRUE)
plot(stdy,logd,xlab="Probability of illness",yaxt="n",ylab="",type="l")
mtext(paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]), side=3, outer=TRUE, line=-3)