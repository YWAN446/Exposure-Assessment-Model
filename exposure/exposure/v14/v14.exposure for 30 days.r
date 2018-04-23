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

##############################################################################;
setwd("~/stat/sanipath/exposure/exposure/v14/")
library(devtools)
source_url('https://gist.github.com/menugget/7864454/raw/f698da873766347d837865eecfa726cdf52a6c40/plot.stream.4.R')
days=100

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
end<-8000+days
vec<-c(8001:end)
vec0<-rep(c(1:days),8)+rep(c(0,1000,2000,3000,4000,5000,6000,7000),each=days)
colors<-c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan")

pdf(paste("./output/","exposure","-100days",".pdf",sep=""),width=12);
#library(sfsmisc)
for (i in 1:8){
  if (i==1){
    par(fig=c(0,1,0,0.8), new=FALSE)
    select<-1:days
    y<-log10(expo[vec0[select],1])
    y[which(expo[vec0[select],1]==0)]<-0
    plot(expo[vec0[select],2],y,col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan")[i],
         #main=paste(neighbourhoods[k.neighb],"Age Group",ages[k.age]),
         #yaxt="n",xlab="Day",ylab="dose",ylim=c(0,20),pch=19,type="l",log = "x")
         yaxt="n",xlab="Day",ylab="Exposure (CFU)",ylim=c(0,20),type="l")
    aty <- axTicks(2)
    labels <- sapply(aty,function(i)
      as.expression(bquote(10^ .(i)))
    )
    labels[1]<-expression(0)
    axis(2,at=aty,labels=labels)
    #eaxis(2, at.small=FALSE, cex.axis=0.8)
  } else {
    select<-1:days+(i-1)*days
    y<-log10(expo[vec0[select],1])
    y[which(expo[vec0[select],1]==0)]<-0
    lines(expo[vec0[select],2],y,col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan")[i])
  }
}

legend("topright",      # location and inset
       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
       title="Pathway",
       c("soil","floor","offgr","drain",
         "DF","food","tap water","sachet"), pch=19,
       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))

mat0<-matrix(expo[vec0,1],ncol=8)
find.max<-function(a){
  return(which(a==max(a)))
}
max0<-apply(mat0,1,find.max)

par(fig=c(0,1,0.55,1), new=TRUE)
barplot(log10(expo[vec,1]),beside=TRUE,ylim=c(0,20),yaxt="n",col=colors[max0],ylab="Total Exposure (CFU)")
aty <- axTicks(2)
labels <- sapply(aty,function(i)
  as.expression(bquote(10^ .(i)))
)
labels[1]<-expression(0)
axis(2,at=aty,labels=labels)
dev.off()
