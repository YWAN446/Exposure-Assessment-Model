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
pdf(paste("./output/","exposure","-hh",".pdf",sep=""));
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
  
  par(mfrow=c(1,4));
  for(k.neighb in 1:n.neighb){
    barplot(c(1-frac0(int.dirt[k.neighb,]),
              1-frac0(int.flo[k.neighb,]),
              1-frac0(int.offgr[k.neighb,]),
              1-frac0(int.drain[k.neighb,]),
              1-frac0(int.septage[k.neighb,]),
              1-frac0(int.produce[k.neighb,]),
              1-frac0(dw[k.neighb,1,]),
              1-frac0(dw[k.neighb,2,])),
            ylab="fraction exposed",las=2,
            main=paste(neighbourhoods[k.neighb],ifelse(k.age==1,"0-1",ifelse(k.age==2,"1-2","2-5"))),
            names=c("soil","floor","offgr","drain","DF","food","tap water","sachet"),ylim=c(0,1),cex.names = 0.8);
    print(c(1-frac0(int.dirt[k.neighb,]),
              1-frac0(int.flo[k.neighb,]),
              1-frac0(int.offgr[k.neighb,]),
              1-frac0(int.drain[k.neighb,]),
              1-frac0(int.septage[k.neighb,]),
              1-frac0(int.produce[k.neighb,]),
              1-frac0(dw[k.neighb,1,]),
              1-frac0(dw[k.neighb,2,])))
  }
  
  par(mfrow=c(1,4));
  for(k.neighb in 1:n.neighb){
    z.exp <- mergequan(cbind(log10(non0(int.dirt[k.neighb,])), # dirt floor -> hand-mouth
                             log10(non0(int.flo[k.neighb,])), # concrete floor -> hand-mouth
                             log10(non0(int.offgr[k.neighb,])), # off ground -> hand-mouth
                             log10(non0(int.drain[k.neighb,])), # drain -> hand-mouth
                             log10(non0(int.septage[k.neighb,])), # septage -> hand-mouth     
                             log10(non0(int.produce[k.neighb,])), # eat -> ingest
                             log10(non0(dw[k.neighb,1,])),   # drink tap water
                             log10(non0(dw[k.neighb,2,])),  # drink sachet water
                             log10(non0(int.total[k.neighb,]))), labels);
    bxp(z.exp,outline=FALSE,ylim=c(0,20),las=2,
        ylab="log10(dose)",main=paste(neighbourhoods[k.neighb],ifelse(k.age==1,"0-1",ifelse(k.age==2,"1-2","2-5"))),cex.axis = 0.8);
    points(1:9,cbind(log10(mean(non0(int.dirt[k.neighb,]),na.rm=TRUE)), # dirt floor -> hand-mouth
                     log10(mean(non0(int.flo[k.neighb,]),na.rm=TRUE)), # concrete floor -> hand-mouth
                     log10(mean(non0(int.offgr[k.neighb,]),na.rm=TRUE)), # off ground -> hand-mouth
                     log10(mean(non0(int.drain[k.neighb,]),na.rm=TRUE)), # drain -> hand-mouth
                     log10(mean(non0(int.septage[k.neighb,]),na.rm=TRUE)), # septage -> hand-mouth     
                     log10(mean(non0(int.produce[k.neighb,]),na.rm=TRUE)), # eat -> ingest
                     log10(mean(non0(dw[k.neighb,1,]),na.rm=TRUE)),   # drink tap water
                     log10(mean(non0(dw[k.neighb,2,]),na.rm=TRUE)),  # drink sachet water
                     log10(mean(non0(int.total[k.neighb,]),na.rm=TRUE))))
    print(cbind(mean(non0(int.dirt[k.neighb,]),na.rm=TRUE), # dirt floor -> hand-mouth
                mean(non0(int.flo[k.neighb,]),na.rm=TRUE), # concrete floor -> hand-mouth
                mean(non0(int.offgr[k.neighb,]),na.rm=TRUE), # off ground -> hand-mouth
                mean(non0(int.drain[k.neighb,]),na.rm=TRUE), # drain -> hand-mouth
                mean(non0(int.septage[k.neighb,]),na.rm=TRUE), # septage -> hand-mouth     
                mean(non0(int.produce[k.neighb,]),na.rm=TRUE), # eat -> ingest
                mean(non0(dw[k.neighb,1,]),na.rm=TRUE),   # drink tap water
                mean(non0(dw[k.neighb,2,]),na.rm=TRUE),  # drink sachet water
                mean(non0(int.total[k.neighb,]),na.rm=TRUE)))
    print(cbind(mean(non0(int.dirt[k.neighb,]),na.rm=TRUE)*(1-frac0(int.dirt[k.neighb,])), # dirt floor -> hand-mouth
                mean(non0(int.flo[k.neighb,]),na.rm=TRUE)*(1-frac0(int.flo[k.neighb,])), # concrete floor -> hand-mouth
                mean(non0(int.offgr[k.neighb,]),na.rm=TRUE)*(1-frac0(int.offgr[k.neighb,])), # off ground -> hand-mouth
                mean(non0(int.drain[k.neighb,]),na.rm=TRUE)*(1-frac0(int.drain[k.neighb,])), # drain -> hand-mouth
                mean(non0(int.septage[k.neighb,]),na.rm=TRUE)*(1-frac0(int.septage[k.neighb,])), # septage -> hand-mouth     
                mean(non0(int.produce[k.neighb,]),na.rm=TRUE)*(1-frac0(int.produce[k.neighb,])), # eat -> ingest
                mean(non0(dw[k.neighb,1,]),na.rm=TRUE)*(1-frac0(dw[k.neighb,1,])),   # drink tap water
                mean(non0(dw[k.neighb,2,]),na.rm=TRUE)*(1-frac0(dw[k.neighb,2,])),  # drink sachet water
                mean(non0(int.total[k.neighb,]),na.rm=TRUE)*(1-frac0(int.total[k.neighb,]))))
  }
}
dev.off();

##############################################################################;

pdf(paste("./output/","exposure","-hh-by-age",".pdf",sep=""));
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

####################################################################;
layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(1,3))
for(k.neighb in 1:n.neighb){
  par(mar=c(0.5, 4, 2, 0.5))
  barplot(c(1-frac0(int.dirt1[k.neighb,]),1-frac0(int.dirt2[k.neighb,]),1-frac0(int.dirt3[k.neighb,]),
            1-frac0(int.flo1[k.neighb,]),1-frac0(int.flo2[k.neighb,]),1-frac0(int.flo3[k.neighb,]),
            1-frac0(int.offgr1[k.neighb,]),1-frac0(int.offgr2[k.neighb,]),1-frac0(int.offgr3[k.neighb,]),
            1-frac0(int.drain1[k.neighb,]),1-frac0(int.drain2[k.neighb,]),1-frac0(int.drain3[k.neighb,]),
            1-frac0(int.septage1[k.neighb,]),1-frac0(int.septage2[k.neighb,]),1-frac0(int.septage3[k.neighb,]),
            1-frac0(int.produce1[k.neighb,]),1-frac0(int.produce2[k.neighb,]),1-frac0(int.produce3[k.neighb,]),
            1-frac0(dw1[k.neighb,1,]),1-frac0(dw2[k.neighb,1,]),1-frac0(dw3[k.neighb,1,]),
            1-frac0(dw1[k.neighb,2,]),1-frac0(dw2[k.neighb,2,]),1-frac0(dw3[k.neighb,2,]),
            1-frac0(int.total1[k.neighb,]),1-frac0(int.total2[k.neighb,]),1-frac0(int.total3[k.neighb,])),
          ylab="",
          main=paste(neighbourhoods[k.neighb]),las = 2,
          #            names=c("","dirt","","","floor","","","offgr","","","drain","","","septage","","","food","","","tap water","","","sachet water",""),
          col=c("antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4"),
          ylim=c(0,1),cex.names = 0.8);
  mtext("fraction exposed", side=2, line=3, cex=0.9)
  grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
  #legend(-0.7,1,bty="n", cex=0.7, title="Age Group",c("0-1","1-2","2-5"),fill=c("antiquewhite2","antiquewhite3","antiquewhite4"))
  
  
  par(mar=c(4, 4, 0.5, 0.5))
  #####################################################################;
  z.exp <- mergequan(cbind(log10(non0(int.dirt1[k.neighb,])),log10(non0(int.dirt2[k.neighb,])),log10(non0(int.dirt3[k.neighb,])), # dirt floor -> hand-mouth
                           log10(non0(int.flo1[k.neighb,])),log10(non0(int.flo2[k.neighb,])),log10(non0(int.flo3[k.neighb,])), # concrete floor -> hand-mouth
                           log10(non0(int.offgr1[k.neighb,])),log10(non0(int.offgr2[k.neighb,])),log10(non0(int.offgr3[k.neighb,])), # off ground -> hand-mouth
                           log10(non0(int.drain1[k.neighb,])),log10(non0(int.drain2[k.neighb,])),log10(non0(int.drain3[k.neighb,])), # drain -> hand-mouth
                           log10(non0(int.septage1[k.neighb,])),log10(non0(int.septage2[k.neighb,])),log10(non0(int.septage3[k.neighb,])), # septage -> hand-mouth     
                           log10(non0(int.produce1[k.neighb,])),log10(non0(int.produce2[k.neighb,])),log10(non0(int.produce3[k.neighb,])), # eat -> ingest
                           log10(non0(dw1[k.neighb,1,])),log10(non0(dw2[k.neighb,1,])),log10(non0(dw3[k.neighb,1,])),   # drink tap water
                           log10(non0(dw1[k.neighb,2,])),log10(non0(dw2[k.neighb,2,])),log10(non0(dw3[k.neighb,2,])),  # drink sachet water
                           log10(non0(int.total1[k.neighb,])),log10(non0(int.total2[k.neighb,])),log10(non0(int.total3[k.neighb,]))), c());
  #c("","dirt","","","floor","","","offgr","","","drain","","","septage","","","food","","","tap water","","","sachet water","","","Total",""));
  bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,xaxt="n",
      boxfill=c("antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4"),
      ylab="log10(dose)",#main=paste(neighbourhoods[k.neighb]),
      cex.axis = 0.8);
  points(1:27,cbind(log10(mean(non0(int.dirt1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[k.neighb,]),na.rm=TRUE)), # dirt floor -> hand-mouth
                    log10(mean(non0(int.flo1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.flo2[k.neighb,]),na.rm=TRUE)), log10(mean(non0(int.flo3[k.neighb,]),na.rm=TRUE)),# concrete floor -> hand-mouth
                    log10(mean(non0(int.offgr1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[k.neighb,]),na.rm=TRUE)), # off ground -> hand-mouth
                    log10(mean(non0(int.drain1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.drain2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.drain3[k.neighb,]),na.rm=TRUE)), # drain -> hand-mouth
                    log10(mean(non0(int.septage1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.septage2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.septage3[k.neighb,]),na.rm=TRUE)), # septage -> hand-mouth     
                    log10(mean(non0(int.produce1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.produce2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.produce3[k.neighb,]),na.rm=TRUE)), # eat -> ingest
                    log10(mean(non0(dw1[k.neighb,1,]),na.rm=TRUE)),log10(mean(non0(dw2[k.neighb,1,]),na.rm=TRUE)),log10(mean(non0(dw3[k.neighb,1,]),na.rm=TRUE)),   # drink tap water
                    log10(mean(non0(dw1[k.neighb,2,]),na.rm=TRUE)),log10(mean(non0(dw2[k.neighb,2,]),na.rm=TRUE)),log10(mean(non0(dw3[k.neighb,2,]),na.rm=TRUE)),  # drink sachet water
                    log10(mean(non0(int.total1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.total2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.total3[k.neighb,]),na.rm=TRUE))),
         col="black",cex=1
  )
  points(1:27,cbind(log10(mean(non0(int.dirt1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[k.neighb,]),na.rm=TRUE)), # dirt floor -> hand-mouth
                    log10(mean(non0(int.flo1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.flo2[k.neighb,]),na.rm=TRUE)), log10(mean(non0(int.flo3[k.neighb,]),na.rm=TRUE)),# concrete floor -> hand-mouth
                    log10(mean(non0(int.offgr1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[k.neighb,]),na.rm=TRUE)), # off ground -> hand-mouth
                    log10(mean(non0(int.drain1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.drain2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.drain3[k.neighb,]),na.rm=TRUE)), # drain -> hand-mouth
                    log10(mean(non0(int.septage1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.septage2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.septage3[k.neighb,]),na.rm=TRUE)), # septage -> hand-mouth     
                    log10(mean(non0(int.produce1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.produce2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.produce3[k.neighb,]),na.rm=TRUE)), # eat -> ingest
                    log10(mean(non0(dw1[k.neighb,1,]),na.rm=TRUE)),log10(mean(non0(dw2[k.neighb,1,]),na.rm=TRUE)),log10(mean(non0(dw3[k.neighb,1,]),na.rm=TRUE)),   # drink tap water
                    log10(mean(non0(dw1[k.neighb,2,]),na.rm=TRUE)),log10(mean(non0(dw2[k.neighb,2,]),na.rm=TRUE)),log10(mean(non0(dw3[k.neighb,2,]),na.rm=TRUE)),  # drink sachet water
                    log10(mean(non0(int.total1[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.total2[k.neighb,]),na.rm=TRUE)),log10(mean(non0(int.total3[k.neighb,]),na.rm=TRUE))),
         col="black",cex=1.5
  )
  grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
  legend(0,20,bty="n", cex=0.8, title="Age Group",c("0-1","1-2","2-5"),fill=c("antiquewhite2","antiquewhite3","antiquewhite4"))
  axis(1, at=c(2,5,8,11,14,17,20,23,26), labels=c("soil","floor","offgr","drain",
                                                  "DF","food","tap water","sachet","total"), las=1, cex.axis=0.8)
}

dev.off();
####################################################################################;

pdf(paste("./output/","exposure","-hh-by-neighb",".pdf",sep=""));
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

####################################################################;
par(mfrow=c(1,1));
layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(1,3))
for(k.age in 1:3){
  if (k.age==1){
    par(mar=c(0.5, 4, 2, 0.5))
    barplot(c(1-frac0(int.dirt1[1,]),1-frac0(int.dirt1[2,]),1-frac0(int.dirt1[3,]),1-frac0(int.dirt1[4,]),
              1-frac0(int.flo1[1,]),1-frac0(int.flo1[2,]),1-frac0(int.flo1[3,]),1-frac0(int.flo1[4,]),
              1-frac0(int.offgr1[1,]),1-frac0(int.offgr1[2,]),1-frac0(int.offgr1[3,]),1-frac0(int.offgr1[4,]),
              1-frac0(int.drain1[1,]),1-frac0(int.drain1[2,]),1-frac0(int.drain1[3,]),1-frac0(int.drain1[4,]),
              1-frac0(int.septage1[1,]),1-frac0(int.septage1[2,]),1-frac0(int.septage1[3,]),1-frac0(int.septage1[4,]),
              1-frac0(int.produce1[1,]),1-frac0(int.produce1[2,]),1-frac0(int.produce1[3,]),1-frac0(int.produce1[4,]),
              1-frac0(dw1[1,1,]),1-frac0(dw1[2,1,]),1-frac0(dw1[3,1,]),1-frac0(dw1[4,1,]),
              1-frac0(dw1[1,2,]),1-frac0(dw1[2,2,]),1-frac0(dw1[3,2,]),1-frac0(dw1[4,2,]),
              1-frac0(int.total1[1,]),1-frac0(int.total1[2,]),1-frac0(int.total1[3,]),1-frac0(int.total1[4,])),
            ylab="",xaxt="n",
            main=paste("Age Group",ages[k.age]),las = 2,
            #names=c("","dirt","","","","floor","","","","offgr","","","","drain","","","","septage","","","","food","","","","tap water","","","","sachet water","",""),
            col=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
            ylim=c(0,1),cex.names = 0.8);
    mtext("fraction exposed", side=2, line=3, cex=0.9)
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    #legend(0,1,bty="n", cex=0.5, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    par(mar=c(4, 4, 0.5, 0.5))
    z.exp <- mergequan(cbind(log10(non0(int.dirt1[1,])),log10(non0(int.dirt1[2,])),log10(non0(int.dirt1[3,])),log10(non0(int.dirt1[4,])),
                             log10(non0(int.flo1[1,])),log10(non0(int.flo1[2,])),log10(non0(int.flo1[3,])),log10(non0(int.flo1[4,])),
                             log10(non0(int.offgr1[1,])),log10(non0(int.offgr1[2,])),log10(non0(int.offgr1[3,])),log10(non0(int.offgr1[4,])),
                             log10(non0(int.drain1[1,])),log10(non0(int.drain1[2,])),log10(non0(int.drain1[3,])),log10(non0(int.drain1[4,])),
                             log10(non0(int.septage1[1,])),log10(non0(int.septage1[2,])),log10(non0(int.septage1[3,])),log10(non0(int.septage1[4,])),
                             log10(non0(int.produce1[1,])),log10(non0(int.produce1[2,])),log10(non0(int.produce1[3,])),log10(non0(int.produce1[4,])),
                             log10(non0(dw1[1,1,])),log10(non0(dw1[2,1,])),log10(non0(dw1[3,1,])),log10(non0(dw1[4,1,])),   # drink tap water
                             log10(non0(dw1[1,2,])),log10(non0(dw1[2,2,])),log10(non0(dw1[3,2,])),log10(non0(dw1[4,2,])),  # drink sachet water
                             log10(non0(int.total1[1,])),log10(non0(int.total1[2,])),log10(non0(int.total1[3,])),log10(non0(int.total1[4,]))), 
                       c("","soil","","","","floor","","","","offgr","","","","drain","","","","DF","","","","food","","","","tap water","","","","sachet","","","","Total","",""));
    bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,
        boxfill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
        ylab="log10(dose)",#main=paste("Age Group",ages[k.age]),
        cex.axis = 0.8,xaxt="n");
    points(1:36,cbind(log10(mean(non0(int.dirt1[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo1[1,]),na.rm=TRUE)),log10(mean(non0(int.flo1[2,]),na.rm=TRUE)),log10(mean(non0(int.flo1[3,]),na.rm=TRUE)),log10(mean(non0(int.flo1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr1[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain1[1,]),na.rm=TRUE)),log10(mean(non0(int.drain1[2,]),na.rm=TRUE)),log10(mean(non0(int.drain1[3,]),na.rm=TRUE)),log10(mean(non0(int.drain1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage1[1,]),na.rm=TRUE)),log10(mean(non0(int.septage1[2,]),na.rm=TRUE)),log10(mean(non0(int.septage1[3,]),na.rm=TRUE)),log10(mean(non0(int.septage1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce1[1,]),na.rm=TRUE)),log10(mean(non0(int.produce1[2,]),na.rm=TRUE)),log10(mean(non0(int.produce1[3,]),na.rm=TRUE)),log10(mean(non0(int.produce1[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw1[1,1,]),na.rm=TRUE)),log10(mean(non0(dw1[2,1,]),na.rm=TRUE)),log10(mean(non0(dw1[3,1,]),na.rm=TRUE)), log10(mean(non0(dw1[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw1[1,2,]),na.rm=TRUE)),log10(mean(non0(dw1[2,2,]),na.rm=TRUE)),log10(mean(non0(dw1[3,2,]),na.rm=TRUE)), log10(mean(non0(dw1[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total1[1,]),na.rm=TRUE)),log10(mean(non0(int.total1[2,]),na.rm=TRUE)),log10(mean(non0(int.total1[3,]),na.rm=TRUE)),log10(mean(non0(int.total1[4,]),na.rm=TRUE))),
           col="black",cex=1
    )
    points(1:36,cbind(log10(mean(non0(int.dirt1[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo1[1,]),na.rm=TRUE)),log10(mean(non0(int.flo1[2,]),na.rm=TRUE)),log10(mean(non0(int.flo1[3,]),na.rm=TRUE)),log10(mean(non0(int.flo1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr1[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain1[1,]),na.rm=TRUE)),log10(mean(non0(int.drain1[2,]),na.rm=TRUE)),log10(mean(non0(int.drain1[3,]),na.rm=TRUE)),log10(mean(non0(int.drain1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage1[1,]),na.rm=TRUE)),log10(mean(non0(int.septage1[2,]),na.rm=TRUE)),log10(mean(non0(int.septage1[3,]),na.rm=TRUE)),log10(mean(non0(int.septage1[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce1[1,]),na.rm=TRUE)),log10(mean(non0(int.produce1[2,]),na.rm=TRUE)),log10(mean(non0(int.produce1[3,]),na.rm=TRUE)),log10(mean(non0(int.produce1[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw1[1,1,]),na.rm=TRUE)),log10(mean(non0(dw1[2,1,]),na.rm=TRUE)),log10(mean(non0(dw1[3,1,]),na.rm=TRUE)), log10(mean(non0(dw1[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw1[1,2,]),na.rm=TRUE)),log10(mean(non0(dw1[2,2,]),na.rm=TRUE)),log10(mean(non0(dw1[3,2,]),na.rm=TRUE)), log10(mean(non0(dw1[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total1[1,]),na.rm=TRUE)),log10(mean(non0(int.total1[2,]),na.rm=TRUE)),log10(mean(non0(int.total1[3,]),na.rm=TRUE)),log10(mean(non0(int.total1[4,]),na.rm=TRUE))),
           col="black",cex=1.5
    )
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    legend(0,20,bty="n", cex=0.8, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    axis(1, at=c(2.5,6.5,10.5,14.5,18.5,22.5,26.5,30.5,34.5), labels=c("soil","floor","offgr","drain",
                                                                       "DF","food","tap water","sachet","total"), las=1, cex.axis=0.8)
    
  }  
  if (k.age==2){
    par(mar=c(0.5, 4, 2, 0.5))
    barplot(c(1-frac0(int.dirt2[1,]),1-frac0(int.dirt2[2,]),1-frac0(int.dirt2[3,]),1-frac0(int.dirt2[4,]),
              1-frac0(int.flo2[1,]),1-frac0(int.flo2[2,]),1-frac0(int.flo2[3,]),1-frac0(int.flo2[4,]),
              1-frac0(int.offgr2[1,]),1-frac0(int.offgr2[2,]),1-frac0(int.offgr2[3,]),1-frac0(int.offgr2[4,]),
              1-frac0(int.drain2[1,]),1-frac0(int.drain2[2,]),1-frac0(int.drain2[3,]),1-frac0(int.drain2[4,]),
              1-frac0(int.septage2[1,]),1-frac0(int.septage2[2,]),1-frac0(int.septage2[3,]),1-frac0(int.septage2[4,]),
              1-frac0(int.produce2[1,]),1-frac0(int.produce2[2,]),1-frac0(int.produce2[3,]),1-frac0(int.produce2[4,]),
              1-frac0(dw2[1,1,]),1-frac0(dw2[2,1,]),1-frac0(dw2[3,1,]),1-frac0(dw2[4,1,]),
              1-frac0(dw2[1,2,]),1-frac0(dw2[2,2,]),1-frac0(dw2[3,2,]),1-frac0(dw2[4,2,]),
              1-frac0(int.total2[1,]),1-frac0(int.total2[2,]),1-frac0(int.total2[3,]),1-frac0(int.total2[4,])),
            ylab="",xaxt="n",
            main=paste("Age Group",ages[k.age]),las = 2,
            #names=c("","dirt","","","","floor","","","","offgr","","","","drain","","","","septage","","","","food","","","","tap water","","","","sachet water","",""),
            col=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
            ylim=c(0,1),cex.names = 0.8);
    mtext("fraction exposed", side=2, line=3, cex=0.9)
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    #legend(0,1,bty="n", cex=0.5, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    par(mar=c(4, 4, 0.5, 0.5))
    z.exp <- mergequan(cbind(log10(non0(int.dirt2[1,])),log10(non0(int.dirt2[2,])),log10(non0(int.dirt2[3,])),log10(non0(int.dirt2[4,])),
                             log10(non0(int.flo2[1,])),log10(non0(int.flo2[2,])),log10(non0(int.flo2[3,])),log10(non0(int.flo2[4,])),
                             log10(non0(int.offgr2[1,])),log10(non0(int.offgr2[2,])),log10(non0(int.offgr2[3,])),log10(non0(int.offgr2[4,])),
                             log10(non0(int.drain2[1,])),log10(non0(int.drain2[2,])),log10(non0(int.drain2[3,])),log10(non0(int.drain2[4,])),
                             log10(non0(int.septage2[1,])),log10(non0(int.septage2[2,])),log10(non0(int.septage2[3,])),log10(non0(int.septage2[4,])),
                             log10(non0(int.produce2[1,])),log10(non0(int.produce2[2,])),log10(non0(int.produce2[3,])),log10(non0(int.produce2[4,])),
                             log10(non0(dw2[1,1,])),log10(non0(dw2[2,1,])),log10(non0(dw2[3,1,])),log10(non0(dw2[4,1,])),   # drink tap water
                             log10(non0(dw2[1,2,])),log10(non0(dw2[2,2,])),log10(non0(dw2[3,2,])),log10(non0(dw2[4,2,])),  # drink sachet water
                             log10(non0(int.total2[1,])),log10(non0(int.total2[2,])),log10(non0(int.total2[3,])),log10(non0(int.total2[4,]))), 
                       c("","soil","","","","floor","","","","offgr","","","","drain","","","","DF","","","","food","","","","tap water","","","","sachet","","","","Total","",""));
    bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,xaxt="n",
        boxfill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
        ylab="log10(dose)",#main=paste("Age Group",ages[k.age]),
        cex.axis = 0.8);
    points(1:36,cbind(log10(mean(non0(int.dirt2[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo2[1,]),na.rm=TRUE)),log10(mean(non0(int.flo2[2,]),na.rm=TRUE)),log10(mean(non0(int.flo2[3,]),na.rm=TRUE)),log10(mean(non0(int.flo2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr2[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain2[1,]),na.rm=TRUE)),log10(mean(non0(int.drain2[2,]),na.rm=TRUE)),log10(mean(non0(int.drain2[3,]),na.rm=TRUE)),log10(mean(non0(int.drain2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage2[1,]),na.rm=TRUE)),log10(mean(non0(int.septage2[2,]),na.rm=TRUE)),log10(mean(non0(int.septage2[3,]),na.rm=TRUE)),log10(mean(non0(int.septage2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce2[1,]),na.rm=TRUE)),log10(mean(non0(int.produce2[2,]),na.rm=TRUE)),log10(mean(non0(int.produce2[3,]),na.rm=TRUE)),log10(mean(non0(int.produce2[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw2[1,1,]),na.rm=TRUE)),log10(mean(non0(dw2[2,1,]),na.rm=TRUE)),log10(mean(non0(dw2[3,1,]),na.rm=TRUE)), log10(mean(non0(dw2[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw2[1,2,]),na.rm=TRUE)),log10(mean(non0(dw2[2,2,]),na.rm=TRUE)),log10(mean(non0(dw2[3,2,]),na.rm=TRUE)), log10(mean(non0(dw2[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total2[1,]),na.rm=TRUE)),log10(mean(non0(int.total2[2,]),na.rm=TRUE)),log10(mean(non0(int.total2[3,]),na.rm=TRUE)),log10(mean(non0(int.total2[4,]),na.rm=TRUE))),
           col="black",cex=1
    )
    points(1:36,cbind(log10(mean(non0(int.dirt2[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo2[1,]),na.rm=TRUE)),log10(mean(non0(int.flo2[2,]),na.rm=TRUE)),log10(mean(non0(int.flo2[3,]),na.rm=TRUE)),log10(mean(non0(int.flo2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr2[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain2[1,]),na.rm=TRUE)),log10(mean(non0(int.drain2[2,]),na.rm=TRUE)),log10(mean(non0(int.drain2[3,]),na.rm=TRUE)),log10(mean(non0(int.drain2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage2[1,]),na.rm=TRUE)),log10(mean(non0(int.septage2[2,]),na.rm=TRUE)),log10(mean(non0(int.septage2[3,]),na.rm=TRUE)),log10(mean(non0(int.septage2[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce2[1,]),na.rm=TRUE)),log10(mean(non0(int.produce2[2,]),na.rm=TRUE)),log10(mean(non0(int.produce2[3,]),na.rm=TRUE)),log10(mean(non0(int.produce2[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw2[1,1,]),na.rm=TRUE)),log10(mean(non0(dw2[2,1,]),na.rm=TRUE)),log10(mean(non0(dw2[3,1,]),na.rm=TRUE)), log10(mean(non0(dw2[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw2[1,2,]),na.rm=TRUE)),log10(mean(non0(dw2[2,2,]),na.rm=TRUE)),log10(mean(non0(dw2[3,2,]),na.rm=TRUE)), log10(mean(non0(dw2[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total2[1,]),na.rm=TRUE)),log10(mean(non0(int.total2[2,]),na.rm=TRUE)),log10(mean(non0(int.total2[3,]),na.rm=TRUE)),log10(mean(non0(int.total2[4,]),na.rm=TRUE))),
           col="black",cex=1.5
    )
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    legend(0,20,bty="n", cex=0.8, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    axis(1, at=c(2.5,6.5,10.5,14.5,18.5,22.5,26.5,30.5,34.5), labels=c("soil","floor","offgr","drain",
                                                                       "DF","food","tap water","sachet","total"), las=1, cex.axis=0.8)
  }
  if (k.age==3){
    par(mar=c(0.5, 4, 2, 0.5))
    barplot(c(1-frac0(int.dirt3[1,]),1-frac0(int.dirt3[2,]),1-frac0(int.dirt3[3,]),1-frac0(int.dirt3[4,]),
              1-frac0(int.flo3[1,]),1-frac0(int.flo3[2,]),1-frac0(int.flo3[3,]),1-frac0(int.flo3[4,]),
              1-frac0(int.offgr3[1,]),1-frac0(int.offgr3[2,]),1-frac0(int.offgr3[3,]),1-frac0(int.offgr3[4,]),
              1-frac0(int.drain3[1,]),1-frac0(int.drain3[2,]),1-frac0(int.drain3[3,]),1-frac0(int.drain3[4,]),
              1-frac0(int.septage3[1,]),1-frac0(int.septage3[2,]),1-frac0(int.septage3[3,]),1-frac0(int.septage3[4,]),
              1-frac0(int.produce3[1,]),1-frac0(int.produce3[2,]),1-frac0(int.produce3[3,]),1-frac0(int.produce3[4,]),
              1-frac0(dw3[1,1,]),1-frac0(dw3[2,1,]),1-frac0(dw3[3,1,]),1-frac0(dw3[4,1,]),
              1-frac0(dw3[1,2,]),1-frac0(dw3[2,2,]),1-frac0(dw3[3,2,]),1-frac0(dw3[4,2,]),
              1-frac0(int.total3[1,]),1-frac0(int.total3[2,]),1-frac0(int.total3[3,]),1-frac0(int.total3[4,])),
            ylab="",xaxt="n",
            main=paste("Age Group",ages[k.age]),las = 2,
            #names=c("","dirt","","","","floor","","","","offgr","","","","drain","","","","septage","","","","food","","","","tap water","","","","sachet water","",""),
            col=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
            ylim=c(0,1),cex.names = 0.8);
    mtext("fraction exposed", side=2, line=3, cex=0.9)
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    #legend(0,1,bty="n", cex=0.5, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    par(mar=c(4, 4, 0.5, 0.5))
    z.exp <- mergequan(cbind(log10(non0(int.dirt3[1,])),log10(non0(int.dirt3[2,])),log10(non0(int.dirt3[3,])),log10(non0(int.dirt3[4,])),
                             log10(non0(int.flo3[1,])),log10(non0(int.flo3[2,])),log10(non0(int.flo3[3,])),log10(non0(int.flo3[4,])),
                             log10(non0(int.offgr3[1,])),log10(non0(int.offgr3[2,])),log10(non0(int.offgr3[3,])),log10(non0(int.offgr3[4,])),
                             log10(non0(int.drain3[1,])),log10(non0(int.drain3[2,])),log10(non0(int.drain3[3,])),log10(non0(int.drain3[4,])),
                             log10(non0(int.septage3[1,])),log10(non0(int.septage3[2,])),log10(non0(int.septage3[3,])),log10(non0(int.septage3[4,])),
                             log10(non0(int.produce3[1,])),log10(non0(int.produce3[2,])),log10(non0(int.produce3[3,])),log10(non0(int.produce3[4,])),
                             log10(non0(dw3[1,1,])),log10(non0(dw3[2,1,])),log10(non0(dw3[3,1,])),log10(non0(dw3[4,1,])),   # drink tap water
                             log10(non0(dw3[1,2,])),log10(non0(dw3[2,2,])),log10(non0(dw3[3,2,])),log10(non0(dw3[4,2,])),  # drink sachet water
                             log10(non0(int.total3[1,])),log10(non0(int.total3[2,])),log10(non0(int.total3[3,])),log10(non0(int.total3[4,]))), 
                       c("","soil","","","","floor","","","","offgr","","","","drain","","","","DF","","","","food","","","","tap water","","","","sachet","","","","Total","",""));
    bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,xaxt="n",
        boxfill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4",
                  "antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
        ylab="log10(dose)",#main=paste("Age Group",ages[k.age]),
        cex.axis = 0.8);
    points(1:36,cbind(log10(mean(non0(int.dirt3[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo3[1,]),na.rm=TRUE)),log10(mean(non0(int.flo3[2,]),na.rm=TRUE)),log10(mean(non0(int.flo3[3,]),na.rm=TRUE)),log10(mean(non0(int.flo3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr3[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain3[1,]),na.rm=TRUE)),log10(mean(non0(int.drain3[2,]),na.rm=TRUE)),log10(mean(non0(int.drain3[3,]),na.rm=TRUE)),log10(mean(non0(int.drain3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage3[1,]),na.rm=TRUE)),log10(mean(non0(int.septage3[2,]),na.rm=TRUE)),log10(mean(non0(int.septage3[3,]),na.rm=TRUE)),log10(mean(non0(int.septage3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce3[1,]),na.rm=TRUE)),log10(mean(non0(int.produce3[2,]),na.rm=TRUE)),log10(mean(non0(int.produce3[3,]),na.rm=TRUE)),log10(mean(non0(int.produce3[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw3[1,1,]),na.rm=TRUE)),log10(mean(non0(dw3[2,1,]),na.rm=TRUE)),log10(mean(non0(dw3[3,1,]),na.rm=TRUE)), log10(mean(non0(dw3[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw3[1,2,]),na.rm=TRUE)),log10(mean(non0(dw3[2,2,]),na.rm=TRUE)),log10(mean(non0(dw3[3,2,]),na.rm=TRUE)), log10(mean(non0(dw3[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total3[1,]),na.rm=TRUE)),log10(mean(non0(int.total3[2,]),na.rm=TRUE)),log10(mean(non0(int.total3[3,]),na.rm=TRUE)),log10(mean(non0(int.total3[4,]),na.rm=TRUE))),
           col="black",cex=1
    )
    points(1:36,cbind(log10(mean(non0(int.dirt3[1,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[2,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[3,]),na.rm=TRUE)),log10(mean(non0(int.dirt3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.flo3[1,]),na.rm=TRUE)),log10(mean(non0(int.flo3[2,]),na.rm=TRUE)),log10(mean(non0(int.flo3[3,]),na.rm=TRUE)),log10(mean(non0(int.flo3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.offgr3[1,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[2,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[3,]),na.rm=TRUE)),log10(mean(non0(int.offgr3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.drain3[1,]),na.rm=TRUE)),log10(mean(non0(int.drain3[2,]),na.rm=TRUE)),log10(mean(non0(int.drain3[3,]),na.rm=TRUE)),log10(mean(non0(int.drain3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.septage3[1,]),na.rm=TRUE)),log10(mean(non0(int.septage3[2,]),na.rm=TRUE)),log10(mean(non0(int.septage3[3,]),na.rm=TRUE)),log10(mean(non0(int.septage3[4,]),na.rm=TRUE)),
                      log10(mean(non0(int.produce3[1,]),na.rm=TRUE)),log10(mean(non0(int.produce3[2,]),na.rm=TRUE)),log10(mean(non0(int.produce3[3,]),na.rm=TRUE)),log10(mean(non0(int.produce3[4,]),na.rm=TRUE)),
                      log10(mean(non0(dw3[1,1,]),na.rm=TRUE)),log10(mean(non0(dw3[2,1,]),na.rm=TRUE)),log10(mean(non0(dw3[3,1,]),na.rm=TRUE)), log10(mean(non0(dw3[4,1,]),na.rm=TRUE)),  # drink tap water
                      log10(mean(non0(dw3[1,2,]),na.rm=TRUE)),log10(mean(non0(dw3[2,2,]),na.rm=TRUE)),log10(mean(non0(dw3[3,2,]),na.rm=TRUE)), log10(mean(non0(dw3[4,2,]),na.rm=TRUE)), # drink sachet water
                      log10(mean(non0(int.total3[1,]),na.rm=TRUE)),log10(mean(non0(int.total3[2,]),na.rm=TRUE)),log10(mean(non0(int.total3[3,]),na.rm=TRUE)),log10(mean(non0(int.total3[4,]),na.rm=TRUE))),
           col="black",cex=1.5
    )
    grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
    legend(0,20,bty="n", cex=0.8, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))
    axis(1, at=c(2.5,6.5,10.5,14.5,18.5,22.5,26.5,30.5,34.5), labels=c("soil","floor","offgr","drain",
                                                                       "DF","food","tap water","sachet","total"), las=1, cex.axis=0.8)
  }
}
dev.off();
#####################################################################;
#2D Scatterplot;
env.conc.mean <- function(comb.vec){
  size <- 1;
  e.conc <- c();
  for (i in 1:10000){
    e.conc[i] <-  rgamma(n=size, shape=mean(env.r[comb.vec[2],]), 
                         scale=mean(env.lambda[comb.vec[1],comb.vec[2],]));
  }
  conc <- mean(e.conc);
  return(conc);
}

SP3D.dat1 <- array(0,dim=c(32,5));
SP3D.dat2 <- array(0,dim=c(32,5));
SP3D.dat3 <- array(0,dim=c(32,5));
for (n in 1:n.neighb) {
  for (j in 1:8){
    SP3D.dat1[(n-1)*8+j,1] <- n;
    SP3D.dat1[(n-1)*8+j,2] <- j;
    SP3D.dat1[(n-1)*8+j,3] <- ifelse(j==1,frac0(int.dirt1[n,]),
                                     ifelse(j==2,frac0(int.flo1[n,]),
                                            ifelse(j==3,frac0(int.offgr1[n,]),
                                                   ifelse(j==4,frac0(int.drain1[n,]),
                                                          ifelse(j==5,frac0(int.septage1[n,]),
                                                                 ifelse(j==6,frac0(int.produce1[n,]),
                                                                        ifelse(j==7,frac0(dw1[n,1,]),frac0(dw1[n,2,]))))))));
    # unit: CFU/gram, CFU/cm2 and CFU/ml;
    SP3D.dat1[(n-1)*8+j,4] <- ifelse(j==1,log10(env.conc.mean(c(n,12))*1000),
                                     ifelse(j==2,log10(env.conc.mean(c(n,27))),
                                            ifelse(j==3,log10(env.conc.mean(c(n,28))),
                                                   ifelse(j==4,log10(env.conc.mean(c(n,53))),
                                                          ifelse(j==5,log10(env.conc.mean(c(n,31))*1000),
                                                                 ifelse(j==6,log10(mean(env.conc.mean(c(n,1)),env.conc.mean(c(n,3)),env.conc.mean(c(n,4)),env.conc.mean(c(n,7)),
                                                                                        env.conc.mean(c(n,8)),env.conc.mean(c(n,63)),env.conc.mean(c(n,83)),env.conc.mean(c(n,84)))),
                                                                        ifelse(j==7,log10(env.conc.mean(c(n,13))),log10(env.conc.mean(c(n,16))))))))));
    SP3D.dat1[(n-1)*8+j,5] <- ifelse(j==1,log10(mean(non0(int.dirt1[n,]),na.rm=TRUE)),
                                     ifelse(j==2,log10(mean(non0(int.flo1[n,]),na.rm=TRUE)),
                                            ifelse(j==3,log10(mean(non0(int.offgr1[n,]),na.rm=TRUE)),
                                                   ifelse(j==4,log10(mean(non0(int.drain1[n,]),na.rm=TRUE)),
                                                          ifelse(j==5,log10(mean(non0(int.septage1[n,]),na.rm=TRUE)),
                                                                 ifelse(j==6,log10(mean(non0(int.produce1[n,]),na.rm=TRUE)),
                                                                        ifelse(j==7,log10(mean(non0(dw1[n,1,]),na.rm=TRUE)),log10(mean(non0(dw1[n,2,]),na.rm=TRUE)))))))));
  }
}

for (n in 1:n.neighb) {
  for (j in 1:8){
    SP3D.dat2[(n-1)*8+j,1] <- n;
    SP3D.dat2[(n-1)*8+j,2] <- j;
    SP3D.dat2[(n-1)*8+j,3] <- ifelse(j==1,frac0(int.dirt2[n,]),
                                     ifelse(j==2,frac0(int.flo2[n,]),
                                            ifelse(j==3,frac0(int.offgr2[n,]),
                                                   ifelse(j==4,frac0(int.drain2[n,]),
                                                          ifelse(j==5,frac0(int.septage2[n,]),
                                                                 ifelse(j==6,frac0(int.produce2[n,]),
                                                                        ifelse(j==7,frac0(dw2[n,1,]),frac0(dw2[n,2,]))))))));
    # unit: CFU/gram, CFU/cm2 and CFU/ml;
    SP3D.dat2[(n-1)*8+j,4] <- ifelse(j==1,log10(env.conc.mean(c(n,12))*1000),
                                     ifelse(j==2,log10(env.conc.mean(c(n,27))),
                                            ifelse(j==3,log10(env.conc.mean(c(n,28))),
                                                   ifelse(j==4,log10(env.conc.mean(c(n,53))),
                                                          ifelse(j==5,log10(env.conc.mean(c(n,31))*1000),
                                                                 ifelse(j==6,log10(mean(env.conc.mean(c(n,1)),env.conc.mean(c(n,3)),env.conc.mean(c(n,4)),env.conc.mean(c(n,7)),
                                                                                        env.conc.mean(c(n,8)),env.conc.mean(c(n,63)),env.conc.mean(c(n,83)),env.conc.mean(c(n,84)))),
                                                                        ifelse(j==7,log10(env.conc.mean(c(n,13))),log10(env.conc.mean(c(n,16))))))))));
    SP3D.dat2[(n-1)*8+j,5] <- ifelse(j==1,log10(mean(non0(int.dirt2[n,]),na.rm=TRUE)),
                                     ifelse(j==2,log10(mean(non0(int.flo2[n,]),na.rm=TRUE)),
                                            ifelse(j==3,log10(mean(non0(int.offgr2[n,]),na.rm=TRUE)),
                                                   ifelse(j==4,log10(mean(non0(int.drain2[n,]),na.rm=TRUE)),
                                                          ifelse(j==5,log10(mean(non0(int.septage2[n,]),na.rm=TRUE)),
                                                                 ifelse(j==6,log10(mean(non0(int.produce2[n,]),na.rm=TRUE)),
                                                                        ifelse(j==7,log10(mean(non0(dw2[n,1,]),na.rm=TRUE)),log10(mean(non0(dw2[n,2,]),na.rm=TRUE)))))))));
  }
}

for (n in 1:n.neighb) {
  for (j in 1:8){
    SP3D.dat3[(n-1)*8+j,1] <- n;
    SP3D.dat3[(n-1)*8+j,2] <- j;
    SP3D.dat3[(n-1)*8+j,3] <- ifelse(j==1,frac0(int.dirt3[n,]),
                                     ifelse(j==2,frac0(int.flo3[n,]),
                                            ifelse(j==3,frac0(int.offgr3[n,]),
                                                   ifelse(j==4,frac0(int.drain3[n,]),
                                                          ifelse(j==5,frac0(int.septage3[n,]),
                                                                 ifelse(j==6,frac0(int.produce3[n,]),
                                                                        ifelse(j==7,frac0(dw3[n,1,]),frac0(dw3[n,2,]))))))));
    # unit: CFU/gram, CFU/cm2 and CFU/ml;
    SP3D.dat3[(n-1)*8+j,4] <- ifelse(j==1,log10(env.conc.mean(c(n,12))*1000),
                                     ifelse(j==2,log10(env.conc.mean(c(n,27))),
                                            ifelse(j==3,log10(env.conc.mean(c(n,28))),
                                                   ifelse(j==4,log10(env.conc.mean(c(n,53))),
                                                          ifelse(j==5,log10(env.conc.mean(c(n,31))*1000),
                                                                 ifelse(j==6,log10(mean(env.conc.mean(c(n,1)),env.conc.mean(c(n,3)),env.conc.mean(c(n,4)),env.conc.mean(c(n,7)),
                                                                                        env.conc.mean(c(n,8)),env.conc.mean(c(n,63)),env.conc.mean(c(n,83)),env.conc.mean(c(n,84)))),
                                                                        ifelse(j==7,log10(env.conc.mean(c(n,13))),log10(env.conc.mean(c(n,16))))))))));
    SP3D.dat3[(n-1)*8+j,5] <- ifelse(j==1,log10(mean(non0(int.dirt3[n,]),na.rm=TRUE)),
                                     ifelse(j==2,log10(mean(non0(int.flo3[n,]),na.rm=TRUE)),
                                            ifelse(j==3,log10(mean(non0(int.offgr3[n,]),na.rm=TRUE)),
                                                   ifelse(j==4,log10(mean(non0(int.drain3[n,]),na.rm=TRUE)),
                                                          ifelse(j==5,log10(mean(non0(int.septage3[n,]),na.rm=TRUE)),
                                                                 ifelse(j==6,log10(mean(non0(int.produce3[n,]),na.rm=TRUE)),
                                                                        ifelse(j==7,log10(mean(non0(dw3[n,1,]),na.rm=TRUE)),log10(mean(non0(dw3[n,2,]),na.rm=TRUE)))))))));
  }
}

SP3D.dat1<-data.frame(SP3D.dat1[,1],SP3D.dat1[,2],SP3D.dat1[,3],SP3D.dat1[,4],SP3D.dat1[,5])
colnames(SP3D.dat1)<-c("Neighb","Pathway","frac0","env.conc","env.intake");
SP3D.dat2<-data.frame(SP3D.dat2[,1],SP3D.dat2[,2],SP3D.dat2[,3],SP3D.dat2[,4],SP3D.dat2[,5])
colnames(SP3D.dat2)<-c("Neighb","Pathway","frac0","env.conc","env.intake");
SP3D.dat3<-data.frame(SP3D.dat3[,1],SP3D.dat3[,2],SP3D.dat3[,3],SP3D.dat3[,4],SP3D.dat3[,5])
colnames(SP3D.dat3)<-c("Neighb","Pathway","frac0","env.conc","env.intake");

# create column indicating point color
SP3D.dat1$pcolor[SP3D.dat1$Pathway==1] <- "chocolate3";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==2] <- "antiquewhite4";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==3] <- "antiquewhite2";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==4] <- "black";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==5] <- "firebrick4";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==6] <- "green2";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==7] <- "blue";
SP3D.dat1$pcolor[SP3D.dat1$Pathway==8] <- "cyan";
SP3D.dat1$pshape[SP3D.dat1$Neighb==1] <- 15;
SP3D.dat1$pshape[SP3D.dat1$Neighb==2] <- 17;
SP3D.dat1$pshape[SP3D.dat1$Neighb==3] <- 18;
SP3D.dat1$pshape[SP3D.dat1$Neighb==4] <- 19;

# create column indicating point color
SP3D.dat2$pcolor[SP3D.dat2$Pathway==1] <- "chocolate3";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==2] <- "antiquewhite4";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==3] <- "antiquewhite2";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==4] <- "black";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==5] <- "firebrick4";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==6] <- "green2";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==7] <- "blue";
SP3D.dat2$pcolor[SP3D.dat2$Pathway==8] <- "cyan";
SP3D.dat2$pshape[SP3D.dat2$Neighb==1] <- 15;
SP3D.dat2$pshape[SP3D.dat2$Neighb==2] <- 17;
SP3D.dat2$pshape[SP3D.dat2$Neighb==3] <- 18;
SP3D.dat2$pshape[SP3D.dat2$Neighb==4] <- 19;

# create column indicating point color
SP3D.dat3$pcolor[SP3D.dat3$Pathway==1] <- "chocolate3";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==2] <- "antiquewhite4";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==3] <- "antiquewhite2";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==4] <- "black";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==5] <- "firebrick4";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==6] <- "green2";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==7] <- "blue";
SP3D.dat3$pcolor[SP3D.dat3$Pathway==8] <- "cyan";
SP3D.dat3$pshape[SP3D.dat3$Neighb==1] <- 15;
SP3D.dat3$pshape[SP3D.dat3$Neighb==2] <- 17;
SP3D.dat3$pshape[SP3D.dat3$Neighb==3] <- 18;
SP3D.dat3$pshape[SP3D.dat3$Neighb==4] <- 19;

pdf(paste("./output/","exposure","-2D-scatterplot",".pdf",sep=""));
#scatter plot - X:Exposure Y:Fract0
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights=c(1,1,1))
par(mar=c(2, 4, 2, 1))
plot(SP3D.dat1$env.intake[which(SP3D.dat1$Neighb==1)],1-SP3D.dat1$frac0[which(SP3D.dat1$Neighb==1)],type="p",col=SP3D.dat1$pcolor,pch=15,cex=2,xlim=c(0,20),ylim=c(0,1),
     #main="Age 0-1", 
     ylab="fraction exposed", xlab="",
     #xlab="Log 10 Mean E. coli Exposure For 14 Hours (CFU)",
     xaxt="n")
points(SP3D.dat1$env.intake[which(SP3D.dat1$Neighb==2)],1-SP3D.dat1$frac0[which(SP3D.dat1$Neighb==2)],type="p",col=SP3D.dat1$pcolor,pch=17,cex=2)
points(SP3D.dat1$env.intake[which(SP3D.dat1$Neighb==3)],1-SP3D.dat1$frac0[which(SP3D.dat1$Neighb==3)],type="p",col=SP3D.dat1$pcolor,pch=18,cex=2)
points(SP3D.dat1$env.intake[which(SP3D.dat1$Neighb==4)],1-SP3D.dat1$frac0[which(SP3D.dat1$Neighb==4)],type="p",col=SP3D.dat1$pcolor,pch=19,cex=2)
grid(nx=NULL, ny=NULL,col="darkgray",lwd=1);
legend(8.5,1.05,expression(bold("Age 0-1")),cex=1.5,bty="n");
legend(15, 0.75,      # location and inset
       bty="n", cex=1,              # suppress legend box, shrink text 50%
       title="Neighborhood",
       c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
legend(18, 0.75,      # location and inset
       bty="n", cex=1,              # suppress legend box, shrink text 50%
       title="Pathway",
       c("soil","floor","offgr","drain",
         "DF","food","tap water","sachet"), pch=19,
       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))

#scatter plot - X:intake Y:Fract0
par(mar=c(3, 4, 1, 1))
plot(SP3D.dat2$env.intake[which(SP3D.dat2$Neighb==1)],1-SP3D.dat2$frac0[which(SP3D.dat2$Neighb==1)],type="p",col=SP3D.dat2$pcolor,pch=15,cex=2,xlim=c(0,20),ylim=c(0,1),
     #main="Age 1-2",
     ylab="fraction exposed", xlab="",
     #xlab="Log 10 Mean E. coli Intake For 14 Hours (CFU)",
     xaxt="n")
points(SP3D.dat2$env.intake[which(SP3D.dat2$Neighb==2)],1-SP3D.dat2$frac0[which(SP3D.dat2$Neighb==2)],type="p",col=SP3D.dat2$pcolor,pch=17,cex=2)
points(SP3D.dat2$env.intake[which(SP3D.dat2$Neighb==3)],1-SP3D.dat2$frac0[which(SP3D.dat2$Neighb==3)],type="p",col=SP3D.dat2$pcolor,pch=18,cex=2)
points(SP3D.dat2$env.intake[which(SP3D.dat2$Neighb==4)],1-SP3D.dat2$frac0[which(SP3D.dat2$Neighb==4)],type="p",col=SP3D.dat2$pcolor,pch=19,cex=2)
grid(nx=NULL, ny=NULL,col="darkgray",lwd=1);
legend(8.5,1.05,expression(bold("Age 1-2")),cex=1.5,bty="n");
#legend(16, 0.985,      # location and inset
#       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
#       title="Neighborhood",
#       c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
#legend(18, 0.985,      # location and inset
#       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
#       title="Pathway",
#       c("soil","floor","offgr","drain",
#         "DF","food","tap water","sachet"), pch=19,
#       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))

#scatter plot - X:intake Y:Fract0
par(mar=c(4, 4, 0, 1))
plot(SP3D.dat3$env.intake[which(SP3D.dat3$Neighb==1)],1-SP3D.dat3$frac0[which(SP3D.dat3$Neighb==1)],type="p",col=SP3D.dat3$pcolor,pch=15,cex=2,xlim=c(0,20),ylim=c(0,1),
     #main="Age 2-5", 
     ylab="fraction exposed", xlab="mean of dose for 14 hr (CFU)",xaxt="n")
points(SP3D.dat3$env.intake[which(SP3D.dat3$Neighb==2)],1-SP3D.dat3$frac0[which(SP3D.dat3$Neighb==2)],type="p",col=SP3D.dat3$pcolor,pch=17,cex=2)
points(SP3D.dat3$env.intake[which(SP3D.dat3$Neighb==3)],1-SP3D.dat3$frac0[which(SP3D.dat3$Neighb==3)],type="p",col=SP3D.dat3$pcolor,pch=18,cex=2)
points(SP3D.dat3$env.intake[which(SP3D.dat3$Neighb==4)],1-SP3D.dat3$frac0[which(SP3D.dat3$Neighb==4)],type="p",col=SP3D.dat3$pcolor,pch=19,cex=2)
grid(nx=NULL, ny=NULL,col="darkgray",lwd=1);
legend(8.5,1.05,expression(bold("Age 2-5")),cex=1.5,bty="n");

#legend(16, 0.985,      # location and inset
#       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
#       title="Neighborhood",
#       c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
#legend(18, 0.985,      # location and inset
#       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
#       title="Pathway",
#       c("soil","floor","offgr","drain",
#         "DF","food","tap water","sachet"), pch=19,
#       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))
axt<-c(0,5,10,15,20)
eaxis(1,at=axt,labels=c(0,pretty10exp(10^axt[2:5], drop.1=TRUE, sub10 = "10")))


dev.off();

###################################################################;
#Network#;
library(igraph);

eqarrowPlot <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                        edge.arrow.size=rep(1, ecount(graph)),
                        vertex.shape="circle",
                        edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], layout=layout, vertex.shape="none",
         vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

eqarrowPlot1 <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                        edge.arrow.size=rep(1, ecount(graph)),
                        vertex.shape="circle",
                        edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], edge.width=edge.arrow.size[e]*2,
         layout=layout, vertex.shape="none",vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

pdf(paste("./output/","exposure","-network-color1",".pdf",sep=""));
par(mfrow=c(2,2),mai = c(0.1, 0.1, 0.1, 0.1));
par(mar=c(0,0,0,0));
for (k.neighb in 1:n.neighb){
  a1.net <- data.frame(start=as.character(Net.HH.a1[[k.neighb]]$start),end=as.character(Net.HH.a1[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a1[[k.neighb]]$size)),stringsAsFactors = FALSE)
  edge <-aggregate(a1.net$size, by=list(a1.net$start,a1.net$end), FUN=sum, na.rm=TRUE)
  edge1 <- edge[which(edge[,3]!=0),]
  edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])
  
  edge.network<-graph.data.frame(edge2, directed=T);
  
  if (k.neighb!=4){
    edge.network<-add.vertices(edge.network,1);
    V(edge.network)[12]$name<-"Sachet water";
  }
  
  n.node <- length(V(edge.network))
  node.size <- c(0,0,0,0,0,0,0,0,0,0,0,0);
  node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
  node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
  node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
  node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
  node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
  node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
  node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
  node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
  node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
  node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
  node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
  node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
  node.size[which(node.size<=0)] <- 1;
  V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
  V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
  V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
  V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
  V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-node.size*5;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  #E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-log10(as.numeric(E(edge.network)$size));
  #weight[which(weight==-Inf)] <- 0;
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
             0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
  if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                              0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);
  
  curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight/10, vertex.size=35, 
              edge.curved=curved,vertex.label.color="black");
  
  legend("topleft",paste(neighbourhoods[k.neighb],"age 0-1"),bty="n", cex=1);
}

par(mfrow=c(2,2),mai = c(0.1, 0.1, 0.1, 0.1));
par(mar=c(0,0,0,0));
for (k.neighb in 1:n.neighb){
  a2.net <- data.frame(start=as.character(Net.HH.a2[[k.neighb]]$start),end=as.character(Net.HH.a2[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a2[[k.neighb]]$size)),stringsAsFactors = FALSE)
  edge <-aggregate(a2.net$size, by=list(a2.net$start,a2.net$end), FUN=sum, na.rm=TRUE)
  edge1 <- edge[which(edge[,3]!=0),]
  edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])
  
  edge.network<-graph.data.frame(edge2, directed=T);
  
  if (k.neighb!=4){
    edge.network<-add.vertices(edge.network,1);
    V(edge.network)[12]$name<-"Sachet water";
  }
  
  n.node <- length(V(edge.network))
  node.size <- c(0,0,0,0,0,0,0,0,0,0,0);
  node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
  node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
  node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
  node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
  node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
  node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
  node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
  node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
  node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
  node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
  node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
  node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
  node.size[which(node.size<=0)] <- 1;
  V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
  V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
  V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
  V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
  V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-node.size*5;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  #E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-log10(as.numeric(E(edge.network)$size));
  #weight[which(weight==-Inf)] <- 0;
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
             0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
  if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                              0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);
  
  curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight/10, vertex.size=35, 
              edge.curved=curved,vertex.label.color="black")
  legend("topleft",paste(neighbourhoods[k.neighb],"age 1-2"),bty="n", cex=1);
}

par(mfrow=c(2,2),mai = c(0.1, 0.1, 0.1, 0.1));
par(mar=c(0,0,0,0));
for (k.neighb in 1:n.neighb){
  a3.net <- data.frame(start=as.character(Net.HH.a3[[k.neighb]]$start),end=as.character(Net.HH.a3[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a3[[k.neighb]]$size)),stringsAsFactors = FALSE)
  edge <-aggregate(a3.net$size, by=list(a3.net$start,a3.net$end), FUN=sum, na.rm=TRUE)
  edge1 <- edge[which(edge[,3]!=0),]
  edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])
  
  edge.network<-graph.data.frame(edge2, directed=T);
  
  if (k.neighb!=4){
    edge.network<-add.vertices(edge.network,1);
    V(edge.network)[12]$name<-"Sachet water";
  }
  
  n.node <- length(V(edge.network))
  node.size <- c(0,0,0,0,0,0,0,0,0,0);
  node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
  node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
  node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
  node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
  node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
  node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
  node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
  node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
  node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
  node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
  node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
  node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
  node.size[which(node.size<=0)] <- 1;
  V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
  V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
  V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
  V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
  V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-node.size*5;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  #E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-log10(as.numeric(E(edge.network)$size));
  #weight[which(weight==-Inf)] <- 0;
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
             0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
  if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                              0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);
  
  curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight/10,edge.width=1, vertex.size=35, 
              edge.curved=curved,vertex.label.color="black")
  legend("topleft",paste(neighbourhoods[k.neighb],"age 2-5"),bty="n", cex=1);
}
dev.off();

#####################################################################################



pdf(paste("./output/","exposure","-network-color2",".pdf",sep=""),width=10.00, height=14.14);
par(mfrow=c(4,3),mai = c(0.05, 0, 0.05, 0));
par(mar=c(0,0,0,0));
for (k.neighb in 1:n.neighb){
  if (k.neighb==1) par(mai = c(0.2, 0, 0.2, 0));
  if (k.neighb!=1) par(mai = c(0.05, 0, 0.05, 0));
  a1.net <- data.frame(start=as.character(Net.HH.a1[[k.neighb]]$start),end=as.character(Net.HH.a1[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a1[[k.neighb]]$size)),stringsAsFactors = FALSE)
  edge <-aggregate(a1.net$size, by=list(a1.net$start,a1.net$end), FUN=sum, na.rm=TRUE)
  edge1 <- edge[which(edge[,3]!=0),]
  edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])
  
  edge.network<-graph.data.frame(edge2, directed=T);
  
  if (k.neighb!=4){
    edge.network<-add.vertices(edge.network,1);
    V(edge.network)[12]$name<-"Sachet water";
  }
  
  n.node <- length(V(edge.network))
  node.size <- c(0,0,0,0,0,0,0,0,0,0,0,0);
  node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
  node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
  node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
  node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
  node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
  node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
  node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
  node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
  node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
  node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
  node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
  node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
  node.size[which(node.size<=0)] <- 1;
  V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
  V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
  V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
  V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
  V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-node.size*5;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  #E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-log10(as.numeric(E(edge.network)$size));
  #weight[which(weight==-Inf)] <- 0;
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
             0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
  if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                              0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);
  
  curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight/15, vertex.size=35, 
               edge.curved=curved,vertex.label.color="black");
  
  #legend("bottom",paste(neighbourhoods[k.neighb],"age 0-1"),bty="n", cex=1);
  titleline<-ifelse(k.neighb==1,0,-1);
  mtext(paste(neighbourhoods[k.neighb],"age 0-1"),side=1,outer=FALSE,line=titleline,cex=1);
####################################################################################;
a2.net <- data.frame(start=as.character(Net.HH.a2[[k.neighb]]$start),end=as.character(Net.HH.a2[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a2[[k.neighb]]$size)),stringsAsFactors = FALSE)
edge <-aggregate(a2.net$size, by=list(a2.net$start,a2.net$end), FUN=sum, na.rm=TRUE)
edge1 <- edge[which(edge[,3]!=0),]
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])

edge.network<-graph.data.frame(edge2, directed=T);

if (k.neighb!=4){
  edge.network<-add.vertices(edge.network,1);
  V(edge.network)[12]$name<-"Sachet water";
}

n.node <- length(V(edge.network))
node.size <- c(0,0,0,0,0,0,0,0,0,0,0);
node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
node.size[which(node.size<=0)] <- 1;
V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";

V(edge.network)$size<-node.size*5;
V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                       ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
E(edge.network)$color <- "red";
E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
#E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
E(edge.network)$curved=FALSE;
E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;

weight<-log10(as.numeric(E(edge.network)$size));
#weight[which(weight==-Inf)] <- 0;
weight[which(weight<=0)] <- 1;#replace negative number

l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
           0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                            0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);

curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);

eqarrowPlot1(edge.network, l, edge.arrow.size=weight/15, vertex.size=35, 
             edge.curved=curved,vertex.label.color="black")
#legend("bottom",paste(neighbourhoods[k.neighb],"age 1-2"),bty="n", cex=1);
mtext(paste(neighbourhoods[k.neighb],"age 1-2"),side=1,outer=FALSE,line=titleline,cex=1);

  a3.net <- data.frame(start=as.character(Net.HH.a3[[k.neighb]]$start),end=as.character(Net.HH.a3[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a3[[k.neighb]]$size)),stringsAsFactors = FALSE)
  edge <-aggregate(a3.net$size, by=list(a3.net$start,a3.net$end), FUN=sum, na.rm=TRUE)
  edge1 <- edge[which(edge[,3]!=0),]
  edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])
  
  edge.network<-graph.data.frame(edge2, directed=T);
  
  if (k.neighb!=4){
    edge.network<-add.vertices(edge.network,1);
    V(edge.network)[12]$name<-"Sachet water";
  }
  
  n.node <- length(V(edge.network))
  node.size <- c(0,0,0,0,0,0,0,0,0,0);
  node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
  node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
  node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
  node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
  node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
  node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
  node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
  node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
  node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
  node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
  node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
  node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
  node.size[which(node.size<=0)] <- 1;
  V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
  V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
  V(edge.network)$name[which(V(edge.network)$name=="Septage")]="DF";
  V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
  V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-node.size*5;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  #E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-log10(as.numeric(E(edge.network)$size));
  #weight[which(weight==-Inf)] <- 0;
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
             0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);
  if (k.neighb==4) l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,3,2.25,0.5,2,4,
                              0,2,0,-2,-1,-2,1,2,0.7,2,1.5,0),12,2);
  
  curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  if (k.neighb==4) curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight/15,edge.width=1, vertex.size=35, 
               edge.curved=curved,vertex.label.color="black")
  #legend("bottom",paste(neighbourhoods[k.neighb],"age 2-5"),bty="n", cex=1);
  mtext(paste(neighbourhoods[k.neighb],"age 2-5"),side=1,outer=FALSE,line=titleline,cex=1);
}
dev.off();