pdf(paste("./output/","exposure","-hh-combined-by-age",".pdf",sep=""));
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
  par(mar=c(0.5, 4, 2, 0.5))
  barplot(c(frac0(int.dirt1[,]),frac0(int.dirt2[,]),frac0(int.dirt3[,]),
            frac0(int.flo1[,]),frac0(int.flo2[,]),frac0(int.flo3[,]),
            frac0(int.offgr1[,]),frac0(int.offgr2[,]),frac0(int.offgr3[,]),
            frac0(int.drain1[,]),frac0(int.drain2[,]),frac0(int.drain3[,]),
            frac0(int.septage1[,]),frac0(int.septage2[,]),frac0(int.septage3[,]),
            frac0(int.produce1[,]),frac0(int.produce2[,]),frac0(int.produce3[,]),
            frac0(dw1[,1,]),frac0(dw2[,1,]),frac0(dw2[,1,]),
            frac0(dw2[,2,]),frac0(dw2[,2,]),frac0(dw2[,2,]),
            frac0(int.total1[,]),frac0(int.total2[,]),frac0(int.total3[,])),
          ylab="fraction 0",
          main="Combined Four Neighborhoods",las = 2,
          #            names=c("","dirt","","","floor","","","offgr","","","drain","","","septage","","","food","","","tap water","","","sachet water",""),
          col=c("antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4"),
          ylim=c(0,1),cex.names = 0.8);
  grid(nx=NA, ny=NULL);
  legend(-0.7,1,bty="n", cex=0.7, title="Age Group",c("0-1","1-2","2-5"),fill=c("antiquewhite2","antiquewhite3","antiquewhite4"))
  
  
  par(mar=c(4, 4, 0.5, 0.5))
  #####################################################################;
  z.exp <- mergequan(cbind(log10(as.vector(non0(int.dirt1[,]))),log10(as.vector(non0(int.dirt2[,]))),log10(as.vector(non0(int.dirt3[,]))), # dirt floor -> hand-mouth
                           log10(as.vector(non0(int.flo1[,]))),log10(as.vector(non0(int.flo2[,]))),log10(as.vector(non0(int.flo3[,]))), # concrete floor -> hand-mouth
                           log10(as.vector(non0(int.offgr1[,]))),log10(as.vector(non0(int.offgr2[,]))),log10(as.vector(non0(int.offgr3[,]))), # off ground -> hand-mouth
                           log10(as.vector(non0(int.drain1[,]))),log10(as.vector(non0(int.drain2[,]))),log10(as.vector(non0(int.drain3[,]))), # drain -> hand-mouth
                           log10(as.vector(non0(int.septage1[,]))),log10(as.vector(non0(int.septage2[,]))),log10(as.vector(non0(int.septage3[,]))), # septage -> hand-mouth     
                           log10(as.vector(non0(int.produce1[,]))),log10(as.vector(non0(int.produce2[,]))),log10(as.vector(non0(int.produce3[,]))), # eat -> ingest
                           log10(as.vector(non0(dw1[,1,]))),log10(as.vector(non0(dw2[,1,]))),log10(as.vector(non0(dw3[,1,]))),   # drink tap water
                           log10(as.vector(non0(dw1[,2,]))),log10(as.vector(non0(dw2[,2,]))),log10(as.vector(non0(dw3[,2,]))),  # drink sachet water
                           log10(as.vector(non0(int.total1[,]))),log10(as.vector(non0(int.total2[,]))),log10(as.vector(non0(int.total3[,])))), c());
  #c("","dirt","","","floor","","","offgr","","","drain","","","septage","","","food","","","tap water","","","sachet water","","","Total",""));
  bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,xaxt="n",
      boxfill=c("antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite2","antiquewhite3","antiquewhite4",
                "antiquewhite2","antiquewhite3","antiquewhite4"),
      ylab="10log(dose)",#main=paste(neighbourhoods[k.neighb]),
      cex.axis = 0.8);
  points(1:27,cbind(log10(mean(as.vector(non0(int.dirt1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.dirt2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.dirt3[,])),na.rm=TRUE)), # dirt floor -> hand-mouth
                    log10(mean(as.vector(non0(int.flo1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.flo2[,])),na.rm=TRUE)), log10(mean(as.vector(non0(int.flo3[,])),na.rm=TRUE)),# concrete floor -> hand-mouth
                    log10(mean(as.vector(non0(int.offgr1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.offgr2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.offgr3[,])),na.rm=TRUE)), # off ground -> hand-mouth
                    log10(mean(as.vector(non0(int.drain1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.drain2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.drain3[,])),na.rm=TRUE)), # drain -> hand-mouth
                    log10(mean(as.vector(non0(int.septage1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.septage2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.septage3[,])),na.rm=TRUE)), # septage -> hand-mouth     
                    log10(mean(as.vector(non0(int.produce1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.produce2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.produce3[,])),na.rm=TRUE)), # eat -> ingest
                    log10(mean(as.vector(non0(dw1[,1,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw2[,1,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw3[,1,])),na.rm=TRUE)),   # drink tap water
                    log10(mean(as.vector(non0(dw1[,2,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw2[,2,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw3[,2,])),na.rm=TRUE)),  # drink sachet water
                    log10(mean(as.vector(non0(int.total1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.total2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.total3[,])),na.rm=TRUE))),
         col="black",cex=1
  )
  points(1:27,cbind(log10(mean(as.vector(non0(int.dirt1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.dirt2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.dirt3[,])),na.rm=TRUE)), # dirt floor -> hand-mouth
                    log10(mean(as.vector(non0(int.flo1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.flo2[,])),na.rm=TRUE)), log10(mean(as.vector(non0(int.flo3[,])),na.rm=TRUE)),# concrete floor -> hand-mouth
                    log10(mean(as.vector(non0(int.offgr1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.offgr2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.offgr3[,])),na.rm=TRUE)), # off ground -> hand-mouth
                    log10(mean(as.vector(non0(int.drain1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.drain2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.drain3[,])),na.rm=TRUE)), # drain -> hand-mouth
                    log10(mean(as.vector(non0(int.septage1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.septage2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.septage3[,])),na.rm=TRUE)), # septage -> hand-mouth     
                    log10(mean(as.vector(non0(int.produce1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.produce2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.produce3[,])),na.rm=TRUE)), # eat -> ingest
                    log10(mean(as.vector(non0(dw1[,1,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw2[,1,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw3[,1,])),na.rm=TRUE)),   # drink tap water
                    log10(mean(as.vector(non0(dw1[,2,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw2[,2,])),na.rm=TRUE)),log10(mean(as.vector(non0(dw3[,2,])),na.rm=TRUE)),  # drink sachet water
                    log10(mean(as.vector(non0(int.total1[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.total2[,])),na.rm=TRUE)),log10(mean(as.vector(non0(int.total3[,])),na.rm=TRUE))),
         col="black",cex=1.5
  )
  grid(nx=NA, ny=NULL);
  legend(0,20,bty="n", cex=0.8, title="Age Group",c("0-1","1-2","2-5"),fill=c("antiquewhite2","antiquewhite3","antiquewhite4"))
  axis(1, at=c(2,5,8,11,14,17,20,23,26), labels=c("dirt","floor","offgr","drain",
                                                  "septage","food","tap water","sachet","total"), las=1, cex.axis=0.8)

dev.off();