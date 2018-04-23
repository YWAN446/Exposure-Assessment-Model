library(lattice);
setwd("~/stat/sanipath/exposure/exposure/v9/")
neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");
labels <- c("dirt","floor","offgr","drain",
            "septage","food","tap water","sachet water","total");

pdf(paste("./output/","exposure","-no-nb",subset[1],subset[2],".pdf",sep=""));

frac0 <- function(mc) return(1-length(mc[mc>0])/length(mc));
non0 <- function(mc){
  tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
}

par(mfrow=c(1,4));
for(k.neighb in 1:n.neighb){
  barplot(c(frac0(int.dirt[k.neighb,]),
            frac0(int.flo[k.neighb,]),
            frac0(int.offgr[k.neighb,]),
            frac0(int.drain[k.neighb,]),
            frac0(int.septage[k.neighb,]),
            frac0(int.produce[k.neighb,]),
            frac0(dw[k.neighb,1,]),
            frac0(dw[k.neighb,2,])),
          ylab="fraction 0",
          main=neighbourhoods[k.neighb],
          names=c("dirt","floor","offgr","drain","septage","food","tap water","sachet water"),ylim=c(0,1),cex.names = 0.8);
}
par(mfrow=c(1,4));
  for(k.neighb in 1:n.neighb){
  boxplot(
    cbind(log10(non0(int.dirt[k.neighb,])), # dirt floor -> hand-mouth
          log10(non0(int.flo[k.neighb,])), # concrete floor -> hand-mouth
          log10(non0(int.offgr[k.neighb,])), # off ground -> hand-mouth
          log10(non0(int.drain[k.neighb,])), # drain -> hand-mouth
          log10(non0(int.septage[k.neighb,])), # septage -> hand-mouth     
          log10(non0(int.produce[k.neighb,])), # eat -> ingest
          log10(non0(dw[k.neighb,1,])),   # drink tap water
          log10(non0(dw[k.neighb,2,])),  # drink sachet water
          log10(non0(int.total[k.neighb,]))),
    ylab="log(dose)",
    outline=FALSE,ylim=c(-15,15),
    main=neighbourhoods[k.neighb],names=labels,cex.axis = 0.8);
}

par(mfrow=c(2,1));
for (k.neighb in 1:4){
plot(density(log10(non0(int.dirt[k.neighb,]))[int.dirt[k.neighb,]!=0]),col=2,xlim=c(-5,10),ylim=c(0,2),lwd=2,main=neighbourhoods[k.neighb])
lines(density(log10(non0(int.flo[k.neighb,]))[int.flo[k.neighb,]!=0]),col=3,lwd=2)
lines(density(log10(non0(int.offgr[k.neighb,]))[int.offgr[k.neighb,]!=0]),col=4,lwd=2)
lines(density(log10(non0(int.drain[k.neighb,]))[int.drain[k.neighb,]!=0]),col=5,lwd=2)
lines(density(log10(non0(int.septage[k.neighb,]))[int.septage[k.neighb,]!=0]),col=6,lwd=2)
lines(density(log10(non0(int.produce[k.neighb,]))[int.produce[k.neighb,]!=0]),col=7,lwd=2)
lines(density(log10(non0(dw[k.neighb,1,]))[dw[k.neighb,1,]!=0]),col=8,lwd=2)
lines(density(log10(non0(dw[k.neighb,2,]))[dw[k.neighb,2,]!=0]),col=9,lwd=2)
legend(-5,2,c("dirt","floor","offgr","drain","septage","food","tap water","sachet water"),
       col=c(2,3,4,5,6,7,8,9),lwd=c(2,2,2,2,2,2,2,2),lty=c(1,1,1,1,1,1,1,1),cex=0.7)
}

#par(mfrow=c(1,4));
#for(k.neighb in 1:n.neighb){
#  boxplot(
#    cbind(int.dirt[k.neighb,]/int.total[k.neighb,], # dirt floor -> hand-mouth
#          int.flo[k.neighb,]/int.total[k.neighb,], # concrete floor -> hand-mouth
#          int.offgr[k.neighb,]/int.total[k.neighb,], # off ground -> hand-mouth
#          int.drain[k.neighb,]/int.total[k.neighb,], # drain -> hand-mouth
#          int.septage[k.neighb,]/int.total[k.neighb,], # septage -> hand-mouth     
#          int.produce[k.neighb,]/int.total[k.neighb,], # eat -> ingest
#          dw[k.neighb,1,]/int.total[k.neighb,],   # drink tap water
#          dw[k.neighb,2,]/int.total[k.neighb,]),  # drink sachet water
#    ylab="percent of intake",
#    outline=FALSE,ylim=c(0,1),
#    main=neighbourhoods[k.neighb],names=c("dirt","floor","offgr","drain",
#                                         "septage","food","tap water","sachet water"),cex.axis = 0.8);
#}

dev.off();

#3D scatter plot;
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

SP3D.dat <- array(0,dim=c(32,5));
  for (n in 1:n.neighb) {
    for (j in 1:8){
      SP3D.dat[(n-1)*8+j,1] <- n;
      SP3D.dat[(n-1)*8+j,2] <- j;
      SP3D.dat[(n-1)*8+j,3] <- ifelse(j==1,frac0(int.dirt[n,]),
                                      ifelse(j==2,frac0(int.flo[n,]),
                                             ifelse(j==3,frac0(int.offgr[n,]),
                                                    ifelse(j==4,frac0(int.drain[n,]),
                                                           ifelse(j==5,frac0(int.septage[n,]),
                                                                  ifelse(j==6,frac0(int.produce[n,]),
                                                                         ifelse(j==7,frac0(dw[n,1,]),frac0(dw[n,2,]))))))));
      # unit: CFU/gram, CFU/cm2 and CFU/ml;
      SP3D.dat[(n-1)*8+j,4] <- ifelse(j==1,log10(env.conc.mean(c(n,12))*1000),
                                      ifelse(j==2,log10(env.conc.mean(c(n,27))),
                                             ifelse(j==3,log10(env.conc.mean(c(n,28))),
                                                    ifelse(j==4,log10(env.conc.mean(c(n,53))),
                                                           ifelse(j==5,log10(env.conc.mean(c(n,31))*1000),
                                                                  ifelse(j==6,log10(mean(env.conc.mean(c(n,1)),env.conc.mean(c(n,3)),env.conc.mean(c(n,4)),env.conc.mean(c(n,7)),
                                                                                   env.conc.mean(c(n,8)),env.conc.mean(c(n,63)),env.conc.mean(c(n,83)),env.conc.mean(c(n,84)))),
                                                                         ifelse(j==7,log10(env.conc.mean(c(n,13))),log10(env.conc.mean(c(n,16))))))))));
      SP3D.dat[(n-1)*8+j,5] <- ifelse(j==1,log10(mean(non0(int.dirt[n,]),na.rm=TRUE)),
                                      ifelse(j==2,log10(mean(non0(int.flo[n,]),na.rm=TRUE)),
                                             ifelse(j==3,log10(mean(non0(int.offgr[n,]),na.rm=TRUE)),
                                                    ifelse(j==4,log10(mean(non0(int.drain[n,]),na.rm=TRUE)),
                                                           ifelse(j==5,log10(mean(non0(int.septage[n,]),na.rm=TRUE)),
                                                                  ifelse(j==6,log10(mean(non0(int.produce[n,]),na.rm=TRUE)),
                                                                         ifelse(j==7,log10(mean(non0(dw[n,1,]),na.rm=TRUE)),log10(mean(non0(dw[n,2,]),na.rm=TRUE)))))))));
    }
  }

SP3D.dat<-data.frame(SP3D.dat[,1],SP3D.dat[,2],SP3D.dat[,3],SP3D.dat[,4],SP3D.dat[,5])
colnames(SP3D.dat)<-c("Neighb","Pathway","frac0","env.conc","env.intake");

library(scatterplot3d)
# create column indicating point color
SP3D.dat$pcolor[SP3D.dat$Pathway==1] <- "chocolate3";
SP3D.dat$pcolor[SP3D.dat$Pathway==2] <- "antiquewhite4";
SP3D.dat$pcolor[SP3D.dat$Pathway==3] <- "antiquewhite2";
SP3D.dat$pcolor[SP3D.dat$Pathway==4] <- "black";
SP3D.dat$pcolor[SP3D.dat$Pathway==5] <- "firebrick4";
SP3D.dat$pcolor[SP3D.dat$Pathway==6] <- "green2";
SP3D.dat$pcolor[SP3D.dat$Pathway==7] <- "blue";
SP3D.dat$pcolor[SP3D.dat$Pathway==8] <- "cyan";
SP3D.dat$pshape[SP3D.dat$Neighb==1] <- 15;
SP3D.dat$pshape[SP3D.dat$Neighb==2] <- 17;
SP3D.dat$pshape[SP3D.dat$Neighb==3] <- 18;
SP3D.dat$pshape[SP3D.dat$Neighb==4] <- 19;
with(SP3D.dat, {
  s3d <- scatterplot3d(env.conc, frac0, env.intake,        # x y and z axis
                       color=pcolor, pch=pshape,        # circle color indicates no. of cylinders
                       type="h", lty.hplot=2,     #lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="3-D Scatterplot for Exposure Analysis",
                       xlab="Log 10 Mean Environmental E. coli Concentration (CFU/g or CFU/mL or CFU/cm2)",
                       ylab="Probability of No intake",
                       zlab="Log 10 Mean E.coli Intake for 14 Hours Daytime",xlim=c(-2,10))
  s3d.coords <- s3d$xyz.convert(env.conc, frac0, env.intake)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels="",       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft",      # location and inset
         bty="n", cex=0.8,              # suppress legend box, shrink text 50%
         title="Neighborhood",
         c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
  legend(s3d$xyz.convert(9, 0.6, 18.5),      # location and inset
         bty="n", cex=0.8,              # suppress legend box, shrink text 50%
         title="Pathway",
         c("dirt","floor","offgr","drain",
           "septage","food","tap water","sachet water"), pch=19,
         col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))
})

par(mar=c(5.1,5.1,4.1,2.1))
#scatter plot - X:environment Y:Fract0
plot(SP3D.dat$env.conc[which(SP3D.dat$Neighb==1)],SP3D.dat$frac0[which(SP3D.dat$Neighb==1)],type="p",col=SP3D.dat$pcolor,pch=15,cex=SP3D.dat$env.intake[which(SP3D.dat$Neighb==1)]/3,
     main="Scatter Plot for Exposure Analysis", ylab="Probability of No intake", xlab="Log 10 Mean Environmental E. coli Concentration (CFU/g or CFU/mL or CFU/cm2)")
points(SP3D.dat$env.conc[which(SP3D.dat$Neighb==2)],SP3D.dat$frac0[which(SP3D.dat$Neighb==2)],type="p",col=SP3D.dat$pcolor,pch=17,cex=SP3D.dat$env.intake[which(SP3D.dat$Neighb==2)]/3)
points(SP3D.dat$env.conc[which(SP3D.dat$Neighb==3)],SP3D.dat$frac0[which(SP3D.dat$Neighb==3)],type="p",col=SP3D.dat$pcolor,pch=18,cex=SP3D.dat$env.intake[which(SP3D.dat$Neighb==3)]/3)
points(SP3D.dat$env.conc[which(SP3D.dat$Neighb==4)],SP3D.dat$frac0[which(SP3D.dat$Neighb==4)],type="p",col=SP3D.dat$pcolor,pch=19,cex=SP3D.dat$env.intake[which(SP3D.dat$Neighb==4)]/3)
text(-0.5, 0.2, "The size of points represent\nlog 10 mean E. coli intake\nfor 14 hours daytime (CFU)",cex=0.8)

legend(-2, 0.785,      # location and inset
       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
       title="Neighborhood",
       c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
legend(-2, 0.6,      # location and inset
       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
       title="Pathway",
       c("dirt","floor","offgr","drain",
         "septage","food","tap water","sachet water"), pch=19,
       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))

#scatter plot - X:intake Y:Fract0
plot(SP3D.dat$env.intake[which(SP3D.dat$Neighb==1)],SP3D.dat$frac0[which(SP3D.dat$Neighb==1)],type="p",col=SP3D.dat$pcolor,pch=15,cex=1,xlim=c(0,17),
     main="Scatter Plot for Exposure Analysis", ylab="Probability of No intake", xlab="Log 10 Mean E. coli Intake For 14 Hours (CFU)")
points(SP3D.dat$env.intake[which(SP3D.dat$Neighb==2)],SP3D.dat$frac0[which(SP3D.dat$Neighb==2)],type="p",col=SP3D.dat$pcolor,pch=17,cex=1)
points(SP3D.dat$env.intake[which(SP3D.dat$Neighb==3)],SP3D.dat$frac0[which(SP3D.dat$Neighb==3)],type="p",col=SP3D.dat$pcolor,pch=18,cex=1)
points(SP3D.dat$env.intake[which(SP3D.dat$Neighb==4)],SP3D.dat$frac0[which(SP3D.dat$Neighb==4)],type="p",col=SP3D.dat$pcolor,pch=19,cex=1)

legend(15, 0.985,      # location and inset
       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
       title="Neighborhood",
       c("alajo","bukom","old-fadama","shiabu"), pch=c(15,17,18,19))
legend(15, 0.8,      # location and inset
       bty="n", cex=0.8,              # suppress legend box, shrink text 50%
       title="Pathway",
       c("dirt","floor","offgr","drain",
         "septage","food","tap water","sachet water"), pch=19,
       col=c("chocolate3","antiquewhite4","antiquewhite2","black","firebrick4","green2","blue","cyan"))
