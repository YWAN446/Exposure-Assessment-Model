library(MCMCpack)

graphdir <- "./output/graphs/";

ver <- "v14";
# load(paste("./output/",ver,".mcmc.",subset[1],subset[2],".rda",sep=""));
a.comp.mc <- prob.parameters$a.comp.mc;
a.behav.mc <- prob.parameters$a.behav.mc;
a.attr.mc <- prob.parameters$a.attr.mc;
a.gattr.mc <- prob.parameters$a.gattr.mc;
r1.mc <- prob.parameters$r1.mc;
lambda1.mc <- prob.parameters$lambda1.mc;
r2.mc <- prob.parameters$r2.mc;
lambda2.mc <- prob.parameters$lambda2.mc;
r.global.mc <- prob.parameters$r.global.mc;
lambda.global.mc <- prob.parameters$lambda.global.mc;

n.neighb <- 4;
# n.comp <- 6;
n.comp <- 5;
# n.behav <-7;
n.behav <-6;
n.global.freq <- 4;
n.iter <- 3000;

pvec.comp.mc <- array(NA,dim=c(n.neighb,n.comp,n.iter));
pvec.behav.mc <- array(NA,dim=c(n.neighb,n.comp,n.behav,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.iter in 1: n.iter) {
    pvec.comp.mc[k.neighb,,k.iter] <-
      rdirichlet(1,a.comp.mc[k.neighb,,k.iter]);
  }
  for(k.comp in 1:n.comp) {
    for(k.iter in 1: n.iter) {
      pvec.behav.mc[k.neighb,k.comp,,k.iter] <-
        rdirichlet(1,a.behav.mc[k.neighb,k.comp,,k.iter]);
    }
  }
}

pvec.attr.mc <- array(NA,dim=c(num.attr,max.attr.lev,n.iter));
for(k.iter in 1:n.iter) {
  pvec.attr.mc[1,,k.iter] <- rdirichlet(1,a.attr.mc[1,1,,k.iter]);
  pvec.attr.mc[2,,k.iter] <- rdirichlet(1,a.attr.mc[1,2,,k.iter]);
  pvec.attr.mc[3,,k.iter] <- rdirichlet(1,a.attr.mc[3,1,,k.iter]);
  pvec.attr.mc[4,,k.iter] <- rdirichlet(1,a.attr.mc[4,1,,k.iter]);
  pvec.attr.mc[5,,k.iter] <- rdirichlet(1,a.attr.mc[5,1,,k.iter]);
  pvec.attr.mc[6,,k.iter] <- rdirichlet(1,a.attr.mc[5,2,,k.iter]);
  pvec.attr.mc[7,,k.iter] <- rdirichlet(1,a.attr.mc[5,3,,k.iter]);
  pvec.attr.mc[8,,k.iter] <- rdirichlet(1,a.attr.mc[6,1,,k.iter]);
  pvec.attr.mc[9,,k.iter] <- rdirichlet(1,a.attr.mc[6,2,,k.iter]);
  pvec.attr.mc[10,,k.iter] <- rdirichlet(1,a.attr.mc[6,3,,k.iter]);
}

pvec.gattr.mc <- array(NA,dim=c(n.global.attr,3,n.iter));
for(k.iter in 1:n.iter) {
  for(k.gattr in 1:n.global.attr) {
    pvec.gattr.mc[k.gattr,,k.iter] <- rdirichlet(1,a.gattr.mc[k.gattr,,k.iter]);
  }
}

time.mc <- array(NA,dim=c(n.neighb,n.comp,n.behav,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.behav in 1:n.behav) {
    for(k.iter in 1:n.iter) {
      # r0 <- rgamma(n=1,shape=r1.mc[k.behav,k.iter],
      #              rate=lambda1.mc[k.neighb,k.behav,k.iter]);
      r0 <- r1.mc[k.behav,k.iter]/
              lambda1.mc[k.neighb,k.behav,k.iter];
      for(k.comp in 1:n.comp) {
        # lambda0 <- rgamma(n=1,shape=r2.mc[k.comp,k.behav,k.iter],
        #                   rate=lambda2.mc[k.neighb,k.comp,k.behav,k.iter]);
        lambda0 <- r2.mc[k.comp,k.behav,k.iter]/
                     lambda2.mc[k.neighb,k.comp,k.behav,k.iter];
        if(!is.nan(lambda0)) {
          time.mc[k.neighb,k.comp,k.behav,k.iter] <-
            rgamma(n=1,shape=r0,rate=lambda0);
          # r0/lambda0;
        } else {
          if(is.na(time.mc[k.neighb,k.comp,k.behav,k.iter])) {
            time.mc[k.neighb,k.comp,k.behav,k.iter] <- 0;
          }
        }
      }
    }
  }
}

pvec.mc <- array(0,dim=c(n.neighb,n.behav,n.iter));
tvec.mc <- array(0,dim=c(n.neighb,n.behav,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.comp in 1:n.comp) {
    pvec.mc[k.neighb,,] <- pvec.mc[k.neighb,,] +
      pvec.behav.mc[k.neighb,k.comp,,]*pvec.comp.mc[k.neighb,k.comp,];
    tvec.mc[k.neighb,,] <- tvec.mc[k.neighb,,] +
      pvec.behav.mc[k.neighb,k.comp,,]*pvec.comp.mc[k.neighb,k.comp,]*
        time.mc[k.neighb,k.comp,,];
  }
}

global.freq.mc <- array(NA,dim=c(n.global.freq,n.iter));
for(k.global.freq in 1:n.global.freq) {
  for(k.iter in 1:n.iter) {
    global.freq.mc[k.global.freq,k.iter] <-
      rpois(n=1,lambda=rgamma(n=1,
                  shape=r.global.mc[k.global.freq,k.iter],
                  rate=lambda.global.mc[k.global.freq,k.iter]));
  }
}

pdf(paste(graphdir,ver,".comp.",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(n.neighb,1),mar=c(2,3,2,1));
for(k.neighb in 1:n.neighb) {
  boxplot(t(pvec.comp.mc[k.neighb,,]),xaxt="n",main=neighbourhoods[k.neighb]);
  axis(side=1,at=1:5,labels=compartments);
}
dev.off();

for(k.neighb in 1:n.neighb) {
  pdf(paste(graphdir,ver,".behav-cond.",neighbourhoods[k.neighb],
            subset[1],subset[2],".pdf",sep=""));
  par(mfrow=c(n.behav,1),mar=c(2,3,2,1));
  for(k.comp in 1:n.comp) {
    boxplot(t(pvec.behav.mc[k.neighb,k.comp,,]),
            xaxt="n",main=compartments[k.comp]);
    axis(side=1,at=1:6,labels=behaviours);
  }
  dev.off();
}

for(k.neighb in 1:n.neighb) {
  pdf(paste(graphdir,ver,".behav-uncond.",neighbourhoods[k.neighb],
            subset[1],subset[2],".pdf",sep=""));
  par(mfrow=c(2,1),mar=c(2,3,2,1));
  boxplot(t(pvec.mc[k.neighb,,]),xaxt="n",main="probabilities");
  axis(side=1,at=1:6,labels=behaviours);
  boxplot(t(tvec.mc[k.neighb,,]),log="y",xaxt="n",main="time",ylim=c(0.1,500));
  axis(side=1,at=1:6,labels=behaviours);
  dev.off();
}

for(k.neighb in 1:n.neighb) {
  pdf(paste(graphdir,ver,".behav-time.",neighbourhoods[k.neighb],
            subset[1],subset[2],".pdf",sep=""));
  par(mfrow=c(5,1),mar=c(2,3,2,1));
  for(k.comp in 1:n.comp) {
    boxplot(t(time.mc[k.neighb,k.comp,,]),
            log="y",xaxt="n",main=compartments[k.comp],ylim=c(1,500));
    axis(side=1,at=1:6,labels=behaviours);
  }
  dev.off();
}

pdf(paste(graphdir,ver,".attr-beh-1-4.",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(4,1),mar=c(2,3,2,1));
boxplot(t(pvec.attr.mc[1,,]),xaxt="n",
        main=paste(behaviours[1]," ",attributes[1]));
axis(side=1,at=1:5,labels=attribute.levels[1,]);
boxplot(t(pvec.attr.mc[2,,]),xaxt="n",
        main=paste(behaviours[1]," ",attributes[2]));
axis(side=1,at=1:5,labels=attribute.levels[2,]);
boxplot(t(pvec.attr.mc[3,,]),xaxt="n",
        main=paste(behaviours[3]," ",attributes[3]));
axis(side=1,at=1:5,labels=attribute.levels[3,]);
boxplot(t(pvec.attr.mc[4,,]),xaxt="n",
        main=paste(behaviours[4]," ",attributes[4]));
axis(side=1,at=1:5,labels=attribute.levels[4,]);
dev.off();

pdf(paste(graphdir,ver,".attr-beh-5-6.",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(6,1),mar=c(2,3,2,1));
boxplot(t(pvec.attr.mc[5,,]),xaxt="n",
        main=paste(behaviours[5]," ",attributes[5]));
axis(side=1,at=1:5,labels=attribute.levels[5,]);
boxplot(t(pvec.attr.mc[6,,]),xaxt="n",
        main=paste(behaviours[5]," ",attributes[6]));
axis(side=1,at=1:5,labels=attribute.levels[6,]);
boxplot(t(pvec.attr.mc[7,,]),xaxt="n",
        main=paste(behaviours[5]," ",attributes[7]));
axis(side=1,at=1:5,labels=attribute.levels[7,]);
boxplot(t(pvec.attr.mc[8,,]),xaxt="n",
        main=paste(behaviours[6]," ",attributes[8]));
axis(side=1,at=1:5,labels=attribute.levels[8,]);
boxplot(t(pvec.attr.mc[9,,]),xaxt="n",
        main=paste(behaviours[6]," ",attributes[9]));
axis(side=1,at=1:5,labels=attribute.levels[9,]);
boxplot(t(pvec.attr.mc[10,,]),xaxt="n",
        main=paste(behaviours[6]," ",attributes[10]));
axis(side=1,at=1:5,labels=attribute.levels[10,]);
dev.off();

pdf(paste(graphdir,ver,".global-attr.",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(6,1),mar=c(2,3,2,1));
boxplot(t(pvec.gattr.mc[1,,]),xaxt="n",
        main=paste(global.attributes[1]));
axis(side=1,at=1:3,labels=global.attribute.levels[1,]);
boxplot(t(pvec.gattr.mc[2,,]),xaxt="n",
        main=paste(global.attributes[2]));
axis(side=1,at=1:3,labels=global.attribute.levels[2,]);
boxplot(t(pvec.gattr.mc[3,,]),xaxt="n",
        main=paste(global.attributes[3]));
axis(side=1,at=1:3,labels=global.attribute.levels[3,]);
boxplot(t(pvec.gattr.mc[4,,]),xaxt="n",
        main=paste(global.attributes[4]));
axis(side=1,at=1:3,labels=global.attribute.levels[4,]);
boxplot(t(pvec.gattr.mc[5,,]),xaxt="n",
        main=paste(global.attributes[5]));
axis(side=1,at=1:3,labels=global.attribute.levels[5,]);
boxplot(t(pvec.gattr.mc[6,,]),xaxt="n",
        main=paste(global.attributes[6]));
axis(side=1,at=1:3,labels=global.attribute.levels[6,]);
dev.off();

pdf(paste(graphdir,ver,".behav-tally.",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(1,1),mar=c(2,3,2,1));
boxplot(t(global.freq.mc),ylim=c(0,20),
        xaxt="n",main="frequency of contact with");
axis(side=1,at=1:4,labels=behav.tally);
dev.off();
