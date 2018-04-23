version <- "v10";
filensmp<-paste(version,"nsmp","r",sep=".");
source(filensmp)
fileda <- paste(version,"data","r",sep=".");
source(fileda);
filemc <- paste(version,"mcmc","rda",sep=".");
filegr <- paste(version,"graph","pdf",sep=".");
load(filemc);

ec.logr <- indic.parameters$ec.logr;
ec.loglambda <- indic.parameters$ec.loglambda;
en.logr <- indic.parameters$en.logr;
en.loglambda <- indic.parameters$en.loglambda;
cm.logr <- indic.parameters$cm.logr;
cm.loglambda <- indic.parameters$cm.loglambda;

ticklab <- c(0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,1e2,2e2,5e2,1e3,
             2e3,5e3,1e4,2e4,5e4,1e5,2e5,5e5,1e6,2e6,5e6,1e7,2e7,5e7,1e8,
             2e8,5e8,1e9,2e9,5e9,1e10);
tickpos <- log(ticklab);

num.mc <- ncol(ec.logr);
# num.comb <- nrow(ec.logr);

for(k.smptp in 1:length(offs)) ind.tp <- c(ind.tp,rep(k.smptp,offs[k.smptp]));
ngh <- c(levels(neigh),NA);
n.comb <- c();
for(k.neigh in 1:length(ngh)){
  if(is.na(ngh[k.neigh]))  ngh.comb <- which(is.na(neigh));
  if(!is.na(ngh[k.neigh])) ngh.comb <- which(neigh==ngh[k.neigh]);
  for(k.comb in 1:nrow(all.comb)){
    smp.comb <- ind.env.smp[which(ind.env.smp[,2]==all.comb[k.comb,1] &
                      ind.env.smp[,3]==all.comb[k.comb,2]),4];
    ncmb <- length(intersect(smp.comb,ngh.comb));
    if(ncmb!=0){
      n.comb <- rbind(n.comb,c(k.neigh,k.comb));
    }
  }
}

mainlabel <- function(k.neighb,k.comb) {
  return(paste(neighb.label[k.neighb]," ",
               env.label[all.comb[k.comb,1]],"\n",
               smp.label[all.comb[k.comb,2]],
               sep=""));
}

gquant <- function(logconc,q,vec.logr,vec.loglambda) {
  tmp <- 10^logconc * log(10)*
         dgamma(10^logconc,shape=exp(vec.logr),scale=exp(vec.loglambda));
  return(quantile(tmp,q));
}

gqvec <- function(logcvec,q,vec.logr,vec.loglambda) {
  tmp <- rep(NA,length(logcvec));
  for(k in 1:length(logcvec)) {
    tmp[k] <- gquant(logcvec[k],q,vec.logr,vec.loglambda);
  }
  return(tmp);
}

plotgammadens <- function(vec.logr,vec.loglambda,mainlab) {
  logc <- seq(-5,12,0.1);
  plot(logc,gqvec(logc,0.975,vec.logr,vec.loglambda),"l",xaxt="n",
       xlab="concentration",ylab="density",main=mainlab);
  axis(side=1,at=tickpos,labels=ticklab);
  lines(logc,gqvec(logc,0.5,vec.logr,vec.loglambda));
  lines(logc,gqvec(logc,0.025,vec.logr,vec.loglambda));
}

pdf(filegr);
# par(mfrow=c(num.env.tp,num.smp.tp));

par(mfrow=c(3,3));
for(k.comb in 1:nrow(n.comb)){
  plotgammadens(ec.logr[n.comb[k.comb,2],],
                ec.loglambda[n.comb[k.comb,1],n.comb[k.comb,2],],
                mainlabel(n.comb[k.comb,1],n.comb[k.comb,2]));
}

dev.off();
