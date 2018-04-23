version <- "v1"

basedir <- "~/stat/sanipath/exposure/dw/";
filemc <- paste(basedir,version,"/output/",version,".mcmc.rda",sep="");
cat("loading drinking water intake parameter estimates ",version,"\n");
load(filemc);

cup <- 237;    # ml
sachet <- 500; # ml

is.tap <- function(k.neighb){
  psac <- rbeta(1,freqs.neigh[k.neighb,1]+1,freqs.neigh[k.neighb,2]+1);
  if(rbinom(n=1,size=1,prob=psac)==0) return(TRUE);
  return(FALSE);
}

dw.tapsmpl <- function(k.age){
  #k.iter <- sample((1:nrow(dwpars$mu.tap)),size=1); #why use random sample from parameter simulation? why not use bayesian estimator of parameters?
  intk <- rnorm(n=1,mean=mean(dwpars$mu.tap[,k.age]),
                sd=sqrt(1/mean(dwpars$tau.tap[,k.age])));
  #intk <- ifelse(intk < 8, intk, 8); #Don't have to restrict this;
  return(exp(intk));
}

dw.sacsmpl <- function(k.age){
  #k.iter <- sample((1:nrow(dwpars$mu.sac)),size=1);
  intk <- rnorm(n=1,mean=mean(dwpars$mu.sac[,k.age]),
                    sd=sqrt(1/mean(dwpars$tau.sac[,k.age])));
  #intk <- ifelse(intk < 8, intk, 8); #Don't have to restrict this;
  return(exp(intk));
}
