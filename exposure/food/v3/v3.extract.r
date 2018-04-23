version <- "v3"

n.chains <- 3
n.iter <- 1000
n.neighb <- 4; # 1=alajo; 2=bukom; 3=old fadama; 4=shiabu
n.fd <- 2; # 1=produce and 2=vended food
n.levs <- 4 # 1="everyday"; 2="few times/wk"; 3="once/wk"; 4="never"

basedir <- "~/stat/sanipath/exposure/food/"
mcmcpost <- paste(basedir,version,"/output/",version,".post.rda",sep="");
mcmcpars <- paste(basedir,version,"/output/",version,".mcmc.rda",sep="");
load(mcmcpost);

extr.mc <- function(varstring) {
  tmp <- array(NA,dim=c(n.chains,n.iter));
  for(k in 1:n.chains) {
    tmp[k,] <- as.vector(mcmc.post[[k]][,varstring]);
  }
  return(c(tmp));
}

pi.prod <- array(NA,dim=c(n.neighb,n.levs,n.chains*n.iter));
pi.vend <- array(NA,dim=c(n.neighb,n.levs,n.chains*n.iter));
for(k.neighb in 1:n.neighb){
  for(k.lev in 1:n.levs){
    pi.prod[k.neighb,k.lev,] <-
      extr.mc(paste("pi.prod[",k.neighb,",",k.lev,"]",sep=""));
    pi.vend[k.neighb,k.lev,] <-
      extr.mc(paste("pi.vend[",k.neighb,",",k.lev,"]",sep=""));
  }
}

fdpars <- list("pi.prod"=pi.prod,"pi.vend"=pi.vend);
save(fdpars,file=mcmcpars,ascii=TRUE);
