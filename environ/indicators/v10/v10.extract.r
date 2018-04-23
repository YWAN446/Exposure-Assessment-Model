version <- "v10";

n.chains <- 3
n.iter <- 1000;

load(paste(version,"mcmc","post","rda",sep="."));
filemc <- paste(version,"mcmc","rda",sep=".");

offset <- 0.02; # offset for the shape parameter for the gamma distributions

retrieve1 <- function(varstring,mcframe,nit,nch,kcomb){
  var <- array(NA,dim=c(nch,nit));
  varstr <- paste(varstring,"[",kcomb,"]",sep="");
  for(k.ch in 1:nch){
    var[k.ch,] <- as.vector(mcframe[[k.ch]][,varstr]);
  }
  return(c(var));
}

retrieve2 <- function(varstring,mcframe,nit,nch,ncomb){
  var <- array(NA,dim=c(ncomb,nch*nit));
  for(k.comb in 1:ncomb){
    var[k.comb,] <- retrieve1(varstring,mcframe,nit,nch,k.comb);
  }
  return(var);
}

retrieve3 <- function(varstring,mcframe,nit,nch,kcomb,kneighb){
  var <- array(NA,dim=c(nch,nit));
  varstr <- paste(varstring,"[",kneighb,",",kcomb,"]",sep="");
  for(k.ch in 1:nch){
    var[k.ch,] <- as.vector(mcframe[[k.ch]][,varstr]);
  }
  return(c(var));
}

retrieve4 <- function(varstring,mcframe,nit,nch,ncomb,nneighb){
  var <- array(NA,dim=c(nneighb,ncomb,nch*nit));
  for(k.neighb in 1:nneighb){
    for(k.comb in 1:ncomb){
      var[k.neighb,k.comb,] <-
        retrieve3(varstring,mcframe,nit,nch,k.comb,k.neighb);
    }
  }
  return(var);
}

ec.logr <- retrieve2("ec.logr",mcmc.post,n.iter,n.chains,num.comb)+offset;
ec.loglambda <-
  retrieve4("ec.loglambda",mcmc.post,n.iter,n.chains,num.comb,n.neighb);
en.logr <- retrieve2("en.logr",mcmc.post,n.iter,n.chains,num.comb)+offset;
en.loglambda <-
  retrieve4("en.loglambda",mcmc.post,n.iter,n.chains,num.comb,n.neighb);
cm.logr <- retrieve2("cm.logr",mcmc.post,n.iter,n.chains,num.comb)+offset;
cm.loglambda <-
  retrieve4("cm.loglambda",mcmc.post,n.iter,n.chains,num.comb,n.neighb);

indic.parameters <- list("ec.logr"=ec.logr,"ec.loglambda"=ec.loglambda,
                         "en.logr"=en.logr,"en.loglambda"=en.loglambda,
                         "cm.logr"=cm.logr,"cm.loglambda"=cm.loglambda);

save(indic.parameters,file=filemc,ascii=TRUE);
