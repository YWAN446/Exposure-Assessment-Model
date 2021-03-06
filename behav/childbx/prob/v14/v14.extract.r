ver <- "v14";
load(paste("./output/",ver,".mcmc.",subset[1],subset[2],".rda",sep=""))

n.chains <- 3;
n.neighb <- 4;
# n.comp <- 6;
n.comp <- 5;
# n.behav <- 7;
n.behav <- 6;
n.global.freq <- 4;
n.global.attr <- 6;
n.iter <- 1000;

a.comp.mc <- array(NA,dim=c(n.neighb,n.comp,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.comp in 1:n.comp) {
    label <- as.character(paste("a.comp[",k.neighb,",",k.comp,"]",sep=""));
    for(k.chains in 1:n.chains) {
      tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
    }
    a.comp.mc[k.neighb,k.comp,] <- c(tmp);
  }
}

a.behav.mc <- array(NA,dim=c(n.neighb,n.comp,n.behav,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.comp in 1:n.comp) {
    for(k.behav in 1:n.behav) {
      label <- as.character(paste("a.behav[",k.neighb,",",k.comp,
                                  ",",k.behav,"]",sep=""));
      for(k.chains in 1:n.chains) {
        tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
      }
      a.behav.mc[k.neighb,k.comp,k.behav,]<-
        c(tmp)*diag(comp.mask[,,k.comp])[k.behav];
    }
  }
}

a.attr.mc <- array(NA,dim=c(n.behav,max.attr,max.attr.lev,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.behav in 1:n.behav) {
  for(k.attr in 1:max.attr) {
    for(k.lev in 1:max.attr.lev) {
      label <- as.character(paste("a.attr[",k.behav,",",k.attr,",",k.lev,"]",
                                  sep=""));
      for(k.chains in 1:n.chains) {
        tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
      }
      a.attr.mc[k.behav,k.attr,k.lev,]<-
        c(tmp)*diag(attr.mask[,,ind.mask[k.behav,k.attr]])[k.lev];
    }
  }
}

a.gattr.mc <- array(NA,dim=c(n.global.attr,3,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.gattr in 1:n.global.attr) {
  for(k.lev in 1:3) {
    label <- as.character(paste("a.gattr[",k.gattr,",",k.lev,"]",
                                sep=""));
    for(k.chains in 1:n.chains) {
      tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
    }
    a.gattr.mc[k.gattr,k.lev,] <-
      c(tmp)*diag(global.attr.mask[,,k.gattr])[k.lev];
  }
}

r1.mc <- array(NA,dim=c(n.behav,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.comp in 1:n.comp) {
  for(k.behav in 1:n.behav) {
    label <- as.character(paste("r1[",k.behav,"]",sep=""));
    for(k.chains in 1:n.chains) {
      tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
    }
    r1.mc[k.behav,]<- c(tmp);
  }
}

lambda1.mc <- array(NA,dim=c(n.neighb,n.behav,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.comp in 1:n.comp) {
    for(k.behav in 1:n.behav) {
      label <- as.character(paste("lambda1[",k.neighb,",",
                                  k.behav,"]",sep=""));
      for(k.chains in 1:n.chains) {
        tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
      }
      lambda1.mc[k.neighb,k.behav,]<- c(tmp);
    }
  }
}

r2.mc <- array(NA,dim=c(n.comp,n.behav,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.comp in 1:n.comp) {
  for(k.behav in 1:n.behav) {
    label <- as.character(paste("r2[",k.comp,",",k.behav,"]",sep=""));
    for(k.chains in 1:n.chains) {
      tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
    }
    r2.mc[k.comp,k.behav,]<- c(tmp)*diag(comp.mask[,,k.comp])[k.behav];
  }
}

lambda2.mc <- array(NA,dim=c(n.neighb,n.comp,n.behav,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.neighb in 1:n.neighb) {
  for(k.comp in 1:n.comp) {
    for(k.behav in 1:n.behav) {
      label <- as.character(paste("lambda2[",k.neighb,",",k.comp,
                                  ",",k.behav,"]",sep=""));
      for(k.chains in 1:n.chains) {
        tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
      }
      lambda2.mc[k.neighb,k.comp,k.behav,]<-
        c(tmp)*diag(comp.mask[,,k.comp])[k.behav];
    }
  }
}

r.global.mc <- array(NA,dim=c(n.global.freq,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.freq in 1:n.global.freq) {
  label <- as.character(paste("r[",k.freq,"]",sep=""));
  for(k.chains in 1:n.chains) {
    tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
  }
  r.global.mc[k.freq,] <- exp(c(tmp));
}

lambda.global.mc <- array(NA,dim=c(n.global.freq,n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.freq in 1:n.global.freq) {
  label <- as.character(paste("lambda[",k.freq,"]",sep=""));
  for(k.chains in 1:n.chains) {
    tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
  }
  lambda.global.mc[k.freq,] <- exp(c(tmp));
}

# dump(c("a.comp.mc","a.behav.mc","a.attr.mc","a.gattr.mc",
#        "r1.mc","lambda1.mc","r2.mc","lambda2.mc",
#        "r.global.mc","lambda.global.mc"),
#      paste(ver,"mcmc.txt",sep="."));

prob.parameters <- list("a.comp.mc"=a.comp.mc,"a.behav.mc"=a.behav.mc,
     "a.attr.mc"=a.attr.mc,"a.gattr.mc"=a.gattr.mc,
     "r1.mc"=r1.mc,"lambda1.mc"=lambda1.mc,
     "r2.mc"=r2.mc,"lambda2.mc"=lambda2.mc,
     "r.global.mc"=r.global.mc,"lambda.global.mc"=lambda.global.mc);

# save(prob.parameters,
#      file=paste("./output/",ver,".mcmc.",subset[1],subset[2],".rda",sep=""),
#      ascii=TRUE);

# address compartment vector for iteration i:
#   a.comp.mc[k.neighb,k.comp,i]
# address behaviour matrix for iteration i:
#  a.behav.mc[k.neighb,k.comp,k.behav,i]
# address attribute matrix for behaviour k in iteration i:
#   a.attr.mc[k.neighb,k.behav,,,i]
