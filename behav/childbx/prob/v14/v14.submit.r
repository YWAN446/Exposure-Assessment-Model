library(rjags);

ver <- "v14";
nburn <- 10000;
niter <- 100000;
thin <- 100;
# nburn <- 100;
# niter <- 100;
# thin <- 1;
tomonitor <- c("a.comp","a.behav","a.attr",
               "r1","lambda1","r2","lambda2",
               "a.gattr",
               "r","lambda");
# tomonitor <- c("p.behav[1:5,1:25]","rate[1:5,1:5]");

# choose subset: subset <- c(domain,agecat)
# where domain = "hh" or "ns"
# and agecat = 0, 1, or 2 (years)

cat("<<< MCMC Sampling >>>\n");
model.jags <- jags.model(file=paste(ver,"model.jags",sep="."),
                         data=sel.data,inits=sel.init,
                         n.chains=3,n.adapt=nburn,quiet=FALSE);
mcmc.post <- coda.samples(model.jags,tomonitor,n.iter=niter,thin=thin);

# Extract mcmc values of the important parameters
# and save them in a text file
# source(fileex);

save(mcmc.post,
     file=paste("./output/",ver,".mcmc.",subset[1],subset[2],".rda",sep=""),
     ascii=TRUE);

cat("<<< Storing the results >>>\n");
sink(paste("./output/",ver,".results.",subset[1],subset[2],".txt",sep=""));
print(summary(mcmc.post));
sink();

# cat("<<< Graphing results >>>\n");
# pdf(paste("./output/",ver,".results.",subset[1],subset[2],".pdf",sep=""));

# plot(mcmc.post,trace=TRUE,density=FALSE);
# plot(mcmc.post,trace=FALSE,density=TRUE);
#
# autocorr(mcmcpos);
# autocorr.plot(mcmcpos);
# crosscorr(mcmcpos);
# crosscorr.plot(mcmcpos);
# geweke.diag(mcmcpos);

# summary(mcmc.post);

# dev.off();
