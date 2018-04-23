library(rjags)
setwd("~/stat/sanipath/environ/indicators/v10/");
".RNG.state" <- c(19900, 14957, 25769)

# constant to be fitted according to the goal
nburn <- 1000;
niter <- 1000;
thin <- 1;
calc <- paste(nburn,niter,thin,sep="|");
version <- "v10";
tomonitor <- c("ec.logr","ec.loglambda",
               "en.logr","en.loglambda",
               "cm.logr","cm.loglambda");
# tomonitor <- c("conc")

# file names
filedat <- paste(version,"data","r",sep=".");
filemod <- paste(version,"model","jags",sep=".");
fileres <- paste(version,"res","pdf",sep=".");
filetxt <- paste(version,"res","txt",sep=".");
fileext <- paste(version,"extract","r",sep=".");

# load data
source(filedat);

### POSTERIOR analysis

cat("<<< MCMC Sampling >>>\n");
model.jags <- jags.model(file=filemod,data=cntdata,n.chains=3,n.adapt=nburn,
                       inits=cntinit,quiet=FALSE);
update(model.jags,n.burn=nburn, progress.bar="text");
mcmc.post <- coda.samples(model.jags,tomonitor,n.iter=niter,thin=thin);

save(mcmc.post,file=paste(version,"mcmc.post.rda",sep="."),ascii=TRUE);

# Extract mcmc values of the important parameters
# and save them in a text file
# source(fileexec);

cat("<<< Store results >>>\n");
sink(filetxt);
print(summary(mcmc.post));
sink();

source(fileext)

# cat("<<< Graph results >>>\n");
# pdf(fileres);
# plot(mcmcpos,trace=TRUE,density=FALSE);
# plot(mcmcpos,trace=FALSE,density=TRUE);
# summary(mcmcpos);

# autocorr(mcmcpos);
# autocorr.plot(mcmcpos);
# crosscorr(mcmcpos);
# crosscorr.plot(mcmcpos);
# geweke.diag(mcmcpos);

# dev.off();
