library('rjags');
setwd("~/stat/sanipath/exposure/food/v3");
version <- "v3";
nburn<-10000;
niter<-100000;
thin<-100;

tomonitor <- c("pi.prod", "pi.vend");

# file names
filedat <- paste(version,".data.r",sep="");
filemod <- paste(version,".model.jags",sep="");
fileres <- paste("output/",version,".res.pdf",sep="");
filetxt <- paste("output/",version,".res.txt",sep="");
fileext <- paste(version,".extract.r",sep="");

# load data
source(filedat);

### POSTERIOR analysis
cat("<<MCMC Sampling>>\n");
model.jags <- jags.model(file=filemod,data=data,#inits=data.init,
                         n.chains=3,n.adapt=nburn,quiet=FALSE);
mcmc.post<-coda.samples(model.jags,tomonitor,n.iter=niter,thin=thin);

save(mcmc.post,file=paste("output/",version,".post.rda",sep=""),ascii=TRUE);

cat("<<< Store results >>>\n");
sink(filetxt);
print(summary(mcmc.post));
sink();

cat("<<< Graph results >>>\n");
pdf(fileres);
plot(mcmc.post,trace=TRUE,density=FALSE);
plot(mcmc.post,trace=FALSE,density=TRUE);
dev.off();
