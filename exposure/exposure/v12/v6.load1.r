setwd("~/stat/sanipath/exposure/exposure/v6")
version <- "v6";
main.version <- version;
here <- getwd();

fileenv <- paste(version,".conc.r",sep="");
source(fileenv);

basedir <- "~/stat/sanipath/behav/childbx/rate/";
# version <- "v12";
version <- "v13";
subset <- c("hh","1");
k.age <- ifelse(subset[2]=="0",1,ifelse(subset[2]=="1",2,3))
# subset <- "hh";
# filemc <- paste(basedir,version,"/",version,"-mcmc-",subset,".rda",sep="");
setwd(paste(basedir,version,"/",sep=""));
filemc <- paste(version,".extract",".r",sep="");
cat("loading behavioural parameter estimates ",version," ",subset,"\n");
# load(filemc);
source(filemc);
hh.r <- rate.parameters$r;
hh.lambda <- rate.parameters$lambda.mc;
# subset <- "hh";
# filemc <- paste(basedir,version,"/",version,"-mcmc-",subset,".rda",sep="");
# cat("loading behavioural parameter estimates ",version," ",subset,"\n");
# load(filemc);
# ns.r <- rate.parameters$r;
# ns.lambda <- rate.parameters$lambda.mc;
rate.parameters <- NULL;
setwd(here);

basedir <- "~/stat/sanipath/behav/childbx/simul/";
version <- "v15";
filesim <- paste(basedir,version,"/",version,".simul.r",sep="");
source(filesim);

basedir <- "~/stat/sanipath/exposure/dw/";
version <- "v1";
filesim <- paste(basedir,version,"/",version,".simul.r",sep="");
source(filesim);

basedir <- "~/stat/sanipath/exposure/food/";
version <- "v3";
filesim <- paste(basedir,version,"/",version,".simul.r",sep="");
source(filesim);

basedir <- "~/stat/sanipath/exposure/hand/";
version <- "v2";
filesim <- paste(basedir,version,"/",version,".hand.r",sep="");
source(filesim);

version <- main.version;

# fileaux <- paste(version,".aux.r",sep="");
# source(fileaux);
# filelnk <- paste(version,".link.r",sep="");
# source(filelnk);
# filefnc <- paste(version,".expfunc.r",sep="");
# source(filefnc);
filesim <- paste(version,".expsim.r",sep="");
source(filesim);
