library(MCMCpack)

ver <- "v10";
#source(paste(ver,"mcmc.txt",sep="."));
load(paste(ver,"mcmc.rda",sep="."));

n.neighb <- 4;
# n.comp <- 6;
n.comp <- 5;
# n.behav <-7;
n.behav <-6;
n.global.freq <- 4;
n.iter <- 3000;

r <- rate.parameters$r;
lambda.mc <- rate.parameters$lambda.mc;
