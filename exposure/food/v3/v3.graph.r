library(foreign)
# library(fitdistrplus)
# library(Hmisc)
# library(animation)
# library(MASS)

#plot(density(produce.prob[!is.na(produce.prob)]))
#plot(density(produce.freq[!is.na(produce.freq)]))

#produce.f1 <- fitdist(produce.prob[!is.na(produce.prob)], "beta", "mle");
#plot(produce.f1)
#produce.f2 <- fitdist((1-produce.prob[!is.na(produce.prob)]), "norm", "mle");
#plot(produce.f2)
#produce.f3 <- fitdist((1-produce.prob[!is.na(produce.prob)]), "unif", "mle");
#plot(produce.f3)
#produce.f4 <- fitdist((1-produce.prob[!is.na(produce.prob)]), "lnorm", "mle");
#plot(produce.f4)

   # determine best fit-- geo or unif
#cdfcomp(list(produce.f4,produce.f2,produce.f3), legendtext=c("LNorm","Norm","Uniform"))
#denscomp(list(produce.f4,produce.f2,produce.f3), legendtext=c("LNorm","Norm","Uniform"))
#qqcomp(list(produce.f4,produce.f2,produce.f3), legendtext=c("LNorm","Norm","Uniform"))
#ppcomp(list(produce.f4,produce.f2,produce.f3), legendtext=c("LNorm","Norm","Uniform"))
#gofstat(list(produce.f4,produce.f2,produce.f3), fitnames=c("LNorm","Norm","Uniform"))

#plot(density(vend.prob[!is.na(vend.prob)]))
#plot(density(vend.freq[!is.na(vend.freq)]))

#vend.f1 <- fitdist(vend.prob[!is.na(vend.prob)], "norm", "mle");
#plot(vend.f1)
#vend.f2 <- fitdist(vend.prob[!is.na(vend.prob)], "beta", "mle");
#plot(vend.f2)
#vend.f3 <- fitdist(vend.prob[!is.na(vend.prob)], "unif", "mle");
#plot(vend.f3)

#cdfcomp(list(vend.f1,vend.f2,vend.f3), legendtext=c("Norm","Beta","Uniform"))
#denscomp(list(vend.f1,vend.f2,vend.f3), legendtext=c("Norm","Beta","Uniform"))
#qqcomp(list(vend.f1,vend.f2,vend.f3), legendtext=c("Norm","Beta","Uniform"))
#ppcomp(list(vend.f1,vend.f2,vend.f3), legendtext=c("Norm","Beta","Uniform"))
#gofstat(list(vend.f1,vend.f2,vend.f3), fitnames=c("Norm","Beta","Uniform"))
