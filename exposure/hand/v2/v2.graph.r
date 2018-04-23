graph.sim <- function(sim.vec,color){
  x <- (1:length(sim.vec))-1;
  points(x,sim.vec,col=color);
  lines(x,sim.vec,col=color);
}

pdf("./simulation.pdf")
# par(mfrow=c(2,1));

sim.res <- rep(NA,1000);
for(k.sim in 1:1000){
  sim.res[k.sim] <- sim.hand(1e3,0,c(0.2,8),c(2,2),10);
}
hist(log10(sim.res),xlab="# particles");

dev.off()
