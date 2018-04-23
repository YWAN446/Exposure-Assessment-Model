library(lattice);

# rate.mn <- array(NA,dim=c(n.neighb,n.comp-1,n.behav-1));
# for(k.neighb in 1:n.neighb){
#   for(k.comp in 1:n.comp-1){
#     for(k.behav in 1:n.behav-1){
#       rate.mn[k.neighb,k.comp,k.behav] <-
#         gamma(1+1/r)*mean(lambda.mc[k.neighb,k.comp,k.behav,]);
#     }
#   }
# }

# rate.mn[rate.mn>10] <- 0;

# zlevs <- seq(0.0,0.02,0.0005);

# trellis.device(color=FALSE,pdf,file=paste(neighbourhoods[1],"pdf",sep="."));
plot.matrix <- function(k.neighb,dmat,maxlev){
  if(!is.na(maxlev)){
    zlevs <- seq(0.0,maxlev,maxlev/50);
  }else{
    zlevs <- seq(0.0,max(dmat),max(dmat)/50);
  }
  levelplot(dmat,at=zlevs,
            main=neighbourhoods[k.neighb],
            xlab=compartments[1:5],ylab=behaviours[1:6],
            pretty=TRUE,interpolate=TRUE);
}
