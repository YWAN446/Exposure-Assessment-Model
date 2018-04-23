library(lattice);

neighbourhoods <- c("alajo","bukom","old-fadama","shiabu");
labels <- c("dirt-ply\nhand","fl-ply\nhand","offgr\nhand","offgr\nsleep",
            "dirt\neat","floor\neat","offgr\neat","SWATA\neat",
            "tap\ndrink","sachet\ndrink");

pdf(paste("./output/","exposure","-no-nb",subset[1],subset[2],".pdf",sep=""));
# pdf(paste("./output/","exposure","-by-nb",subset[1],subset[2],".pdf",sep=""));
par(mfrow=c(2,1));
# for(k.neighb in 1:n.neighb){
#   boxplot(
#   cbind(hm[k.neighb,1,1,],   # dirt floor + play/sit -> hand-mouth
#         hm[k.neighb,2,1,],   # concrete floor + play/sit -> hand-mouth
#         fd[k.neighb,1,6,],   # dirt floor + eat -> ingest
#         fd[k.neighb,2,6,],   # concrete floor + eat -> ingest
#         fd[k.neighb,3,6,],   # off ground + eat -> ingest
#         fd[k.neighb,4,6,],   # SWATA + eat -> ingest
#         dw[k.neighb,1,],     # drink tap water
#         dw[k.neighb,2,]),    # drink sachet water
#       outline=FALSE,ylim=c(0,1e6),
#       main=neighbourhoods[k.neighb],names=labels);
# }
frac0 <- function(mc) return(1-length(mc[mc>0])/length(mc));
non0 <- function(mc){
  tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
}

for(k.neighb in 1:n.neighb){
  barplot(c(frac0(hm[k.neighb,1,1,]),
            frac0(hm[k.neighb,2,1,]),
            frac0(hm[k.neighb,3,1,]),
            frac0(hm[k.neighb,,2,]),
            frac0(fd[k.neighb,1,6,]),
            frac0(fd[k.neighb,2,6,]),
            frac0(fd[k.neighb,3,6,]),
            frac0(fd[k.neighb,4,6,]),
            frac0(dw[k.neighb,1,]),
            frac0(dw[k.neighb,2,])),
          ylab="fraction 0",
          main=neighbourhoods[k.neighb],
          names=labels,ylim=c(0,1),cex.names = 0.8);
  boxplot(
  cbind(log10(non0(hm[k.neighb,1,1,])), # dirt floor + play/sit -> hand-mouth
        log10(non0(hm[k.neighb,2,1,])), # concrete floor + play/sit -> hand-mouth
        log10(non0(hm[k.neighb,3,1,])), # off ground + play/sit -> hand-mouth
        log10(non0(hm[k.neighb,3,2,])), # off ground + sleep -> sleep mouthing       
        log10(non0(fd[k.neighb,1,6,])), # dirt floor + eat -> ingest
        log10(non0(fd[k.neighb,2,6,])), # concrete floor + eat -> ingest
        log10(non0(fd[k.neighb,3,6,])), # off ground + eat -> ingest
        log10(non0(fd[k.neighb,4,6,])), # SWATA + eat -> ingest
        log10(non0(dw[k.neighb,1,])),   # drink tap water
        log10(non0(dw[k.neighb,2,]))),  # drink sachet water
      ylab="log(dose)",
      outline=FALSE,ylim=c(-15,15),
      main=neighbourhoods[k.neighb],names=labels,cex.axis = 0.8);
}
dev.off();

hand[is.nan(hand)] <- NA;
hmat <- array(NA,dim=c(n.neighb,n.comp,n.behav));
for(k.neighb in 1:n.neighb){
  for(k.comp in 1:n.comp){
    for(k.behav in 1:n.behav){
      hmat[k.neighb,k.comp,k.behav] <- mean(hand[k.neighb,k.comp,k.behav,],
                                            na.rm=TRUE);
    }
  }
}
hmat[is.nan(hmat)] <- NA;
hmat <- log10(hmat);

# for(k.neighb in 1:n.neighb){
# trellis.device(color=FALSE,pdf,file=paste(neighbourhoods[k.neighb],"pdf",sep="."));
plot.matrix <- function(k.neighb,dmat,maxlev){
  if(is.na(maxlev)) maxlev <- max(dmat[!is.na(dmat)]);
  zlevs <- seq(0.0,maxlev,maxlev/50);
  levelplot(dmat,at=zlevs,
            main=neighbourhoods[k.neighb],
            xlab=compartments[1:5],ylab=behaviours[1:6],
            pretty=TRUE,interpolate=TRUE);
}
#   dev.off();
# }
