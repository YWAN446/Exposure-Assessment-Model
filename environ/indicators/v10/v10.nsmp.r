ind.tp <- c();
for(k.smptp in 1:length(offs)) ind.tp <- c(ind.tp,rep(k.smptp,offs[k.smptp]));
ngh <- c(levels(neigh),NA);
sink("./v10.comb.csv");
line <- paste("num",",","neighbourhood",",","num",",",
              "environment",",","smpl type",",","smpl attrib",",",
              "nsampl",sep="");
cat(line,"\n");
for(k.neigh in 1:length(ngh)){
  if(is.na(ngh[k.neigh]))  ngh.comb <- which(is.na(neigh));
  if(!is.na(ngh[k.neigh])) ngh.comb <- which(neigh==ngh[k.neigh]);
  for(k.comb in 1:nrow(all.comb)){
    smp.comb <- ind.env.smp[which(ind.env.smp[,2]==all.comb[k.comb,1] &
                      ind.env.smp[,3]==all.comb[k.comb,2]),4];
    ncmb <- length(intersect(smp.comb,ngh.comb));
    if(ncmb!=0){
      line <- paste(k.neigh,",",ngh[k.neigh],",",k.comb,",",
                    env.label[all.comb[k.comb,1]],",",
                    smt.label[ind.tp[all.comb[k.comb,2]]],",",
                    smp.label[all.comb[k.comb,2]],",",ncmb,sep="");
      cat(line,"\n");
    }
  }
}
sink();
