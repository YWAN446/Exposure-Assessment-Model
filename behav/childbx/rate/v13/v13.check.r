cnt.comp <- function(comp){
  num <- rep(NA,n.neighb);
  obs.dur <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
  for(k.neighb in 1:n.neighb){
    for(k.subj in 1:n.subj[k.neighb]){
      for(k.obs in 1:n.obs[k.neighb,k.subj]){
        if(obs.comp[k.neighb,k.subj,k.obs]==comp){
          obs.dur[k.neighb,k.subj,k.obs] <-
            time.behav[k.neighb,k.subj,k.obs];
          # print(c(k.neighb," ",k.subj," ",k.obs," ",
          #         obs.dur[k.neighb,k.subj,k.obs]));
        }
      }
    }
    tmp <- c(obs.dur[k.neighb,,]);
    num[k.neighb] <- length(tmp[!is.na(tmp)]);
  }
  return(num);
  # return(c(obs.dur))
}
