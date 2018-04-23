version <- "v6"

exp.by.cat <- function(k.neighb,tot.dur) {
  states <- gen.period.seq(k.neighb,3,2,tot.dur);
  n.states <- ncol(states);
  expos <- array(NA,dim=c(8,n.states));
  comp <- states[1,];
  behav <- states[2,];
  dur <- states[3,];
  n.hand <- 0;
  for(k.state in 1:n.states){
    expos[1:3,k.state] <- states[,k.state];
    res <- expos.by.state(k.neighb,states[1,k.state],states[2,k.state],
                          states[3,k.state],n.hand);
    if(length(res)==2){
      n.hand <- res[1];
      expos[4,k.state] <- n.hand;
      if(states[2,k.state]==6){
        expos[6,k.state] <- res[2];
      }else{
        expos[5,k.state] <- res[2];
      }
    }
  }
  expos[7:8,n.states] <- expos.dw(k.neighb,tot.dur);
  return(expos);
}
