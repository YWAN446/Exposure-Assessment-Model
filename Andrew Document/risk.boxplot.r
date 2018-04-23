riskcol <- function(x,loglimit){
  highlev <- quantile(x,0.95);
  meanlev <- log10(mean(10^x));
  if((highlev < loglimit) & (meanlev < loglimit)) return("green");
  if((highlev > loglimit) & (meanlev < loglimit)) return("orange");
  if((highlev > loglimit) & (meanlev > loglimit)) return("red");
}

boxquan <- function(x){
  stats <- c(as.numeric(quantile(x,probs=c(0.05,0.25),na.rm=TRUE)),
             # log10(mean(10^x,na.rm=TRUE)),
             quantile(x,0.5,na.rm=TRUE),
             as.numeric(quantile(x,probs=c(0.75,0.95),na.rm=TRUE)));
  n  <- length(which(!is.na(x)));
  # conf <- c( 1.58*(stats[4]-stats[2])/sqrt(n),
  #           -1.58*(stats[4]-stats[2])/sqrt(n)) +
  #         as.numeric(quantile(x,probs=0.5,na.rm=TRUE));
  conf <- c(stats[5],stats[1]);
  out <- x[x < stats[1] | x > stats[5]]; # outliers; not used
  return(list("stats"=stats,"n"=n,"conf"=conf,"out"=out));
}

mergequan <- function(xmat,names){
  stats <- c(); n <- c(); conf <- c(); out <- c(); group <- c();
  for(k in 1:ncol(xmat)){
    tmp   <- boxquan(xmat[,k]);
    stats <- cbind(stats,tmp$stats);
    n     <- c(n,tmp$n);
    conf  <- cbind(conf,tmp$conf);
    out   <- c(out,tmp$out);
    group <- c(group,rep(k,length(tmp$out)));
  }
  return(list("stats"=stats,"n"=n,"conf"=conf,
              "out"=out,"group"=group,"names"=names));
}

# example of use
# z.exp <- mergequan(cbind(risk(pred),pred.yr), c("day","year"));
# bxp(z.exp,outline=FALSE,ylim=c(-12,1),
#     boxfill=c(riskcol(pred,-4),riskcol(pred.yr,-4)),
#     ylab="10log(risk)");
# lines(x=c(0,3),y=c(-4,-4),lty=2);

