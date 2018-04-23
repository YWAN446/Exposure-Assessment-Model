library(Hmisc);
library(stringr);
source("./minticks.r")
ver <- "v14";

neighbourhoods <- c("alajo","bukom","old-fadama","shiabu")
compartments <-c("dirt","floor","off grnd","SWATA","drain");
behaviours <- c("play-sit","sleep","handw","bathe","defec","eat");
cat.lab <- c("0-1","1-2",">2","0-1","1-2",">2");
nb.lab <- c("A","B","OF","S")
# dom.lab <- c("hh","ns","hh","ns","hh","ns","hh","ns");
dom.lab <- c("hh","hh","hh","ns","ns","ns");

n.neighb <- length(neighbourhoods);
n.comp <- length(compartments);
n.behav <-length(behaviours);
n.sim <- 1000;
n.cat <- 6;

file.eps <- function(name) paste("./output/marg/",name,".eps",sep="");
file.sim <- function(ss) paste("./output/",ss[1],ss[2],"sim.rda",sep="");

mknm <- function(labels,sel){
  name <- "";
  for(k in 1:length(sel)){
    name <- paste(name,labels[sel[k]],sep="");
  }
  return(name);
}

mnpred <- function(mcsmpl){
  mn <- mean(mcsmpl,na.rm=TRUE);
  pi <- c(quantile(mcsmpl,0.025,na.rm=TRUE),mn,
          quantile(mcsmpl,0.975,na.rm=TRUE));
  sc <- sqrt(length(mcsmpl));
  sc <- 1;
  return(mn + (pi-mn)/sc);
}

freq.mc <- array(NA,dim=c(n.cat,n.neighb,n.comp,n.behav,n.sim));
time.mc <- array(NA,dim=c(n.cat,n.neighb,n.comp,n.behav,n.sim));
for(k.cat in 1:n.cat){
  if(k.cat<=3) load(file.sim(c("hh",k.cat-1)));
  if(k.cat >3) load(file.sim(c("ns",k.cat-4)));
  freq.mc[k.cat,,,,] <- sim$freq.day;
  time.mc[k.cat,,,,] <- sim$time.day;
}

bycomp <- function(outp,k.comp){
  tmp <- array(0,dim=c(n.neighb,n.cat,n.sim));
  for(k.cat in 1:n.cat){
    for(k.neighb in 1:n.neighb){
      for(k.behav in 1:n.behav){
          tmp[k.neighb,k.cat,] <- tmp[k.neighb,k.cat,] +
            outp[k.cat,k.neighb,k.comp,k.behav,];
      }
    }
  }
  return(tmp);
}

bybehav <- function(outp,k.behav){
  tmp <- array(0,dim=c(n.neighb,n.cat,n.sim));
  for(k.cat in 1:n.cat){
    for(k.neighb in 1:n.neighb){
      for(k.comp in 1:n.comp){
        tmp[k.neighb,k.cat,] <- tmp[k.neighb,k.cat,] +
            outp[k.cat,k.neighb,k.comp,k.behav,];
      }
    }
  }
  return(tmp);
}

mbycomp <- function(outp,compvec){
  tm <- array(0,dim=c(n.neighb,n.cat,n.sim));
  for(k.cat in 1:n.cat){
    for(k.neighb in 1:n.neighb){
      for(k in 1:length(compvec)){
        tmp0 <- bycomp(outp,compvec[k]);
        tm[k.neighb,k.cat,] <- tm[k.neighb,k.cat,] + tmp0[k.neighb,k.cat,];
      }
    }
  }
  return(tm);
}

mbybehav <- function(outp,behavvec){
  tm <- array(0,dim=c(n.neighb,n.cat,n.sim));
  for(k.cat in 1:n.cat){
    for(k.neighb in 1:n.neighb){
      for(k in 1:length(behavvec)){
        tmp0 <- bybehav(outp,behavvec[k]);
        tm[k.neighb,k.cat,] <- tm[k.neighb,k.cat,] + tmp0[k.neighb,k.cat,];
      }
    }
  }
  return(tm);
}

boxp.comp <- function(compvec,catvec,
                      omit.zero=FALSE,logscale=TRUE,yrange){
  setEPS();
  postscript(file.eps(mknm(compartments,compvec)),width=6,height=3);
  tcomp <- mbycomp(time.mc,compvec)/(60);
  mc <- array(NA,dim=c(n.neighb*length(catvec),length(tcomp[1,1,])));
  k.cnt <- 1;
  for(k.neighb in 1:n.neighb){
    for(k.cat in catvec){
      mc[k.cnt,] <- tcomp[k.neighb,k.cat,];
      k.cnt <- k.cnt+1;
    }
  }
  n.cnt <- k.cnt-1;
  if(omit.zero) mc[mc==0] <- NA;
  if (logscale){
    mc <- log10(mc);
    boxplot(t(mc),outline=FALSE,range=2,ylim=yrange,
         xlab="",ylab="hrs",xaxt="none",yaxt="none");
    ticks.log(2,n.major=2);
  }
  if (!logscale) boxplot(t(mc),outline=FALSE,range=2,ylim=yrange,
         xlab="",ylab="hrs",xaxt="none");
  axis(1,at=(1:n.cnt),cex.axis=0.7,
       lab=c(rep(cat.lab[catvec],n.neighb)),padj=-2);
  axis(1,at=seq(from=(length(catvec)+1)/2,to=n.cnt,by=length(catvec)),
       cex.axis=1,lab=neighbourhoods,lwd=0,padj=0);
  dev.off();
}

boxp.behav <- function(behavvec,catvec,
                       omit.zero=FALSE,logscale=TRUE,yrange,is.freq=FALSE){
  setEPS();
  postscript(file.eps(mknm(behaviours,behavvec)),width=6,height=3);
  if( is.freq) tbeh <- mbybehav(freq.mc,behavvec);
  if(!is.freq) tbeh <- mbybehav(time.mc,behavvec)/(60);
  mc <- array(NA,dim=c(n.neighb*length(catvec),length(tbeh[1,1,])));
  k.cnt <- 1;
  for(k.neighb in 1:n.neighb){
    for(k.cat in catvec){
      mc[k.cnt,] <- tbeh[k.neighb,k.cat,];
      k.cnt <- k.cnt+1;
    }
  }
  n.cnt <- k.cnt-1;
  if(!is.freq) ax.lab <- "hrs";
  if( is.freq) ax.lab <- "1/day";
  if(omit.zero) mc[mc==0] <- NA;
  if(logscale){
    mc <- log10(mc);
    boxplot(t(mc),outline=FALSE,range=2,ylim=yrange,
         xlab="",ylab=ax.lab,xaxt="none",yaxt="none");
    ticks.log(2,n.major=2);
  }
  if (!logscale) boxplot(t(mc),outline=FALSE,range=2,ylim=yrange,
         xlab="",ylab=ax.lab,xaxt="none");
  axis(1,at=(1:n.cnt),cex.axis=0.7,
       lab=c(rep(cat.lab[catvec],n.neighb)),padj=-2);
  axis(1,at=seq(from=(length(catvec)+1)/2,to=n.cnt,by=length(catvec)),
       cex.axis=1,lab=neighbourhoods,lwd=0,padj=0);
  dev.off();
}

print.comp <- function(compvec,catvec,
                       omit.zero=FALSE,logscale=TRUE){
  tcomp <- mbycomp(time.mc,compvec)/(60);
  mc <- array(NA,dim=c(n.neighb*length(catvec),length(tcomp[1,1,])));
  k.cnt <- 1;
  for(k.neighb in 1:n.neighb){
    for(k.cat in catvec){
      mc[k.cnt,] <- tcomp[k.neighb,k.cat,];
      k.cnt <- k.cnt+1;
    }
  }
  n.cnt <- k.cnt-1;
  if(omit.zero) mc[mc==0] <- NA;
  if (logscale) mc <- log10(mc);
  cat(compartments[compvec],"\n");
  cat("neighb     cat dom mn       q50      q2.5     q97.5  \n")
  for(k.cnt in 1:n.cnt)
    cat(paste(str_pad(neighbourhoods[ceil(k.cnt/length(catvec))],10,"right"),
      str_pad(cat.lab[catvec[((k.cnt-1)%%length(catvec))+1]],3,"right"),
      str_pad(dom.lab[catvec[((k.cnt-1)%%length(catvec))+1]],3,"right"),
      str_pad(signif(mean(mc[k.cnt,],na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.5,na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.025,na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.975,na.rm=TRUE),digits=3),8,"right")),
      "\n");
  # return(mc);
}

print.behav <- function(behavvec,catvec,
                        omit.zero=FALSE,logscale=TRUE,is.freq=FALSE){
  if( is.freq) tbeh <- mbybehav(freq.mc,behavvec);
  if(!is.freq) tbeh <- mbybehav(time.mc,behavvec)/(60);
  mc <- array(NA,dim=c(n.neighb*length(catvec),length(tbeh[1,1,])));
  k.cnt <- 1;
  for(k.neighb in 1:n.neighb){
    for(k.cat in catvec){
      mc[k.cnt,] <- tbeh[k.neighb,k.cat,];
      k.cnt <- k.cnt+1;
    }
  }
  n.cnt <- k.cnt-1;
  if(omit.zero) mc[mc==0] <- NA;
  if(logscale) mc <- log10(mc);
  cat(behaviours[behavvec],"\n");
  cat("neighb     cat dom mn       q50      q2.5     q97.5  \n")
  for(k.cnt in 1:n.cnt)
    cat(paste(str_pad(neighbourhoods[ceil(k.cnt/length(catvec))],10,"right"),
      str_pad(cat.lab[catvec[((k.cnt-1)%%length(catvec))+1]],3,"right"),
      str_pad(dom.lab[catvec[((k.cnt-1)%%length(catvec))+1]],3,"right"),
      str_pad(signif(mean(mc[k.cnt,],na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.5,na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.025,na.rm=TRUE),digits=3),8,"right"),
      str_pad(signif(quantile(mc[k.cnt,],0.975,na.rm=TRUE),digits=3),8,"right")),
      "\n");
  # return(mc);
}

# boxp.comp(c(1),  c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
# boxp.comp(c(2),  c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
# boxp.comp(c(3),  c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
# boxp.comp(c(4,5),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,5));
# boxp.behav(c(1),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15),
#            is.freq=FALSE);
# boxp.behav(c(2),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,5),
#            is.freq=FALSE);
# boxp.behav(c(3),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,20),
#            is.freq=TRUE);
# boxp.behav(c(4),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,20),
#            is.freq=TRUE);
# boxp.behav(c(5),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,20),
#            is.freq=TRUE);
# boxp.behav(c(6),c(1,2,3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,20),
#            is.freq=TRUE);

boxp.comp(c(1),  c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
boxp.comp(c(2),  c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
boxp.comp(c(3),  c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15));
boxp.comp(c(4,5),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,5));
boxp.behav(c(1),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,15),
           is.freq=FALSE);
boxp.behav(c(2),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,5),
           is.freq=FALSE);
boxp.behav(c(3),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,60),
           is.freq=TRUE);
boxp.behav(c(4),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,60),
           is.freq=TRUE);
boxp.behav(c(5),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,60),
           is.freq=TRUE);
boxp.behav(c(6),c(4,5,6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,60),
           is.freq=TRUE);

# qbycomp <- function(outp,compvec,omit.zero){
#   tq <- array(NA,dim=c(n.neighb,n.cat,3));
#   for(k.cat in 1:n.cat){
#     for(k.neighb in 1:n.neighb){
#       tmp <- rep(0,n.behav*n.sim);
#       for(k in 1:length(compvec)){
#         tmp0 <- bycomp(outp,compvec[k]);
#         tmp <- tmp + tmp0[k.neighb,k.cat,];
#       }
#       if(omit.zero) tmp[tmp==0] <- NA;
#       tq[k.neighb,k.cat,] <- mnpred(tmp);
#     }
#   }
#   return(tq);
# }

# qbybehav <- function(outp,behavvec,omit.zero){
#   tq <- array(NA,dim=c(n.neighb,n.cat,3));
#   for(k.cat in 1:n.cat){
#     for(k.neighb in 1:n.neighb){
#       tmp <- rep(0,n.comp*n.sim);
#       for(k in 1:length(behavvec)){
#         tmp0 <- bybehav(outp,behavvec[k]);
#         tmp <- tmp + tmp0[k.neighb,k.cat,];
#       }
#       if(omit.zero) tmp[tmp==0] <- NA;
#       tq[k.neighb,k.cat,] <- mnpred(tmp);
#     }
#   }
#   return(tq);
# }

# errp.comp <- function(compvec,omit.zero=FALSE,logscale=TRUE,yrange){
#   addto <- FALSE;
#   xa <- numeric(0);
#   setEPS();
#   postscript(file.eps(mknm(compartments,compvec)),width=6,height=3);
#   tcomp <- qbycomp(time.mc,compvec,omit.zero)/(60);
#   if(logscale) tcomp <- log10(tcomp);
#   for(k.neighb in 1:n.neighb){
#     if(k.neighb>1) addto <- TRUE;
#     xt <- (k.neighb-1)*n.cat+c((1:(n.cat/2)),((1+n.cat/2):n.cat)+1)/1.5;
#     if (logscale) errbar(x=xt,y=tcomp[k.neighb,,2],
#            yplus=tcomp[k.neighb,,3],yminus=tcomp[k.neighb,,1],
#            xlim=c(1,n.neighb*n.cat),ylim=yrange,add=addto,
#            xlab="",ylab="hrs",xaxt="none",yaxt="none");
#     if (!logscale) errbar(x=xt,y=tcomp[k.neighb,,2],
#            yplus=tcomp[k.neighb,,3],yminus=tcomp[k.neighb,,1],
#            xlim=c(1,n.neighb*n.cat),ylim=yrange,add=addto,
#            xlab="",ylab="hrs",xaxt="none");
#     xa <- c(xa,xt);
#   }
#   ticks.log(2,n.major=2);
#   axis(1,at=xa[c(2,5,8,11,14,17,20,23)],cex.axis=0.7,lab=dom.lab,padj=-2);
#   axis(1,at=c(2.5,8.5,14.5,20.5),cex.axis=1,lab=neighbourhoods,lwd=0,padj=0);
#   dev.off();
# }

# errp.behav <- function(behavvec,omit.zero=FALSE,logscale=TRUE,yrange,
#                        is.freq=FALSE){
#   addto <- FALSE;
#   xa <- numeric(0);
#   setEPS();
#   postscript(file.eps(mknm(behaviours,behavvec)),width=6,height=3);
#   if( is.freq) tbeh <- qbybehav(freq.mc,behavvec,omit.zero);
#   if(!is.freq) tbeh <- qbybehav(time.mc,behavvec,omit.zero=TRUE)/(60);
#   if(logscale) tbeh <- log10(tbeh);
#   for(k.neighb in 1:n.neighb){
#     if(k.neighb>1) addto <- TRUE;
#     xt <- (k.neighb-1)*n.cat+c((1:(n.cat/2)),((1+n.cat/2):n.cat)+1)/1.5;
#     if( is.freq) y.label <- "1/day";
#     if(!is.freq) y.label <- "hrs";
#     if (logscale) errbar(x=xt,y=tbeh[k.neighb,,2],
#            yplus=tbeh[k.neighb,,3],yminus=tbeh[k.neighb,,1],
#            xlim=c(1,n.neighb*n.cat), ylim=yrange,
#            add=addto,xlab="",ylab=y.label,xaxt="none",yaxt="none");
#     if (!logscale) errbar(x=xt,y=tbeh[k.neighb,,2],
#            yplus=tbeh[k.neighb,,3],yminus=tbeh[k.neighb,,1],
#            xlim=c(1,n.neighb*n.cat), ylim=yrange,
#            add=addto,xlab="",ylab=y.label,xaxt="none");
#     xa <- c(xa,xt);
#   }
#   if(logscale) ticks.log(2,n.major=2);
#   axis(1,at=xa[c(2,5,8,11,14,17,20,23)],cex.axis=0.7,lab=dom.lab,padj=-2);
#   axis(1,at=c(2.5,8.5,14.5,20.5),cex.axis=1,lab=neighbourhoods,lwd=0,padj=0);
#   dev.off();
# }

# errp.comp(c(1),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0));   # off ground
# errp.comp(c(2),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0));   # floor
# errp.comp(c(3),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0));   # dirt
# errp.comp(c(4,5),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0)); # swata/drain

# errp.behav(c(1),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0),
#            is.freq=FALSE); # play/sit
# errp.behav(c(2),omit.zero=TRUE,logscale=TRUE,yrange=c(-4,0),
#            is.freq=FALSE); # sleep
# errp.behav(c(3),omit.zero=FALSE,logscale=FALSE,yrange=c(0,10),
#            is.freq=TRUE); # handwashing
# errp.behav(c(4),omit.zero=FALSE,logscale=FALSE,yrange=c(0,10),
#            is.freq=TRUE); # bathing
# errp.behav(c(5),omit.zero=FALSE,logscale=FALSE,yrange=c(0,10),
#            is.freq=TRUE); # defecating
# errp.behav(c(6),omit.zero=FALSE,logscale=FALSE,yrange=c(0,10),
#            is.freq=TRUE); # eating
