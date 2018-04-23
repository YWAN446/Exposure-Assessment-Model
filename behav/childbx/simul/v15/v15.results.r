library(Hmisc);
ver <- "v15";
file.eps <- function(name) paste("./output/",subset[1],subset[2],"/",
                               name,".eps",sep="");
file.sim <- function(ss) paste("./output/",subset[1],subset[2],"sim.rda",sep="");

nbh <- c("A","B","O-F","S");

print.stats <- function(mc){
  if(length(dim(mc))>0){
    mc <- apply(mc,length(dim(mc)),sum);
  }
  res <- c(mean(mc),quantile(mc,c(0.50,0.025,0.975)))/60;
  rep <- paste("mean",res[1],"median",res[2],"95% range",
               res[3],"-",res[4],sep=" ");
  return(rep);
}

plot.nw <- function(graph,k.neighb){
  setEPS();
  # par(mar=c(0.1,0.1,0.1,0.1));
  postscript(file.eps(neighbourhoods[k.neighb]),
             width=6,height=6);
  plot(graph,vertex.label.family="Helvetica",vertex.label.cex=0.75,
       rescale=FALSE,xlim=c(-1,1),ylim=c(-1,1));
  dev.off();
}

plot.nw(alajo.nw$graph,1);
plot.nw(bukom.nw$graph,2);
plot.nw(oldfadama.nw$graph,3);
plot.nw(shiabu.nw$graph,4);

betaquant <- function(quant,np){
  return(qbeta(p=quant,1+np[1],1+np[2]));
}

plot.frac <- function(fractab,pos,range,name,color,addyn){
  errbar(x=pos,
         y=c(betaquant(0.5,fractab[,1]),betaquant(0.5,fractab[,2]),
             betaquant(0.5,fractab[,3]),betaquant(0.5,fractab[,4])),
         yminus=c(betaquant(0.025,fractab[,1]),betaquant(0.025,fractab[,2]),
             betaquant(0.025,fractab[,3]),betaquant(0.025,fractab[,4])),
         yplus=c(betaquant(0.975,fractab[,1]),betaquant(0.975,fractab[,2]),
             betaquant(0.975,fractab[,3]),betaquant(0.975,fractab[,4])),
         add=addyn,xlim=c(pos[1]-0.25,pos[4]+0.25),ylim=range,
         xlab="",ylab=name,errbar.col=color,
         xaxt="n");
}

frac.plot <- function(fractab,testtab,
                      yrange,yname,fname){
  setEPS();
  postscript(file.eps(fname),width=3,height=5);
  plot.frac(fractab,c(1,2,3,4),yrange,yname,"black",FALSE);
  lines(x=c(0,5),y=c(testtab[1]/testtab[2],testtab[1]/testtab[2]),col="grey");
  axis(side=1,at=c(1,2,3,4),labels=nbh);
  dev.off();
}

frac.plot(eat.hw,test.eat.hw,c(0.0,1.0),
         "P(handwashing before eating)","eat-hw");
frac.plot(eat.bt,test.eat.bt,c(0.0,1.0),
         "P(bathing before eating)","eat-bt");
frac.plot(def.hw,test.def.hw,c(0.0,1.0),
         "P(handwashing after defecating)","def-hw");
frac.plot(def.bt,test.def.bt,c(0.0,1.0),
         "P(bathing after defecating)","def-bt");

frac.plot(eat.og,test.eat.og,c(0.0,1.0),
         "P(off ground before eating)","eat-og");
frac.plot(eat.pf,test.eat.pf,c(0.0,1.0),
         "P(played on floor/dirt before eating)","eat-pf");
frac.plot(eat.pd,test.eat.pd,c(0.0,1.0),
         "P(played in drain before eating)","eat-pd");
frac.plot(eat.df,test.eat.df,c(0.0,1.0),
         "P(defecated before eating)","eat-df");

