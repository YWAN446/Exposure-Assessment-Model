library(Hmisc);
ver <- "v5";
agecats <- c("0-1","1-2","2-");
fileps <- function(name) paste("./output/",ver,".",name,".eps",sep="");

plot.nw <- function(graph,k.neighb,k.agecat){
  setEPS();
  # par(mar=c(0.1,0.1,0.1,0.1));
  postscript(fileps(paste(neighbourhoods[k.neighb],agecats[k.agecat],sep="")),
             width=6,height=6);
  plot(graph,vertex.label.family="Helvetica",vertex.label.cex=0.75);
  dev.off();
}

plot.nw(alajo.0,1,1);
plot.nw(alajo.1,1,2);
plot.nw(alajo.2,1,3);
plot.nw(bukom.0,2,1);
plot.nw(bukom.1,2,2);
plot.nw(bukom.2,2,3);
plot.nw(oldfadama.0,3,1);
plot.nw(oldfadama.1,3,2);
plot.nw(oldfadama.2,3,3);
plot.nw(shiabu.0,4,1);
plot.nw(shiabu.1,4,2);
plot.nw(shiabu.2,4,3);

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
         add=addyn,xlim=c(pos[1]-0.25,pos[4]+0.75),ylim=range,
         xlab="",ylab=name,errbar.col=color,
         xaxt="n");
}

frac.plot <- function(fractab1,fractab2,fractab3,yrange,yname,fname){
  setEPS();
  postscript(fileps(fname),width=7,height=5);
  plot.frac(fractab1,c(1,2,3,4),yrange,yname,"black",FALSE);
  plot.frac(fractab2,c(1,2,3,4)+0.25,yrange,yname,"blue",TRUE);
  plot.frac(fractab3,c(1,2,3,4)+0.50,yrange,yname,"darkblue",TRUE);
  axis(side=1,at=c(1,2,3,4)+0.25,labels=neighbourhoods);
  dev.off();
}

frac.plot(eat.hw.0,eat.hw.1,eat.hw.2,c(0.0,0.2),
          "P(handwashing before eating)","eat-hw");
frac.plot(eat.bt.0,eat.bt.1,eat.bt.2,c(0.0,0.2),
          "P(bathing before eating)","eat-bt");
frac.plot(def.hw.0,def.hw.1,def.hw.2,c(0.0,1.0),
          "P(handwashing after defecation)","def-hw");
frac.plot(def.bt.0,def.bt.1,def.bt.2,c(0.0,1.0),
          "P(bathing after defecation)","def-bt");
