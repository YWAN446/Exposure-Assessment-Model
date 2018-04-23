library(lattice);
setwd("~/stat/sanipath/exposure/exposure/v14/")
load("HH0-1.rda")
load("HH1-2.rda")
load("HH2-5.rda")
load("Net.HH0-1.rda")
load("Net.HH1-2.rda")
load("Net.HH2-5.rda")

neighbourhoods <- c("Alajo","Bukom","Old Fadama","Shiabu");
ages<-c("0-1","1-2","2-5");
labels <- c("soil","floor","offgr","drain",
            "DF","food","tap","sachet","total");

frac0 <- function(mc) return(1-length(mc[mc>0])/length(mc));
non0 <- function(mc){
  tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
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


n.neighb <- 4;
num.mc <- 10000;

setwd("~/stat/sanipath/exposure/exposure/v13/")

int.dirt<-array(NA,dim=c(4,10000,3))
int.flo<-array(NA,dim=c(4,10000,3))
int.offgr<-array(NA,dim=c(4,10000,3))
int.drain<-array(NA,dim=c(4,10000,3))
int.septage<-array(NA,dim=c(4,10000,3))
int.produce<-array(NA,dim=c(4,10000,3))
dw<-array(NA,dim=c(4,2,10000,3))
int.total<-array(NA,dim=c(4,10000,3))

    int.dirt[,,1]<-HH.1[[14]]
    int.flo[,,1]<-HH.1[[16]]
    int.offgr[,,1]<-HH.1[[15]]
    int.drain[,,1]<-HH.1[[13]]
    int.septage[,,1]<-HH.1[[17]]
    int.produce[,,1]<-HH.1[[18]]
    dw[,,,1]<-HH.1[[19]]
    int.total[,,1]<-HH.1[[20]]

    int.dirt[,,2]<-HH.2[[14]]
    int.flo[,,2]<-HH.2[[16]]
    int.offgr[,,2]<-HH.2[[15]]
    int.drain[,,2]<-HH.2[[13]]
    int.septage[,,2]<-HH.2[[17]]
    int.produce[,,2]<-HH.2[[18]]
    dw[,,,2]<-HH.2[[19]]
    int.total[,,2]<-HH.2[[20]]

    int.dirt[,,3]<-HH.3[[14]]
    int.flo[,,3]<-HH.3[[16]]
    int.offgr[,,3]<-HH.3[[15]]
    int.drain[,,3]<-HH.3[[13]]
    int.septage[,,3]<-HH.3[[17]]
    int.produce[,,3]<-HH.3[[18]]
    dw[,,,3]<-HH.3[[19]]
    int.total[,,3]<-HH.3[[20]]

####################################################################;
pdf(paste("./output/","exposure","-age-neighb",".pdf",sep=""));
par(mfrow=c(1,1));
z.exp <- mergequan(cbind(log10(non0(int.total[1,,1])),log10(non0(int.total[2,,1])),log10(non0(int.total[3,,1])),log10(non0(int.total[4,,1])),
                         log10(non0(int.total[1,,2])),log10(non0(int.total[2,,2])),log10(non0(int.total[3,,2])),log10(non0(int.total[4,,2])),
                         log10(non0(int.total[1,,3])),log10(non0(int.total[2,,3])),log10(non0(int.total[3,,3])),log10(non0(int.total[4,,3]))), 
                   c("","0-1 year old","","","","1-2 years old","","","","2-5 years old","",""));
bxp(z.exp,outline=FALSE,ylim=c(0,20),
    boxfill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4","antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"),
    ylab="log10(dose)",xaxt="n",cex.axis = 0.8);
axis(1,at=c(2.5,6.5,10.5),labels=c("0-1 year old","1-2 years old","2-5 years old"))
points(1:12,cbind(log10(mean(non0(int.total[1,,1]),na.rm=TRUE)),log10(mean(non0(int.total[2,,1]),na.rm=TRUE)),log10(mean(non0(int.total[3,,1]),na.rm=TRUE)), log10(mean(non0(int.total[4,,1]),na.rm=TRUE)),
                  log10(mean(non0(int.total[1,,2]),na.rm=TRUE)),log10(mean(non0(int.total[2,,2]),na.rm=TRUE)),log10(mean(non0(int.total[3,,2]),na.rm=TRUE)), log10(mean(non0(int.total[4,,2]),na.rm=TRUE)),
                  log10(mean(non0(int.total[1,,3]),na.rm=TRUE)),log10(mean(non0(int.total[2,,3]),na.rm=TRUE)),log10(mean(non0(int.total[3,,3]),na.rm=TRUE)), log10(mean(non0(int.total[4,,3]),na.rm=TRUE))),
       col="black",cex=1
)
points(1:12,cbind(log10(mean(non0(int.total[1,,1]),na.rm=TRUE)),log10(mean(non0(int.total[2,,1]),na.rm=TRUE)),log10(mean(non0(int.total[3,,1]),na.rm=TRUE)), log10(mean(non0(int.total[4,,1]),na.rm=TRUE)),
                  log10(mean(non0(int.total[1,,2]),na.rm=TRUE)),log10(mean(non0(int.total[2,,2]),na.rm=TRUE)),log10(mean(non0(int.total[3,,2]),na.rm=TRUE)), log10(mean(non0(int.total[4,,2]),na.rm=TRUE)),
                  log10(mean(non0(int.total[1,,3]),na.rm=TRUE)),log10(mean(non0(int.total[2,,3]),na.rm=TRUE)),log10(mean(non0(int.total[3,,3]),na.rm=TRUE)), log10(mean(non0(int.total[4,,3]),na.rm=TRUE))),
       col="black",cex=1.5
)
abline(v=4.5,lty=2)
abline(v=8.5,lty=2)
legend(0,20,bty="n", cex=0.8, title="Neighborhood",neighbourhoods,fill=c("antiquewhite1","antiquewhite2","antiquewhite3","antiquewhite4"))  
dev.off()

pdf(paste("./output/","exposure","-2-5-Bukom",".pdf",sep=""));
par(mfrow=c(1,1));
layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(1,3))
par(mar=c(0.5, 4, 2, 0.5))
barplot(c(1-frac0(int.dirt[2,,3]),
          1-frac0(int.flo[2,,3]),
          1-frac0(int.offgr[2,,3]),
          1-frac0(int.drain[2,,3]),
          1-frac0(int.septage[2,,3]),
          1-frac0(int.produce[2,,3]),
          1-frac0(dw[2,1,,3]),
          1-frac0(dw[2,2,,3]),
          1-frac0(int.total[2,,3])),
        ylab="fraction exposed",
        main="",las = 2,xaxt="n",
        names=c("soil","floor","offgr","drain","DF","food","tap","sachet","total"),
        col="antiquewhite2",ylim=c(0,1),cex.names = 0.8);

par(mar=c(4, 4, 0.5, 0.5))
z.exp <- mergequan(cbind(log10(non0(int.dirt[2,,3])),
                         log10(non0(int.flo[2,,3])),
                         log10(non0(int.offgr[2,,3])),
                         log10(non0(int.drain[2,,3])),
                         log10(non0(int.septage[2,,3])),
                         log10(non0(int.produce[2,,3])),
                         log10(non0(dw[2,1,,3])),
                         log10(non0(dw[2,2,,3])),
                         log10(non0(int.total[2,,3]))), 
                   c("soil","floor","offgr","drain","DF","food","tap","sachet","total"));
bxp(z.exp,outline=FALSE,ylim=c(0,20),las = 2,xaxt="n",
    boxfill="antiquewhite2",ylab="log10(dose)",cex.axis = 0.8);
axis(1,at=1:9,labels=c("soil","floor","offgr","drain","DF","food","tap","sachet","total"))
points(1:9,cbind(log10(mean(non0(int.dirt[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.flo[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.offgr[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.drain[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.septage[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.produce[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(dw[2,1,,3]),na.rm=TRUE)),
                  log10(mean(non0(dw[2,2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.total[2,,3]),na.rm=TRUE))),
       col="black",cex=1
)
points(1:9,cbind(log10(mean(non0(int.dirt[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.flo[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.offgr[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.drain[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.septage[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.produce[2,,3]),na.rm=TRUE)),
                  log10(mean(non0(dw[2,1,,3]),na.rm=TRUE)),
                  log10(mean(non0(dw[2,2,,3]),na.rm=TRUE)),
                  log10(mean(non0(int.total[2,,3]),na.rm=TRUE))),
       col="black",cex=1.5
)
dev.off()

#Network
library(igraph);

eqarrowPlot <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                        edge.arrow.size=rep(1, ecount(graph)),
                        vertex.shape="circle",
                        edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], layout=layout, vertex.shape="none",
         vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

eqarrowPlot1 <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                         edge.arrow.size=rep(1, ecount(graph)),
                         vertex.shape="circle",
                         edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], edge.width=edge.arrow.size[e],
         layout=layout, vertex.shape="none",vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

pdf("./output/Figure6.pdf",height=6.5,width=6);
par(mfrow=c(1,1))
par(mai=c(0.1, 0.1, 0.1, 0.1));
par(mar=c(0,0,0,0));

k.neighb=2

a3.net <- data.frame(start=as.character(Net.HH.a3[[k.neighb]]$start),end=as.character(Net.HH.a3[[k.neighb]]$end),size=as.numeric(as.character(Net.HH.a3[[k.neighb]]$size)),stringsAsFactors = FALSE)
edge <-aggregate(a3.net$size, by=list(a3.net$start,a3.net$end), FUN=sum, na.rm=TRUE)
edge1 <- edge[which(edge[,3]!=0),]
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])

edge.network<-graph.data.frame(edge2, directed=T);

if (k.neighb!=4){
  edge.network<-add.vertices(edge.network,1);
  V(edge.network)[12]$name<-"Sachet water";
}

n.node <- length(V(edge.network))
node.size <- c(0,0,0,0,0,0,0,0,0,0);
node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
node.size[which(V(edge.network)$name=="DF")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="DF")])));
node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
node.size[which(node.size<=0)] <- 1;
V(edge.network)$name[which(V(edge.network)$name=="Person")]="Mouth";
V(edge.network)$name[which(V(edge.network)$name=="Drain")]="Drain";
V(edge.network)$name[which(V(edge.network)$name=="DF")]="DF";
V(edge.network)$name[which(V(edge.network)$name=="Sachet water")]="Sachet";
V(edge.network)$name[which(V(edge.network)$name=="Tap water")]="Tap";
V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";

V(edge.network)$size<-node.size*5;
V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                       ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));

E(edge.network)$color <- "red";
E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
#E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$color <- "green";
E(edge.network)$curved=FALSE;
E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;

weight<-log10(as.numeric(E(edge.network)$size));
#weight[which(weight==-Inf)] <- 0;
weight[which(weight<=0)] <- 1;#replace negative number

l=matrix(c(0,-2,-3,2,-2.5,-2,-2.5,2.25,0.5,2,4,3,
           0,2,0,-2,-1,-2,1,0.7,2,1.5,0,2),12,2);

curved<-c(0,0.5,0,0,0.5,0,0,0,0,0,0,0,0);

eqarrowPlot1(edge.network, l, edge.arrow.size=weight/5,edge.width=1, vertex.size=35, 
             edge.curved=curved,vertex.label.color="black")
#legend("topleft",paste(neighbourhoods[k.neighb],"age 2-5 years"),bty="n", cex=1.5);

dev.off()

