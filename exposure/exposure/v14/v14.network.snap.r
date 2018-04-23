load("~/stat/sanipath/exposure/exposure/v11/single.rda")
library(igraph);
n.comb <- length(single[1,]);
net.array <- array(0, dim=c(12,12,n.comb));
k.iter <- 1;
k.comp <- single[1,k.iter];
k.behav <- single[2,k.iter];
time <- single[3,];
#play/sit;
if (k.comp==1 & k.behav==1) {
  net.array[4,2,k.iter] <- single[5,k.iter]+single[17,k.iter];
  net.array[2,1,k.iter] <- single[17,k.iter];
}
if (k.comp==2 & k.behav==1) {
  net.array[5,2,k.iter] <- single[7,k.iter]+single[19,k.iter];
  net.array[2,1,k.iter] <- single[19,k.iter];
}
if (k.comp==3 & k.behav==1) {
  net.array[6,2,k.iter] <- single[6,k.iter]+single[18,k.iter];
  net.array[2,1,k.iter] <- single[18,k.iter];
}
if ((k.comp==4 | k.comp==5) & k.behav==1) {
  net.array[7,2,k.iter] <- single[4,k.iter]+single[16,k.iter];
}

#defecate;
if (k.comp==1 & k.behav==5) {
  net.array[4,2,k.iter] <- single[5,k.iter];
  net.array[8,2,k.iter] <- single[8,k.iter];
}
if (k.comp==2 & k.behav==5) {
  net.array[5,2,k.iter] <- single[7,k.iter];
  net.array[8,2,k.iter] <- single[8,k.iter];
}
if (k.comp==3 & k.behav==5) {
  net.array[6,2,k.iter] <- single[6,k.iter];
  net.array[8,2,k.iter] <- single[8,k.iter];
}
if ((k.comp==4 | k.comp==5) & k.behav==5) {
  net.array[7,2,k.iter] <- single[4,k.iter];
  net.array[8,2,k.iter] <- single[8,k.iter];
}

#eating;
if (k.behav==6) {
  net.array[3,2,k.iter] <- single[9,k.iter];
  net.array[3,1,k.iter] <- single[21,k.iter];  
}

for (k.iter in 2:n.comb){
  k.comp <- single[1,k.iter];
  k.behav <- single[2,k.iter];
  #play/sit
  if (k.comp==1 & k.behav==1) {
    net.array[4,2,k.iter] <- single[5,k.iter]-single[5,k.iter-1]+sum(single[16:21,k.iter]);
    net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
  }
  if (k.comp==2 & k.behav==1) {
    net.array[5,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1]+sum(single[16:21,k.iter]);
    net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
  }
  if (k.comp==3 & k.behav==1) {
    net.array[6,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1]+sum(single[16:21,k.iter]);
    net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
  }
  if ((k.comp==4 | k.comp==5) & k.behav==1) {
    net.array[7,2,k.iter] <- single[4,k.iter]-single[4,k.iter-1];
  }
  
  #sleeping
  if (k.behav==2) {
    net.array[2,1,k.iter] <- sum(single[16:21,k.iter]);
  }
  #HW
  if (k.behav==3) {
    net.array[2,9,k.iter] <- sum(single[4:9,k.iter-1])-sum(single[4:9,k.iter]);
  }
  #bathe
  if (k.behav==4) {
    net.array[2,10,k.iter] <- sum(single[4:9,k.iter-1])-sum(single[4:9,k.iter]);
  }
  #defecate;
  if (k.comp==1 & k.behav==5) {
    net.array[4,2,k.iter] <- single[5,k.iter]-single[5,k.iter-1];
    net.array[8,2,k.iter] <- single[8,k.iter];
  }
  if (k.comp==2 & k.behav==5) {
    net.array[5,2,k.iter] <- single[7,k.iter]-single[7,k.iter-1];
    net.array[8,2,k.iter] <- single[8,k.iter];
  }
  if (k.comp==3 & k.behav==5) {
    net.array[6,2,k.iter] <- single[6,k.iter]-single[6,k.iter-1];
    net.array[8,2,k.iter] <- single[8,k.iter];
  }
  if ((k.comp==4 | k.comp==5) & k.behav==5) {
    net.array[7,2,k.iter] <- single[4,k.iter];
    net.array[8,2,k.iter] <- single[8,k.iter];
  }
  #eating;
  if (k.behav==6) {
    net.array[3,2,k.iter] <- single[9,k.iter]-single[9,k.iter-1];
    net.array[3,1,k.iter] <- sum(single[16:21,k.iter]);
    net.array[2,3,k.iter] <- sum(single[4:8,k.iter-1])-sum(single[4:8,k.iter]);
  }
  if (k.iter==n.comb) {
    net.array[11,1,k.iter] <- single[22,k.iter];
    net.array[12,1,k.iter] <- single[23,k.iter];
  }
}
mat.labels <- c("Mouth","Hand","Food","Soil","Floor","Off-ground","Drain","DF","HW","Bath","Tap water","Sachet water");


edge <- array(0,dim=c(144*n.comb,4))
for (t in 1:n.comb){
  for (i in 1:12){
    for (j in 1:12){
      edge[(t-1)*144+(i-1)*12+j,4] <- sum(as.numeric(time[1:t]));
      edge[(t-1)*144+(i-1)*12+j,1] <- mat.labels[i];
      edge[(t-1)*144+(i-1)*12+j,2] <- mat.labels[j];
      edge[(t-1)*144+(i-1)*12+j,3] <- net.array[i,j,t];
    }
  }
}
edge1 <- edge[which(edge[,3]!=0),];
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3],time=edge1[,4]);
edge.net<-graph.data.frame(edge2, directed=T);

g <- graph.edgelist(as.matrix(edge2[,c(1,2)]),directed=T)
E(g)$time <- as.numeric(as.vector(edge2[,4]))
E(g)$conc <- as.numeric(as.vector(edge2[,3]))
replace_end<-as.vector(edge2[which(E(g)$conc<0),1]);
replace_start<-as.vector(edge2[which(E(g)$conc<0),2]);
v.start<-as.vector(edge2[,1]);
v.end<-as.vector(edge2[,2]);
v.start[which(E(g)$conc<0)]<-replace_start;
v.end[which(E(g)$conc<0)]<-replace_end;
edge3<-data.frame(start=v.start,end=v.end, size=edge1[,3],time=edge1[,4]);
g <- graph.edgelist(as.matrix(edge3[,c(1,2)]),directed=T)
E(g)$time <- as.numeric(as.vector(edge3[,4]))
E(g)$conc <- abs(as.numeric(as.vector(edge3[,3])))

#generate a cool palette for the graph
#YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
#YlOrBr.Lab <- colorRampPalette(YlOrBr, space = "Lab")
#colors for the nodes are chosen from the very beginning
#vcolor <- rev(YlOrBr.Lab(vcount(g)))

vcolor <- ifelse(V(g)$name=="Mouth","lightslateblue",
                 ifelse(is.element(V(g)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                        ifelse(is.element(V(g)$name,c("HW","Bath")),"Green1","Red1")));



#time in the edges goes from 1 to 300. We kick off at time 3
ti <- ceiling(min(E(g)$time))
#weights of edges formed up to time ti is 1. Future edges are weighted 0
E(g)$weight <- log10(E(g)$conc);

uni_t<-unique(E(g)$time)

###########################################################################

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

#setwd("~/stat/sanipath/exposure/exposure/v14/")
letters<-c("a","b","c","d","e","f","g","h","i","j","k","l")
pdf(file=paste("./output/","exposure","-network-snap-1",".pdf",sep=""),width=10.00, height=14.14);
par(mfrow=c(4,3));
par(mar=c(2,2,2,2));
#for (k.t in 1:length(uni_t)){
for (k.t in 13:24){
  
  edge.network<-delete_edges(g, which(!E(g)$time==uni_t[k.t]))
  
  n.node <- length(V(edge.network))

  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-1;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-E(edge.network)$weight/3
  weight[which(weight<=0)] <- 1;#replace negative number
  
  l<-matrix(c(-2,0,2,4,-2.5,-2,-3,0.7,2,-2.5,-2,0,-2,0,1,2,0,2.25,1.5,-1),10,2)
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35, #edge.curved=curved,
               vertex.label.color="black");
  mtext(paste("(",letters[k.t-12],")",sep=""),side=1,line=1)
}
dev.off();

total_time <- max(E(g)$time)
dt <- 1

png(file="~/stat/sanipath/exposure/exposure/v14/network/example%03d.png", width=1600,height=1600, res=400)
nsteps <- max(E(g)$time)
count=ceiling(min(E(g)$time));
par(mar=c(2, 0.5, 4, 0.5))
par(mfrow=c(1,1),mai = c(0.35, 0.1, 0.65, 0.1));

for (k.t in 1:length(uni_t)){
  edge.network<-delete_edges(g, which(!E(g)$time==uni_t[k.t]))
  
  n.node <- length(V(edge.network))
  
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-1;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-E(edge.network)$weight/3
  weight[which(weight<=0.5)] <- 0.5;#replace negative number
  
  l<-matrix(c(-2,0,2,4,-2.5,-2,-3,0.7,2,-2.5,-2,0,-2,0,1,2,0,2.25,1.5,-1),10,2)
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=40, #edge.curved=curved,
               vertex.label.color="black");
  title(main=paste("Time = ",7+(uni_t[k.t]%/%60),":",round(uni_t[k.t]%%60,0)), cex.main = 0.8)
  #use the new layout in the next round
  #layout.old <- layout.new 
}

dev.off();

png(file="~/stat/sanipath/exposure/exposure/v14/network/example027.png", width=1600,height=1600, res=400)
nsteps <- max(E(g)$time)
count=ceiling(min(E(g)$time));
par(mar=c(2, 0.5, 4, 0.5))
par(mfrow=c(1,1),mai = c(0.35, 0.1, 0.65, 0.1));


  edge.network<-g
  
  n.node <- length(V(edge.network))
  
  V(edge.network)$name[which(V(edge.network)$name=="Off-ground")]="Off-gr";
  
  V(edge.network)$size<-1;
  V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                  ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"gold1",
                                         ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"green1","red1")));
  
  E(edge.network)$color <- "red";
  E(edge.network)[ V(edge.network) %->% V(edge.network)[ name=="Bath" | name=="HW"] ]$color <- "green";
  E(edge.network)$curved=FALSE;
  E(edge.network)[ V(edge.network)[ name=="Hand"] %->% V(edge.network)[ name=="Food"] ]$curved=TRUE;
  E(edge.network)[ V(edge.network)[ name=="Food"] %->% V(edge.network)[ name=="Hand"] ]$curved=TRUE;
  
  weight<-E(edge.network)$weight/3
  weight[which(weight<=0.5)] <- 0.5;#replace negative number
  
  l<-matrix(c(-2,0,2,4,-2.5,-2,-3,0.7,2,-2.5,-2,0,-2,0,1,2,0,2.25,1.5,-1),10,2)
  
  eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=40, #edge.curved=curved,
               vertex.label.color="black");
  #title(main=paste("Time = ",7+(uni_t[k.t]%/%60),":",round(uni_t[k.t]%%60,0)), cex.main = 0.8)
  #use the new layout in the next round
  #layout.old <- layout.new 

dev.off();

