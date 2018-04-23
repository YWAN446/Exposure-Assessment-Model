n.comb <- length(single[1,]);
net.array <- array(0, dim=c(12,12,n.comb));
k.iter <- 1;
k.comp <- single[1,k.iter];
k.behav <- single[2,k.iter];
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

#total amount matrix
my.mat <- apply(net.array,1:2,sum);
mat.labels <- c("Person","Hand","Food","Soil","Floor","Off-ground","Drain","Septage","HW","Bath","Tap water","Sachet water");

edge <- array(0,dim=c(144,3))
for (i in 1:12){
  for (j in 1:12){
    edge[(i-1)*12+j,1] <- mat.labels[i];
    edge[(i-1)*12+j,2] <- mat.labels[j];
    edge[(i-1)*12+j,3] <- my.mat[i,j];
  }
}
edge1 <- edge[which(edge[,3]!=0),];
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])

#install.packages("igraph");
#library(igraph);

edge.network<-graph.data.frame(edge2, directed=T);
n.node <- length(V(edge.network))
node.size <- c(0,0,0,0,0,0,0,0,0,0);
node.size[which(V(edge.network)$name=="Hand")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Hand")])));
node.size[which(V(edge.network)$name=="Person")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Person")])));
node.size[which(V(edge.network)$name=="Food")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Food")])));
node.size[which(V(edge.network)$name=="Soil")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Soil")])));
node.size[which(V(edge.network)$name=="Floor")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Floor")])));
node.size[which(V(edge.network)$name=="Off-ground")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Off-ground")])));
node.size[which(V(edge.network)$name=="Drain")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Drain")])));
node.size[which(V(edge.network)$name=="Septage")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Septage")])));
node.size[which(V(edge.network)$name=="HW")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="HW")])));
node.size[which(V(edge.network)$name=="Bath")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,2]=="Bath")])));
node.size[which(V(edge.network)$name=="Tap water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Tap water")])));
node.size[which(V(edge.network)$name=="Sachet water")] <- log10(sum(as.numeric(E(edge.network)$size[which(edge2[,1]=="Sachet water")])));
node.size[which(node.size<=0)] <- 1;


V(edge.network)$size<-node.size*5;
V(edge.network)$color <- ifelse(V(edge.network)$name=="Person","Blue",
                                ifelse(is.element(V(edge.network)$name,c("Hand","Food","Tap water","Sachet water")),"Purple",
                                       ifelse(is.element(V(edge.network)$name,c("HW","Bath")),"Green","Red")));

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
weight<-log10(as.numeric(E(edge.network)$size));
weight[which(weight<=0)] <- 1;#replace negative number 

eqarrowPlot(edge.network, layout.auto(edge.network), edge.arrow.size=weight/5,
            edge.width=1)


plot(edge.network,edge.width=log10(as.numeric(E(edge.network)$size)*5),
     edge.arrow.size=log10(as.numeric(E(edge.network)$size)));
