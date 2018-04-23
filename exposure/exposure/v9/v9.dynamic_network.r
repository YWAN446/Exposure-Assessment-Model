#dynamic network plot
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
mat.labels <- c("Mouth","Hand","Food","Soil","Floor","Off-ground","drain","DF","HW","Bath","Tap water","Sachet water");


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
###E(g)$weight <- ifelse(E(g)$time < ti,1,0)
#generate first layout using weights.
#layout.old <- layout.fruchterman.reingold(g,params=list(weights=E(g)$weight))


#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10 
#of the time (i.e. 10 snapshots) between adding two consecutive nodes 
dt <- 1
#Output for each frame will be a png with HD size 1600x900 :)
#png(file="~/stat/sanipath/exposure/exposure/v10/network/example%03d.png", width=1600,height=900, res=200)
png(file="~/stat/sanipath/exposure/exposure/v11/network5/example%03d.png", width=1600,height=900, res=200)
nsteps <- max(E(g)$time)
count=ceiling(min(E(g)$time));
par(mar=c(4, 0.5, 4, 0.5))
par(mfrow=c(1,1),mai = c(1, 0.1, 1, 0.1));
#lay.out=layout.fruchterman.reingold(g)
#lay.out<-matrix(c(0,5,-2.5,2.5,-2.5,2.5,-3,1,-2,-2,0,0,-1,-2,1,1,0,2,-2,2),10,2)
#lay.out<-matrix(c(-2,0,2,4,-2.5,-2,-3,0.7,2,-2.5,-2,0,-2,0,1,2,0,2.25,1.5,-1),10,2)
lay.out<-matrix(c(0,4,-2.5,0.7,2,-3,-2,2,-2.5,-2,
                  0,0,-1,2.25,1.5,0,2,-2,1,-2),10,2)
#Time loop starts
for(ti in seq(ceiling(min(E(g)$time)),total_time,dt)){
  #define weight for edges present up to time ti.
  E(g)$weight <- ifelse(E(g)$time < ti,log10(E(g)$conc),0) 
  #Edges with non-zero weight are in gray. The rest are transparent
  E(g)$color <- ifelse(E(g)$time < ti,"gray",rgb(0,0,0,0))
  #Nodes with at least a non-zero weighted edge are in color. The rest are transparent
  V(g)$color <- ifelse(graph.strength(g)==0,rgb(0,0,0,0),vcolor)
  #given the new weights, we update the layout a little bit
  #layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,start=layout.old,weights=log10(E(g)$weight),maxdelta=1))
  #plot the new graph
  strength<-graph.strength(g)
  for (i in 1:10){
    if (strength[i]<1 & strength[i]>0) strength[i]=1
  }
  plot(g,layout=lay.out,vertex.size=1+5*log(strength),vertex.frame.color=V(g)$color,vertex.label.color="black",edge.width=E(g)$weight/1.5,edge.arrow.size=0.75,asp=9/16,margin=0)
  title(main="simulated exposure in a typical child day (2-5 years old, Bukom)", 
        sub=paste("Time = ",7+(count%/%60),":",count%%60), cex.main = 1.5, cex.sub = 1)
  count = count +1;
  #use the new layout in the next round
  #layout.old <- layout.new 
}
dev.off();

#In the ffmpeg, use the code "ffmpeg -r 10 -i example%03d.png -b 20M output.mp4"

#In the ffmpeg, use the code "ffmpeg -framerate 10 -i example%03d.png -r 10 output.mp4"

