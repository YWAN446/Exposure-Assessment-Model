ver <- "v5";
# library(network);
# library(sna);
library(igraph);

col <- c("dark gray","light gray","white","pink","red");
# lab <- c("play","sleep","handw","bathe","defec","eat")
lab <- c("play","slp","hw","bath","def","eat")

xcoord <- c(1.0,1.5,2.5,3.5,4.5,5.0,
            2.0,2.5,3.5,4.0,3.5,2.5,
            1.0,1.5,2.5,3.5,4.5,5.0,
            0.0,0.0,0.0,0.0,0.0,0.0,
            6.0,6.0,6.0,6.0,6.0,6.0);
ycoord <- c(2.2,1.2,0.5,0.5,1.2,2.2,
            3.0,4.0,4.0,3.0,2.0,2.0,
            3.8,4.7,5.5,5.5,4.7,3.8,
            0.5,1.5,2.5,3.5,4.5,5.5,
            0.5,1.5,2.5,3.5,4.5,5.5);

vert.labels <- rep(NA,n.comp*n.behav);
vert.colors <- rep(NA,n.comp*n.behav);
for(k.comp in 1:n.comp){
  for(k.behav in 1:n.behav){
    vert.labels[(k.comp-1)*n.behav+k.behav] <- lab[k.behav];
    vert.colors[(k.comp-1)*n.behav+k.behav] <- col[k.comp];
  }
}

make.edgeweights <- function(bs){
  edg.mat <- make.edges(bs);
  edg.wgt  <- edg.mat[edg.mat[,3]!=0,3];
  return(edg.wgt);
}

make.nw <- function(k.neighb,k.agecat){
  beh.seq <- obs.sequence(k.neighb,k.agecat);
  adj.mat <- make.adj(beh.seq);
  adj.mat[adj.mat!=0]=1;
  nw <- graph.adjacency(adj.mat);
  E(nw)$weight <- make.edgeweights(beh.seq);
  V(nw)$name <- vert.labels;
  V(nw)$color <- vert.colors;
  V(nw)$x <- xcoord;
  V(nw)$y <- ycoord;
  return(nw);
}

pathstrength <- function(gr,from.list,to.list,direction){
  wt.sel <- 0;
  wt.nonsel <- 0;
  for(k.from in 1:length(from.list)){
    nbr <- neighbors(gr,from.list[k.from],direction);
    sel <- intersect(nbr,to.list);
    nonsel <- setdiff(nbr,to.list);
    # cat("yes: ",sel,"no: ",nonsel,"\n");
    if(direction=="out" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(gr,P=c(from.list[k.from],sel[k.to]))$weight;
      }
    }
    if(direction=="out" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(gr,P=c(from.list[k.from],nonsel[k.to]))$weight;
      }
    }
    if(direction=="in" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(gr,P=c(sel[k.to],from.list[k.from]))$weight;
      }
    }
    if(direction=="in" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(gr,P=c(nonsel[k.to],from.list[k.from]))$weight;
      }
    }
  }
  return(c(wt.sel,wt.nonsel));
}

play  <- c( 1, 7,13,19,25);
sleep <- c( 2, 8,14,20,26);
handw <- c( 3, 9,15,21,27);
bathe <- c( 4,10,16,22,28);
defec <- c( 5,11,17,23,29);
eat   <- c( 6,12,18,24,30);

alajo.0 <- make.nw(1,1);
bukom.0 <- make.nw(2,1);
oldfadama.0 <- make.nw(3,1);
shiabu.0 <- make.nw(4,1);

alajo.1 <- make.nw(1,2);
bukom.1 <- make.nw(2,2);
oldfadama.1 <- make.nw(3,2);
shiabu.1 <- make.nw(4,2);

alajo.2 <- make.nw(1,3);
bukom.2 <- make.nw(2,3);
oldfadama.2 <- make.nw(3,3);
shiabu.2 <- make.nw(4,3);

# eating when first washed hands
eat.hw.0 <- cbind(pathstrength(alajo.0,eat,handw,"in"),
                  pathstrength(bukom.0,eat,handw,"in"),
                  pathstrength(oldfadama.0,eat,handw,"in"),
                  pathstrength(shiabu.0,eat,handw,"in"));
eat.hw.1 <- cbind(pathstrength(alajo.1,eat,handw,"in"),
                  pathstrength(bukom.1,eat,handw,"in"),
                  pathstrength(oldfadama.1,eat,handw,"in"),
                  pathstrength(shiabu.1,eat,handw,"in"));
eat.hw.2 <- cbind(pathstrength(alajo.2,eat,handw,"in"),
                  pathstrength(bukom.2,eat,handw,"in"),
                  pathstrength(oldfadama.2,eat,handw,"in"),
                  pathstrength(shiabu.2,eat,handw,"in"));
# defecation followed by handwashing
def.hw.0 <- cbind(pathstrength(alajo.0,defec,handw,"out"),
                  pathstrength(bukom.0,defec,handw,"out"),
                  pathstrength(oldfadama.0,defec,handw,"out"),
                  pathstrength(shiabu.0,defec,handw,"out"));
def.hw.1 <- cbind(pathstrength(alajo.1,defec,handw,"out"),
                  pathstrength(bukom.1,defec,handw,"out"),
                  pathstrength(oldfadama.1,defec,handw,"out"),
                  pathstrength(shiabu.1,defec,handw,"out"));
def.hw.2 <- cbind(pathstrength(alajo.2,defec,handw,"out"),
                  pathstrength(bukom.2,defec,handw,"out"),
                  pathstrength(oldfadama.2,defec,handw,"out"),
                  pathstrength(shiabu.2,defec,handw,"out"));
# eating when first bathed
eat.bt.0 <- cbind(pathstrength(alajo.0,eat,bathe,"in"),
                  pathstrength(bukom.0,eat,bathe,"in"),
                  pathstrength(oldfadama.0,eat,bathe,"in"),
                  pathstrength(shiabu.0,eat,bathe,"in"));
eat.bt.1 <- cbind(pathstrength(alajo.1,eat,bathe,"in"),
                  pathstrength(bukom.1,eat,bathe,"in"),
                  pathstrength(oldfadama.1,eat,bathe,"in"),
                  pathstrength(shiabu.1,eat,bathe,"in"));
eat.bt.2 <- cbind(pathstrength(alajo.2,eat,bathe,"in"),
                  pathstrength(bukom.2,eat,bathe,"in"),
                  pathstrength(oldfadama.2,eat,bathe,"in"),
                  pathstrength(shiabu.2,eat,bathe,"in"));
# defecation followed by bathing
def.bt.0 <- cbind(pathstrength(alajo.0,defec,bathe,"out"),
                  pathstrength(bukom.0,defec,bathe,"out"),
                  pathstrength(oldfadama.0,defec,bathe,"out"),
                  pathstrength(shiabu.0,defec,bathe,"out"));
def.bt.1 <- cbind(pathstrength(alajo.1,defec,bathe,"out"),
                  pathstrength(bukom.1,defec,bathe,"out"),
                  pathstrength(oldfadama.1,defec,bathe,"out"),
                  pathstrength(shiabu.1,defec,bathe,"out"));
def.bt.2 <- cbind(pathstrength(alajo.2,defec,bathe,"out"),
                  pathstrength(bukom.2,defec,bathe,"out"),
                  pathstrength(oldfadama.2,defec,bathe,"out"),
                  pathstrength(shiabu.2,defec,bathe,"out"));

# connected or not: subcomponent(graph,vertex,mode=c("in","out","all")
# or: neighbors(graph,vertex1, vertex2,mode-c("in","out","all")
# closeness(alajo)[handw]
# betweenness(alajo)[handw]
# degree(alajo,handw,"in")
# authority.score(alajo)$vector
# hub.score(alajo)$vector
