ver <- "v15";
library(igraph);

colch <- c("dark gray","light gray","white","pink","red","black");
colcg <- "white";
labch <- c("play","slp","hw","bath","def","eat","oov");
labcg <- c("hw","food","disp","latr","pay","swp");
alpha <- c("a","b","c","d","e","f");

chpos <- array(c(1.0,2.0,1.0,0.0,6.0,3.0,
                 1.5,2.5,1.5,0.0,6.0,3.0,
                 2.5,3.5,2.5,0.0,6.0,3.0,
                 3.5,4.0,3.5,0.0,6.0,3.0,
                 4.5,3.5,4.5,0.0,6.0,3.0,
                 5.5,2.5,5.0,0.0,6.0,3.0,
                 3.0,3.0,3.0,3.0,3.0,3.0,
                 2.2,3.0,3.8,0.5,0.5,3.0,
                 1.2,4.0,4.7,1.5,1.5,3.0,
                 0.5,4.0,5.5,2.5,2.5,3.0,
                 0.5,3.0,5.5,3.5,3.5,3.0,
                 1.2,2.0,4.7,4.5,4.5,3.0,
                 2.2,2.0,3.8,5.5,5.5,3.0,
                 3.0,3.0,3.0,3.0,3.0,3.0),dim=c(6,7,2));

cgpos <- array(c(1.0,2.0,3.0,4.0,5.0,6.0,
                 7.0,7.0,7.0,7.0,7.0,7.0),dim=c(6,2));

chpos <- (chpos-3.5)/3.5;
cgpos <- (cgpos-3.5)/3.5;

del.oov <- function(seq){
  accept.from <- grepl("[1-5] [1-6]",seq[,1]);
  accept.to   <- grepl("[1-5] [1-6]",seq[,2]);
  return(seq[(accept.from & accept.to),]);
}

del.loop <- function(seq){
  return(seq[seq[,1]!=seq[,2],]);
}

label.node <- function(node){
  cgnode <- which(alpha==node);
  if (length(cgnode)==1) return(labcg[cgnode]);
  chnode <- c(as.numeric(substr(node,1,1)),as.numeric(substr(node,3,3)));
  chnode <- labch[chnode[2]];
  return(chnode);
}

colour.node <- function(node){
  cgnode <- which(alpha==node);
  if (length(cgnode)==1) return(colcg);
  chnode <- c(as.numeric(substr(node,1,1)),as.numeric(substr(node,3,3)));
  chnode <- colch[chnode[1]];
  return(chnode);
}

shape.node <- function(node){
  cgnode <- which(alpha==node);
  if (length(cgnode)==1) return("square");
  chnode <- c(as.numeric(substr(node,1,1)),as.numeric(substr(node,3,3)));
  chnode <- "circle";
  return(chnode);
}

coord.node <- function(node){
  cgnode <- which(alpha==node);
  if(length(cgnode)==1) return(cgpos[cgnode,]);
  chnode <- c(as.numeric(substr(node,1,1)),as.numeric(substr(node,3,3)));
  return(chpos[chnode[1],chnode[2],]);
}

xcoord.nodes <- function(nodelist){
  xc <- rep(NA,length(nodelist));
  for(k in 1:length(nodelist)) xc[k] <- coord.node(nodelist[k])[1];
  return(xc);
}

ycoord.nodes <- function(nodelist){
  yc <- rep(NA,length(nodelist));
  for(k in 1:length(nodelist)) yc[k] <- coord.node(nodelist[k])[2];
  return(yc);
}

vert.labels <- function(nodelist){
  ndlen <- length(nodelist);
  labels <- rep("",ndlen);
  for(k in 1:ndlen){
    labels[k] <- label.node(nodelist[k]);
  }
  return(labels);
}

vert.colours <- function(nodelist){
  ndlen <- length(nodelist);
  colours <- rep("",ndlen);
  for(k in 1:ndlen){
    colours[k] <- colour.node(nodelist[k]);
  }
  return(colours);
}

vert.shapes <- function(nodelist){
  ndlen <- length(nodelist);
  shapes <- rep("",ndlen);
  for(k in 1:ndlen){
    shapes[k] <- shape.node(nodelist[k]);
  }
  return(shapes);
}

make.sequence <- function(k.neighb,n.sim){
  old.comp <- 3;
  old.behav <- 2;
  bc <- array(NA,dim=c(n.sim,2));
  for(k.sim in 1:n.sim){
    newstate <- gen.behav(k.neighb,old.comp,old.behav);
    new.comp <- newstate[1];
    new.behav <- newstate[2];
    bc[k.sim,] <- c(paste(old.comp,old.behav),paste(new.comp,new.behav));
    old.comp <- new.comp;
    old.behav <- new.behav;
  }
  return(bc);
}

find.nodes <- function(seq){
  return(unique(c(seq)));
}

find.edges <- function(seq){
  ndlist <- find.nodes(seq);
  n.nd <- length(ndlist);
  edg <- array(NA,dim=c(n.nd*n.nd,3));
  for(k.from in 1:n.nd){
    for(k.to in 1:n.nd){
      es <- length(seq[(seq[,1]==ndlist[k.from] & seq[,2]==ndlist[k.to]),1]);
      edg[(k.from-1)*n.nd + k.to,] <- c(k.from,k.to,es);
    }
  }
  return(edg[edg[,3]!=0,1:3]);
}

make.edgeweights <- function(bs){
  edg.mat <- find.edges(bs);
  edg.wgt  <- edg.mat[,3];
  return(edg.wgt);
}

make.edgecolors <- function(wlist){
  edg.lev <- 1-wlist/max(wlist);
  return(grey(edg.lev));
}

make.nw <- function(seq,oov,loops){
  bs <- seq;
  if(!oov) bs <- del.oov(seq);
  if(!loops) bs <- del.loop(bs);
  nl <- find.nodes(bs);
  el <- find.edges(bs);
  # el <- el[order(el[,3],decreasing=TRUE),];
  gr <- graph.edgelist(el[,1:2]);
  E(gr)$weight <- make.edgeweights(bs);
  E(gr)$width <- make.edgeweights(bs)/5;
  E(gr)$color <- make.edgecolors(make.edgeweights(bs));
  E(gr)$arrow.size <- 0.5;
  E(gr)$curved <- 0.1;
  V(gr)$name <- vert.labels(nl);
  V(gr)$color <- vert.colours(nl);
  V(gr)$shape <- vert.shapes(nl);
  V(gr)$x <- xcoord.nodes(nl);
  V(gr)$y <- ycoord.nodes(nl);
  nw <- list("nodes"=nl,"edges"=el,"graph"=gr);
  return(nw);
}

# connected or not: subcomponent(graph,vertex,mode=c("in","out","all")
# or: neighbors(graph,vertex1, vertex2,mode-c("in","out","all")
# closeness(alajo)[handw]
# betweenness(alajo)[handw]
# degree(alajo,handw,"in")
# authority.score(alajo)$vector
# hub.score(alajo)$vector
