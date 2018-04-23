pathstrength <- function(nw,from.id,to.id,direction){
  from.list <- get.node(nw$nodes,from.id);
  to.list <- get.node(nw$nodes,to.id);
  wt.sel <- 0;
  wt.nonsel <- 0;
  for(k.from in 1:length(from.list)){
    nbr <- neighbors(nw$graph,from.list[k.from],direction);
    sel <- intersect(nbr,to.list);
    nonsel <- setdiff(nbr,to.list);
    if(direction=="out" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(nw$graph,P=c(from.list[k.from],sel[k.to]))$weight;
      }
    }
    if(direction=="out" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(nw$graph,P=c(from.list[k.from],nonsel[k.to]))$weight;
      }
    }
    if(direction=="in" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(nw$graph,P=c(sel[k.to],from.list[k.from]))$weight;
      }
    }
    if(direction=="in" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(nw$graph,P=c(nonsel[k.to],from.list[k.from]))$weight;
      }
    }
  }
  return(c(wt.sel,wt.nonsel));
}

# get node in network by its identifier:
# for a child, either a single id may be given for behaviour
# or two ids may be given, enter these as c(compartment,behaviour)!
# or a list of (comp,behav) pairs may be given
get.node <- function(nodelist,id){
  if(length(id)==1) nodes <- grep(id,nodelist);
  if(length(id)==2){
    idstr <- paste(substr(id[1],start=1,stop=1),id[2],sep="");
    nodes <- grep(idstr,nodelist);
  }
  if(length(id) >2){ # assume columns with list of node ids
    nodes <- numeric(0);
    for(k in 1:nrow(id)){
      idstr <- paste(substr(id[1,k],start=1,stop=1),id[2,k],sep="");
      nodes <- c(nodes,grep(idstr,nodelist));
    }
  }
  return(nodes);
}

# child behav  child compt    caregiver behav
play  <- " 1"; dirt  <- "1 "; cg.handw <- "a";
sleep <- " 2"; floor <- "2 "; cg.food  <- "b";
handw <- " 3"; offgr <- "3 "; cg.disp  <- "c";
bathe <- " 4"; swata <- "4 "; cg.latr  <- "d";
defec <- " 5"; drain <- "5 "; cg.pay   <- "e";
eat   <- " 6"; oovc  <- "6 "; cg.sweep <- "f";
oovb  <- " 7";

# eating when first washed hands
eat.hw <- cbind(pathstrength(alajo.nw,eat,handw,"in"),
                pathstrength(bukom.nw,eat,handw,"in"),
                pathstrength(oldfadama.nw,eat,handw,"in"),
                pathstrength(shiabu.nw,eat,handw,"in"));
# defecation followed by handwashing
def.hw <- cbind(pathstrength(alajo.nw,defec,handw,"out"),
                pathstrength(bukom.nw,defec,handw,"out"),
                pathstrength(oldfadama.nw,defec,handw,"out"),
                pathstrength(shiabu.nw,defec,handw,"out"));
# eating when first bathed
eat.bt <- cbind(pathstrength(alajo.nw,eat,bathe,"in"),
                pathstrength(bukom.nw,eat,bathe,"in"),
                pathstrength(oldfadama.nw,eat,bathe,"in"),
                pathstrength(shiabu.nw,eat,bathe,"in"));
# defecation followed by bathing
def.bt <- cbind(pathstrength(alajo.nw,defec,bathe,"out"),
                pathstrength(bukom.nw,defec,bathe,"out"),
                pathstrength(oldfadama.nw,defec,bathe,"out"),
                pathstrength(shiabu.nw,defec,bathe,"out"));

# eating after having been offground (not defecating)
eat.og <-
    cbind(pathstrength(alajo.nw,eat,cbind(c(offgr,play),
                                          c(offgr,sleep),
                                          c(offgr,handw),
                                          c(offgr,bathe)),"in"),
          pathstrength(bukom.nw,eat,cbind(c(offgr,play),
                                          c(offgr,sleep),
                                          c(offgr,handw),
                                          c(offgr,bathe)),"in"),
          pathstrength(oldfadama.nw,eat,cbind(c(offgr,play),
                                          c(offgr,sleep),
                                          c(offgr,handw),
                                          c(offgr,bathe)),"in"),
          pathstrength(shiabu.nw,eat,cbind(c(offgr,play),
                                          c(offgr,sleep),
                                          c(offgr,handw),
                                          c(offgr,bathe)),"in"));
# eating when played on dirt or floor
eat.pf <-
    cbind(pathstrength(alajo.nw,eat,cbind(c(dirt,play),c(floor,play)),"in"),
          pathstrength(bukom.nw,eat,cbind(c(dirt,play),c(floor,play)),"in"),
          pathstrength(oldfadama.nw,eat,cbind(c(dirt,play),c(floor,play)),"in"),
          pathstrength(shiabu.nw,eat,cbind(c(dirt,play),c(floor,play)),"in"));
# eating when played in drain or swata
eat.pd <-
    cbind(pathstrength(alajo.nw,eat,cbind(c(drain,play),c(swata,play)),"in"),
          pathstrength(bukom.nw,eat,cbind(c(drain,play),c(swata,play)),"in"),
          pathstrength(oldfadama.nw,eat,cbind(c(drain,play),c(swata,play)),"in"),
          pathstrength(shiabu.nw,eat,cbind(c(drain,play),c(swata,play)),"in"));

# eating when first defecated
eat.df <- cbind(pathstrength(alajo.nw,eat,defec,"in"),
                pathstrength(bukom.nw,eat,defec,"in"),
                pathstrength(oldfadama.nw,eat,defec,"in"),
                pathstrength(shiabu.nw,eat,defec,"in"));
