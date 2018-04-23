beh.seq <- array(NA,dim=c(30*30,2));
k.cnt <- 1;
for(k.oldcomp in 1:5){ # old compartments
  for(k.oldbehav in 1:6){ # old behaviours
    for(k.newcomp in 1:5){ # new compartments
      for(k.newbehav in 1:6){ # new behaviours
        beh.seq[k.cnt,] <- c(paste(k.oldcomp,k.oldbehav),
                             paste(k.newcomp,k.newbehav));
        k.cnt <- k.cnt + 1;
      }
    }
  }
}

test.nw <- make.nw(beh.seq,oov=FALSE,loops=FALSE);

test.eat.hw <- pathstrength(test.nw,eat,handw,"in");
test.def.hw <- pathstrength(test.nw,defec,handw,"out");
test.eat.bt <- pathstrength(test.nw,eat,bathe,"in");
test.def.bt <- pathstrength(test.nw,defec,bathe,"out");
test.eat.og <- pathstrength(test.nw,eat,cbind(c(offgr,play),
                                              c(offgr,sleep),
                                              c(offgr,handw),
                                              c(offgr,bathe)),"in");
test.eat.pf <- pathstrength(test.nw,eat,cbind(c(dirt,play),c(floor,play)),"in");
test.eat.pd <- pathstrength(test.nw,eat,cbind(c(drain,play),c(swata,play)),"in");
test.eat.df <- pathstrength(test.nw,eat,defec,"in");
