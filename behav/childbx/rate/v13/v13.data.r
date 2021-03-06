library(foreign)
version <- "v13";
setwd("~/stat/sanipath/behav/childbx/rate/v13/")
source(paste(version,"auxfunc","r",sep="."));

#subset <- c("hh",2);
# choose subset: subset <- c(domain,agecat)
# where domain = "hh" or "ns"
# and agecat = 0, 1, or 2 (years)

here <- getwd();
setwd("~/stat/sanipath/data/sanipath24092013/");
childbx <- read.dta("Bx081513.dta", convert.factors=TRUE,
         convert.underscore=TRUE, warn.missing.labels=TRUE)
setwd(here);

dates <- as.character(childbx$be.date);
start <- as.character(childbx$be.stc);
times.start <- as.character(childbx$be.st);
times.end <- as.character(childbx$be.et);

start1 <- as.POSIXlt(strptime(paste(dates,start),"%m/%d/%Y %H:%M"));
start0 <- as.POSIXlt(strptime(paste(dates,times.start),"%m/%d/%Y %H:%M"));
end0 <- as.POSIXlt(strptime(paste(dates,times.end),"%m/%d/%Y %H:%M"));

neighb <- childbx$neighb;
neighb.levels <- levels(neighb);
n.neighb <- nlevels(neighb);
dom <- childbx$dom; # Household (2) or Nursery (13)
household.domain <- 2;
nursery.domain   <- 13;

# find observations of individual children
# by location ID (locid) and child ID (chid).
locid <- factor(childbx$locid);
chid <- factor(childbx$chid);
ntot <- length(childbx[,1]);
index <- 1:ntot;

locidlevels <- levels(locid);
n0.subj <- rep(0,n.neighb);
nmax.obs <- 1;
for(k.neighb in 1:n.neighb) {
  for(k.locid in 1:nlevels(locid)) {
    loc0 <- index[locid==locidlevels[k.locid] & neighb==neighb.levels[k.neighb]];
    if(length(loc0)!=0) {
      chloc <- factor(chid[loc0[!is.na(loc0)]]);
      chlevels <- nlevels(chloc);
      nmax.obs <- max(nmax.obs,length(loc0));
      n0.subj[k.neighb] <- n0.subj[k.neighb] + chlevels;
    }
  }
}
n0max.subj <- max(n0.subj);

ind0.subj <- array(NA,dim=c(n.neighb,n0max.subj,nmax.obs));
n0.obs <- array(NA,dim=c(n.neighb,n0max.subj));
for(k.neighb in 1:n.neighb) {
  k.subj <- 1;
  for(k.locid in 1:nlevels(locid)) {
    loc0 <- index[locid==locidlevels[k.locid] & neighb==neighb.levels[k.neighb]];
    if(length(loc0)!=0) {
      chloc <- factor(chid[loc0[!is.na(loc0)]]);
      chlevels <- levels(chloc);
      for(m0 in 1:nlevels(chloc)) {
        loc1 <- loc0[chloc==chlevels[m0]];
        n0.obs[k.neighb,k.subj] <- length(loc1);
        for(m1 in 1:n0.obs[k.neighb,k.subj]) {
          ind0.subj[k.neighb,k.subj,m1] <- loc1[m1];
        }
        k.subj <- k.subj + 1;
      }
    }
  }
}
n.comp <- 5;

empty.record <- function(k.neighb,k.subj){
  has.comp  <- which(!is.na(childbx$be.area[ind0.subj[k.neighb,k.subj,]]));
  has.play  <- which(!is.na(childbx$be.play[ind0.subj[k.neighb,k.subj,]]));
  has.sleep <- which(!is.na(childbx$be.sleep[ind0.subj[k.neighb,k.subj,]]));
  has.handw <- which(!is.na(childbx$be.chw[ind0.subj[k.neighb,k.subj,]]));
  has.bathe <- which(!is.na(childbx$be.cbt[ind0.subj[k.neighb,k.subj,]]));
  has.defec <- which(!is.na(childbx$be.cdf[ind0.subj[k.neighb,k.subj,]]));
  has.eat   <- which(!is.na(childbx$be.ce[ind0.subj[k.neighb,k.subj,]]));
  if(length(has.comp)==0 & length(has.play)==0 & length(has.sleep)==0 &
     length(has.handw)==0 & length(has.bathe)==0 & length(has.defec)==0 &
     length(has.eat)==0) return(TRUE);
  return(FALSE);
}

n.subj <- rep(NA,n.neighb);
for(k.neighb in 1:n.neighb) {
  k.subj <- 0;
  for(k0.subj in 1: n0.subj[k.neighb]) {
    if(!empty.record(k.neighb,k0.subj)) k.subj <- k.subj + 1;
    }
  n.subj[k.neighb] <- k.subj;
}
nmax.subj <- max(n.subj);
ind.subj <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
n.obs <- array(NA,dim=c(n.neighb,nmax.subj));
for(k.neighb in 1:n.neighb) {
  k.subj <- 0;
  for(k0.subj in 1: n0.subj[k.neighb]) {
    if(!empty.record(k.neighb,k0.subj)){
      k.subj <- k.subj + 1;
      ind.subj[k.neighb,k.subj,] <- ind0.subj[k.neighb,k0.subj,];
      n.obs[k.neighb,k.subj] <- n0.obs[k.neighb,k0.subj];
    }
  }
}

#####################################################################
# coding errors??
end0[ind.subj[1,42,1:n.obs[1,42]]]   <- "2012-08-02 10:50:00";
start0[ind.subj[2,24,1:n.obs[2,24]]] <- "2012-03-08 08:00:00";
end0[ind.subj[3,30,1:n.obs[3,30]]]   <- "2012-06-20 11:21:00";
start0[ind.subj[4,1,1:n.obs[4,1]]]   <- "2012-03-13 13:05:00";
#####################################################################

# sort observations in order of increasing start time
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    ind.subj[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]] <-
      ind.subj[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][
        order(start1[ind.subj[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]]]-
              start0[ind.subj[k.neighb,k.subj,1]])];
  }
}

#####################################################################
# coding errors??
start1[ind.subj[3,8,n.obs[3,8]]]     <- "2012-03-24 10:30:00";
#####################################################################

# durations of activities...
active.period <- 12*60; # minutes
dur.obs <- array(NA,dim=c(n.neighb,nmax.subj));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    dur.obs[k.neighb,k.subj] <-
      difftime(end0[ind.subj[k.neighb,k.subj,1]],
               start0[ind.subj[k.neighb,k.subj,1]],
               units="mins");
    # minimum duration 5 minutes?
    if(is.na(dur.obs[k.neighb,k.subj])) dur.obs[k.neighb,k.subj] <- 5.0;
  }
}
time.behav <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    for(k.obs in 1:n.obs[k.neighb,k.subj]-1) {
      time.behav[k.neighb,k.subj,k.obs] <-
        difftime(start1[ind.subj[k.neighb,k.subj,k.obs+1]],
                 start1[ind.subj[k.neighb,k.subj,k.obs]],
                 units="mins");
    }
    time.behav[k.neighb,k.subj,n.obs[k.neighb,k.subj]] <-
      difftime(end0[ind.subj[k.neighb,k.subj,n.obs[k.neighb,k.subj]]],
               start1[ind.subj[k.neighb,k.subj,n.obs[k.neighb,k.subj]]],
               units="mins");
  }
}
# several actions were recorded with duration 0;
# these are concurrent actions: same duration
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    for(k.obs in 2:n.obs[k.neighb,k.subj]) {
      if(!is.na(time.behav[k.neighb,k.subj,k.obs])) {
        if(time.behav[k.neighb,k.subj,k.obs]==0) {
          time.behav[k.neighb,k.subj,k.obs] <-
            time.behav[k.neighb,k.subj,k.obs-1];
        }
      }
    }
  }
}

ttfrac <- array(NA,dim=c(n.neighb,nmax.subj));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    ttfrac[k.neighb,k.subj] <-
      dur.obs[k.neighb,k.subj]/(active.period*active.period);
  }
}
# precision of time measurements: var.time = tau.time * mu.time^2
tau.time <- 1;

#########################################################################
childbx$be.play[ind.subj[3,13,22]] <- 0; # was 1
childbx$be.cdf[ind.subj[3,13,22]] <- 1;  # was 4
#########################################################################
n.behav <- 6; # for now: will be changed below
obs.behav <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
eat.attr <-array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    for(k.obs in 1:n.obs[k.neighb,k.subj]) {
      tmp <- behaviour(childbx$be.play[ind.subj[k.neighb,k.subj,k.obs]],
                       childbx$be.sleep[ind.subj[k.neighb,k.subj,k.obs]],
                       childbx$be.chw[ind.subj[k.neighb,k.subj,k.obs]],
                       childbx$be.cbt[ind.subj[k.neighb,k.subj,k.obs]],
                       childbx$be.cdf[ind.subj[k.neighb,k.subj,k.obs]],
                       childbx$be.ce[ind.subj[k.neighb,k.subj,k.obs]]);
      if(tmp[6]==1 & sum(tmp)==2) {
        eat.attr[k.neighb,k.subj,k.obs] <- sum(tmp[1:n.behav-1]*(1:5));
        tmp <- c(0,0,0,0,0,1);
      }
      if(sum(tmp)==0) {
        tmp <- NA;
      } else {
        tmp <-sum(tmp*(1:n.behav));
      }
      obs.behav[k.neighb,k.subj,k.obs] <- tmp;
    }
  }
}

obs.comp <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    for(k.obs in 1:n.obs[k.neighb,k.subj]) {
      obs.comp[k.neighb,k.subj,k.obs] <-
        compartment(childbx$be.area[ind.subj[k.neighb,k.subj,k.obs]]);
    }
  }
}

# out of view...
# for(k.neighb in 1:n.neighb) {
#   for(k.subj in 1:n.subj[k.neighb]) {
#     no.behav <- is.na(obs.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]]);
#     no.comp <- is.na(obs.comp[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]]);
#     unseen <- (no.behav | no.comp);
#     obs.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][unseen] <- 7;
#     obs.comp[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][unseen] <- 6;
#   }
# }

# n.behav <- 7; # removed added an additional behaviour for out of view
# n.comp <- 6;  # removed added an additional compartment for out of view

# instead, remove any missing data
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    use <- !(is.na(obs.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]])|
             is.na(time.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]])|
             is.na(obs.comp[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]]));
    tmp <- obs.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][use];
    if(length(tmp)!=0) {
      obs.behav[k.neighb,k.subj,] <- rep(NA,nmax.obs);
      obs.behav[k.neighb,k.subj,1:length(tmp)] <- tmp;
      tmp <- time.behav[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][use];
      time.behav[k.neighb,k.subj,] <- rep(NA,nmax.obs);
      time.behav[k.neighb,k.subj,1:length(tmp)] <- tmp;
      tmp <- obs.comp[k.neighb,k.subj,1:n.obs[k.neighb,k.subj]][use];
      obs.comp[k.neighb,k.subj,] <- rep(NA,nmax.obs);
      obs.comp[k.neighb,k.subj,1:length(tmp)] <- tmp;
      len <- length((1:n.obs[k.neighb,k.subj])[use]);
      n.obs[k.neighb,k.subj] <- len;
    }else{
      n.obs[k.neighb,k.subj] <- 0;
    }
  }
}

n.global.attr <- 6;
obs.global.attr <- array(NA,dim=c(n.neighb,nmax.subj,n.global.attr));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    obs.global.attr[k.neighb,k.subj, 1] <-
      childbx$be.cage[ind.subj[k.neighb,k.subj,1]];
    obs.global.attr[k.neighb,k.subj, 2] <-
      childbx$be.sex[ind.subj[k.neighb,k.subj,1]];
    obs.global.attr[k.neighb,k.subj, 3:5] <-
      mob.score(childbx$be.mob[ind.subj[k.neighb,k.subj,1]]);
    obs.global.attr[k.neighb,k.subj, 6] <-
      childbx$be.teeth[ind.subj[k.neighb,k.subj,1]];
  }
}
n.global.freq <- 4;
obs.global.freq <- array(NA,dim=c(n.neighb,nmax.subj,n.global.freq));
obs.global.cens <- array(FALSE,dim=c(n.neighb,nmax.subj,n.global.freq));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    obs.global.freq[k.neighb,k.subj,1] <-
      childbx$be.cchld[ind.subj[k.neighb,k.subj,1]];
    obs.global.freq[k.neighb,k.subj,2] <-
      childbx$be.cadlt[ind.subj[k.neighb,k.subj,1]];
    obs.global.freq[k.neighb,k.subj,3] <-
      childbx$be.fmth[ind.subj[k.neighb,k.subj,1]];
    obs.global.freq[k.neighb,k.subj,4] <-
      childbx$be.omth[ind.subj[k.neighb,k.subj,1]];
  }
}
obs.global.init <- obs.global.freq;
for(k.freq in 1:n.global.freq) {
  obs.global.cens[,,k.freq][!is.na(obs.global.freq[,,k.freq]) &
                      obs.global.freq[,,k.freq]==999] <- TRUE;
  obs.global.freq[,,k.freq][!is.na(obs.global.freq[,,k.freq]) &
                      obs.global.freq[,,k.freq]==999] <- NA;
  obs.global.init[,,k.freq][!is.na(obs.global.freq[,,k.freq]) &
                      obs.global.freq[,,k.freq]!=999] <- NA;
}
censorlimit <- 100;
behav.tally <- c("children","adults","fingers","objects");

n.attr <- c(2,0,1,1,3,3,0);
num.attr <- sum(n.attr);
max.attr.lev <- 5;
max.attr <- max(n.attr);
ind.attr <- array(NA,dim=c(n.behav,max.attr,max.attr.lev));
ind.attr[1,1,] <- c( 1, 2,NA,NA,NA);
ind.attr[1,2,] <- c( 3, 4,NA,NA,NA);
ind.attr[3,1,] <- c( 5, 6,NA,NA,NA);
ind.attr[4,1,] <- c( 7, 8,NA,NA,NA);
ind.attr[5,1,] <- c( 9,10,11,12,13);
ind.attr[5,2,] <- c(14,15,NA,NA,NA);
ind.attr[5,3,] <- c(16,17,18,19,NA);
ind.attr[6,1,] <- c(20,21,22,23,NA);
ind.attr[6,2,] <- c(24,25,NA,NA,NA);
ind.attr[6,3,] <- c(26,27,28,29,30);

n.attr.param <- 30;
ind.mask <- array(NA,dim=c(n.behav,max.attr));
ind.mask[1,] <- c(1, 2,NA);
ind.mask[3,] <- c(3,NA,NA);
ind.mask[4,] <- c(4,NA,NA);
ind.mask[5,] <- c(5, 6, 7);
ind.mask[6,] <- c(8, 9,10);

obs.attr <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs,n.behav,max.attr));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    if(n.obs[k.neighb,k.subj] > 0){
      for(k.obs in 1:n.obs[k.neighb,k.subj]) {
        obs.attr[k.neighb,k.subj,k.obs,1,1] <-
          yesno(childbx$be.hands[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,1,2] <-
          yesno(childbx$be.shoes[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,3,1] <-
          soap(childbx$be.chw1[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,4,1] <-
          soap(childbx$be.cbs[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,5,1] <-
          childbx$be.cdf1[ind.subj[k.neighb,k.subj,k.obs]];
        obs.attr[k.neighb,k.subj,k.obs,5,2] <-
          wipe(childbx$be.cdf2[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,5,3] <-
          dispose(childbx$be.cdf3[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,6,1] <-
          food(childbx$be.ce1[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,6,2] <-
          cutlery(childbx$be.ce2[ind.subj[k.neighb,k.subj,k.obs]]);
        obs.attr[k.neighb,k.subj,k.obs,6,3] <-
          eat.attr[k.neighb,k.subj,k.obs];
      }
    }
  }
}

global.attributes <- c("age","gender","crawl","walk","sit","teeth");
global.attribute.levels <- array(NA,dim=c(n.global.attr,3));
global.attribute.levels[1,] <- c("<1","1-2",">2");
global.attribute.levels[2,] <- c("F","M",NA);
global.attribute.levels[3,] <- c("no","yes",NA);
global.attribute.levels[4,] <- c("no","yes",NA);
global.attribute.levels[5,] <- c("no","yes",NA);
global.attribute.levels[6,] <- c("no","yes",NA);

# "attr.mask" indicating attributes of behaviour not recorded.
global.attr1.mask  <- c(1,1,1);
global.attr2.mask  <- c(1,1,0);
global.attr3.mask  <- c(1,1,0);
global.attr4.mask  <- c(1,1,0);
global.attr5.mask  <- c(1,1,0);
global.attr6.mask  <- c(1,1,0);

global.attr.mask <- array(c(diag(global.attr1.mask),
                            diag(global.attr2.mask),
                            diag(global.attr3.mask),
                            diag(global.attr4.mask),
                            diag(global.attr5.mask),
                            diag(global.attr6.mask)),
                          ,dim=c(3,3,n.global.attr));

neighbourhoods <- c("alajo","bukom","old-fadama","shiabu")
# compartments <-c("dirt","floor","off grnd","SWATA","drain","out of view");
compartments <-c("dirt","floor","off grnd","SWATA","drain");
# behaviours <- c("play/sit","sleep","handw","bathe","defec","eat","out of view");
behaviours <- c("play/sit","sleep","handw","bathe","defec","eat");
attributes <- c("touch w. hands","wear shoes","w. soap","w. soap","where","wipe",
                "dispose","food","cutlery","w. action");
attribute.levels <- array(NA,dim=c(num.attr,max.attr.lev));
attribute.levels[ 1,] <- c("no","yes",NA,NA,NA);
attribute.levels[ 2,] <- c("no","yes",NA,NA,NA);
attribute.levels[ 3,] <- c("no","yes",NA,NA,NA);
attribute.levels[ 4,] <- c("no","yes",NA,NA,NA);
attribute.levels[ 5,] <- c("ground","latrine","bag","potty","drain");
attribute.levels[ 6,] <- c("no","yes",NA,NA,NA);
attribute.levels[ 7,] <- c("latrine","drain","trash","ground",NA);
attribute.levels[ 8,] <- c("prep at home","bought","raw produce","DK",NA);
attribute.levels[ 9,] <- c("no","yes",NA,NA,NA);
attribute.levels[10,] <- c("play","sleep","handw","bathe","defec");

# "attr.mask" indicating attributes of behaviour not recorded.
attr1.mask  <- c(1,1,0,0,0);
attr2.mask  <- c(1,1,0,0,0);
attr3.mask  <- c(1,1,0,0,0);
attr4.mask  <- c(1,1,0,0,0);
attr5.mask  <- c(1,1,1,1,1);
attr6.mask  <- c(1,1,0,0,0);
attr7.mask  <- c(1,1,1,1,0);
attr8.mask  <- c(1,1,1,1,0);
attr9.mask  <- c(1,1,0,0,0);
attr10.mask <- c(1,1,1,1,1);

attr.mask <- array(c(diag(attr1.mask),diag(attr2.mask),diag(attr3.mask),
                     diag(attr4.mask),diag(attr5.mask),diag(attr6.mask),
                     diag(attr7.mask),diag(attr8.mask),diag(attr9.mask),
                     diag(attr10.mask)),
                    ,dim=c(max.attr.lev,max.attr.lev,num.attr));


# Figure out which behaviours are never observed and set corresponding
# lambda's to zero.
comp1.mask <- c(1,0,1,1,1,1);
comp2.mask <- c(1,1,1,1,1,1);
comp3.mask <- c(1,1,1,1,1,1);
comp4.mask <- c(1,0,0,0,0,1);
comp5.mask <- c(1,0,0,0,0,0);
comp.mask <- rbind(comp1.mask,comp2.mask,comp3.mask,
                   comp4.mask,comp5.mask);

############################################################################
# times missing...
for(k.neighb in 1:n.neighb){
  for(k.subj in 1:n.subj[k.neighb]){
    if(n.obs[k.neighb,k.subj] > 0){
      for(k.obs in 1:n.obs[k.neighb,k.subj]){
        if(is.na(time.behav[k.neighb,k.subj,k.obs]))
          time.behav[k.neighb,k.subj,k.obs] <- 5;
      }
    }
  }
}
############################################################################

# Retrieve location and child ID by index[neighbourhood,subject,observation]
get.chid <- function(cgidstring){
  cgvec <- c("cg1","cg2","cg3");
  chvec <- c("ch1","ch2","ch3");
  cg <- rep(NA,3);
  ch <- rep(NA,3);
  for(k in 1:length(cgvec)){
    if(grepl(cgvec[k],cgidstring,ignore.case=TRUE)) cg[k]<-k;
    if(grepl(chvec[k],cgidstring,ignore.case=TRUE)) ch[k]<-k;
  }
  cg <- cg[!is.na(cg)];
  if(length(cg)==0) cg <- NA;
  ch <- ch[!is.na(ch)];
  if(length(ch)==0) ch <- NA;
  return(c(cg,ch));
}

ch.id <- array(NA,dim=c(n.neighb,nmax.subj,3));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    ch.id[k.neighb,k.subj,1] <- locid[ind.subj[k.neighb,k.subj,1]];
    ch.id[k.neighb,k.subj,2:3] <-
      get.chid(as.character(chid[ind.subj[k.neighb,k.subj,1]]));
  }
}
loclist <- levels(locid);
chidlist <- levels(chid);

# mu.logr <- c(0,0,0,0);
# sd.logr <- c(6,6,6,6);
# mu.loglambda <- c(0,0,0,0);
# sd.loglambda <- c(6,6,6,6);

logr           <-  1.5; # 2.303;
tau.loglambda  <-  4.0;
mu0.loglambda  <-  6.0;
tau0.loglambda <-  0.1;
zeros <- array(0,dim=c(n.neighb,nmax.subj,nmax.obs));

# select subset data and prepare for JAGS
# domain: household.domain or nursery.domain
# agecat: 1 (0<age<1), 2 (1<age<2), or 3 (age>2)
if(subset[1]=="hh") domain <- household.domain;
if(subset[1]=="ns") domain <- nursery.domain;
if(subset[2]==0) agecat <- 1;
if(subset[2]==1) agecat <- 2;
if(subset[2]==2) agecat <- 3;
sel.subj <- rep(NA,n.neighb);
for(k.neighb in 1:n.neighb){
  k.sel <- 0;
  for(k.subj in 1:n.subj[k.neighb]){
    if(!is.na(dom[ind.subj[k.neighb,k.subj,1]]) &
       !is.na(obs.global.attr[k.neighb,k.subj,1])){
      if(dom[ind.subj[k.neighb,k.subj,1]]==domain &
         obs.global.attr[k.neighb,k.subj,1]==agecat &
         n.obs[k.neighb,k.subj] > 1) k.sel <- k.sel + 1; # need at least 2
    }
  }
  sel.subj[k.neighb] <- k.sel;
}

max.subj <- max(sel.subj);
sel.obs <- array(NA,dim=c(n.neighb,max.subj));
sel.comp  <- array(NA,dim=c(n.neighb,max.subj,nmax.obs));
sel.behav <- array(NA,dim=c(n.neighb,max.subj,nmax.obs));
sel.time  <- array(NA,dim=c(n.neighb,max.subj,nmax.obs));
sel.zeros  <- array(0,dim=c(n.neighb,max.subj,nmax.obs));
for(k.neighb in 1:n.neighb){
  k.sel <- 0;
  for(k.subj in 1:n.subj[k.neighb]){
    if(!is.na(dom[ind.subj[k.neighb,k.subj,1]]) &
       !is.na(obs.global.attr[k.neighb,k.subj,1])){
      if(dom[ind.subj[k.neighb,k.subj,1]]==domain &
         obs.global.attr[k.neighb,k.subj,1]==agecat &
         n.obs[k.neighb,k.subj] > 1){ # need at least 2
        k.sel <- k.sel + 1;
        sel.obs[k.neighb,k.sel] <- n.obs[k.neighb,k.subj];
        sel.comp[k.neighb,k.sel,] <- obs.comp[k.neighb,k.subj,];
        sel.behav[k.neighb,k.sel,] <- obs.behav[k.neighb,k.subj,];
        sel.time[k.neighb,k.sel,] <- time.behav[k.neighb,k.subj,];
      }
    }
  }
}

sel.data <- list("n.neighb"=n.neighb,"n.comp"=n.comp,
                 "n.subj"=sel.subj,"n.obs"=sel.obs,
                 "n.behav"=n.behav, "obs.comp"=sel.comp,
                 "comp.mask"=comp.mask,
                 "obs.behav"=sel.behav,
                 "obs.time"=sel.time,"zeros"=sel.zeros,
                 "logr"=logr,"tau.loglambda"=tau.loglambda,
                 "mu0.loglambda"=mu0.loglambda,
                 "tau0.loglambda"=tau0.loglambda);

sel.time.obs <- array(NA,dim=c(n.neighb,max.subj));
for(k.neighb in 1:n.neighb){
  for(k.subj in 1:sel.subj[k.neighb]){
    sel.time.obs[k.neighb,k.subj] <- sum(na.omit(sel.time[k.neighb,k.subj,]));
  }
}
