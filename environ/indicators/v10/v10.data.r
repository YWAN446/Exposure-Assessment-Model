library(foreign)
version <- "v10";

sanibase <- "~/stat/sanipath/"
curwd <-  getwd();
setwd(paste(sanibase,"data/sanipath03112014/",sep=""));
sanidata <- read.dta("Ev110314.dta", convert.factor=TRUE,
		convert.underscore=TRUE, warn.missing.labels=TRUE);
setwd(curwd);

# generate index for the different levels of categorical variables
mkindex <- function(attrvec) {
  tmp <- array(0,dim=c(length(attrvec)));
  lev <- levels(attrvec);
  for(k in 1:length(lev)) {
    tmp <- tmp + k * as.numeric(attrvec==lev[k]);
  }
  return(list(levs=levels(attrvec),index=tmp));
}

barcode <- sanidata$"barcode";
n.data <- length(barcode);

env.tp <- factor(sanidata$"environ");
ind.env.tp <- mkindex(env.tp)$index;
n.env.tp <- length(unique(ind.env.tp));

smp.tp  <- factor(sanidata$"samtype");
ind.smp.tp <- mkindex(smp.tp)$index;
n.smp.tp <- length(unique(ind.smp.tp));

id.smp <- sanidata$"samid";
locid <- sanidata$"locid";
neigh <- sanidata$"neighbor";
# dom <- factor(sanidata$"dom");
# domnum <- sanidata$"domnum";
obs <- sanidata$"obs";
dates <- as.character(sanidata$"ec.sd");
times <- as.character(sanidata$"ec.st");
tdata <- as.POSIXlt(strptime(paste(dates,times),"%m/%d/%Y %H:%M"));
who <- factor(sanidata$"hr.who");

# Collect any available data on sample types...
tmp <- sanidata$"sp.ot";
tmp[tmp==""] <- NA;
septage <- list(type=factor(sanidata$"sp.type"),
                othr=factor(tmp));
tmp <- sanidata$"pa.type";
tmp[is.na(tmp) & smp.tp == "particulate"] <- "soil";
particu <- list(type=factor(tmp));
largewa <- list(type=factor(sanidata$"lw.type"),
                srce=factor(sanidata$"ld.source"));
smalldw <- list(puse=factor(sanidata$"sd.primw"),
                srce=factor(sanidata$"sd.source"),
                sach=factor(sanidata$"sd.s"),
                styp=factor(sanidata$"sd.sach"),
                stor=factor(sanidata$"sd.h"),
                suse=factor(sanidata$"sd.hp1"));
smallev <- list(type=factor(sanidata$"se.sr"),
                srce=factor(sanidata$"se.po"),
                drsz=factor(sanidata$"se.sz"),
                drhh=factor(sanidata$"se.dhh"));
tmp <- sanidata$"fo.typ";
tmp[tmp==""] <- NA;
foods   <- list(type=factor(sanidata$"fo.ftype"),
                prtp=factor(tmp),
                wght=sanidata$"fo.wgt",
                nitm=sanidata$"fo.num");
flies   <- list(type=sanidata$"fl.loc");
handr   <- list(type=sanidata$"hr.who");
swabs   <- list(type=factor(sanidata$"sw.type"),
                drwt=factor(sanidata$"sw.wet"),
                ongd=factor(sanidata$"sw.on.off"),
                area=factor(sanidata$"sw.area"));

ind.septage <- mkindex(septage$type)$index;
ind.particu <- mkindex(particu$type)$index;
tmp <- rep(NA,length(largewa$type));
tmp[largewa$type=="drinking water" & largewa$srce=="public tap"] <- 1;
tmp[largewa$type=="drinking water" & largewa$srce=="compound/private tap"] <- 2;
tmp[largewa$type=="ocean water"] <- 3;
ind.largewa <- tmp;
tmp <- rep(NA,length(smalldw$puse));
tmp[smalldw$srce=="public tap"] <- 1;
tmp[smalldw$srce=="compound/private tap"] <- 2;
tmp[smalldw$sach=="+"] <- 3;
tmp[smalldw$stor=="1" & smalldw$suse=="1"] <- 4;
tmp[smalldw$stor=="1" & smalldw$suse=="0"] <- 5;
ind.smalldw <- tmp;
tmp <- rep(NA,length(smallev));
tmp[smallev$type=="open drain" & smallev$drsz=="small" & smallev$drhh=="+"] <- 1;
tmp[smallev$type=="open drain" & smallev$drsz=="medium" & smallev$drhh=="+"] <- 2;
tmp[smallev$type=="open drain" & smallev$drsz=="large" & smallev$drhh=="+"] <- 3;
tmp[smallev$type=="open drain" & smallev$drsz=="small" & smallev$drhh=="-"] <- 4;
tmp[smallev$type=="open drain" & smallev$drsz=="medium" & smallev$drhh=="-"] <- 5;
tmp[smallev$type=="open drain" & smallev$drsz=="large" & smallev$drhh=="-"] <- 6;
tmp[smallev$type=="other" & smallev$srce=="Ecological Drain"] <- 7;
tmp[smallev$type=="other" & smallev$srce=="Stagnant water"] <- 8;
tmp[smallev$type=="other" & smallev$srce=="lagoon"] <- 9;
tmp[smallev$type=="other" &
    (smallev$srce=="Flood" | smallev$srce=="flood" |
     smallev$srce=="Flood area" | smallev$srce=="flood water" |
     smallev$srce=="Flood water" | smallev$srce=="Flood water Before")] <- 10;
ind.smallev <- tmp;
tmp <- rep(NA,length(foods$type));
tmp[foods$type=="prepared food" & foods$prtp=="salad"] <- 1;
tmp[foods$type=="prepared food" & foods$prtp=="Porridge"] <- 2;
tmp[foods$type=="other" & foods$prtp=="Fish"] <- 3;
tmp[foods$type=="other" & foods$prtp=="waakye"] <- 4;
tmp[foods$type=="other" & foods$prtp=="ONION"] <- 5;
tmp[foods$type=="other" & foods$prtp=="Water Melon"] <- 6;
tmp[foods$type=="other" & foods$prtp=="water melon"] <- 6;
tmp[foods$type=="lettuce"] <- 7;
tmp[foods$type=="tomatoes"] <- 8;
tmp[foods$type=="oranges"] <- 9;
tmp[foods$type=="apples"] <- 10;
tmp[foods$type=="mango"] <- 11;
tmp[foods$type=="chili peppers"] <- 12;
tmp[foods$type=="spring onion"] <- 13;
tmp[foods$type=="cabbage"] <- 14;
ind.foods <- tmp;
tmp <- rep(NA,length(flies$type));
tmp[smp.tp=="flies"] <- 1;
ind.flies <- tmp;
tmp <- rep(NA,length(handr$type));
tmp[smp.tp=="handrinse"] <- 1;
ind.handr <- tmp;
tmp <- rep(NA,length(swabs$type));
tmp[swabs$ongd=="1" & swabs$drwt=="dry"] <- 1;
tmp[swabs$ongd=="2" & swabs$drwt=="dry"] <- 2;
tmp[swabs$ongd=="1" & swabs$drwt=="wet"] <- 3;
tmp[swabs$ongd=="2" & swabs$drwt=="wet"] <- 4;
ind.swabs <- tmp;

neighb.label  <- c("alajo","bukom","old-fadama","shiabu","NA")
env.label     <- c("market","household","publ.latr.","prim.school",
                   "sewage","ocean/beach","irrigation","publ.drain",
                   "str.vend.","publ.dom.","nursery","flood","farm");
smt.label     <- c("septage","partic.","lrg.wat.","sm.dw.","sm.env.",
                    "food","flies","handrinse","swabs");
septage.label <- c("Tanker truck",# "Sewage outfall","HH latrine",
                   "Public latrine",#"School/nursery latrine",
                   "Other");
particu.label <- c("sediment","soil","sand");
largewa.label <- c("public tap","compound/private tap","ocean");
smalldw.label <- c("public tap","compound/private tap","sachet",
                   "stored drinking water","stored water");
smallev.label <- c("small open drain near hh","medium open drain near hh",
                   "large open drain near hh","small open drain",
                   "medium open drain","large open drain",
                   "ecological drain", "stagnant water","lagoon",
                   "flood water");
foods.label   <- c("salad","porridge","fish","waakye","onion","water melon",
                   "lettuce","tomatoes","oranges","apples","mango",
                   "chili peppers","spring onion","cabbage");
flies.label   <- c("flies");
handr.label   <- c("handrinse");
swabs.label   <- c("dry on the ground","dry off the ground","wet on the ground","wet off the ground");

smp.label <- c(septage.label,particu.label,largewa.label,smalldw.label,
               smallev.label,foods.label,flies.label,handr.label,
               swabs.label);

offs <- c(max(ind.septage,na.rm=TRUE),max(ind.particu,na.rm=TRUE),
          max(ind.largewa,na.rm=TRUE),max(ind.smalldw,na.rm=TRUE),
          max(ind.smallev,na.rm=TRUE),max(ind.foods,na.rm=TRUE),
          max(ind.flies,na.rm=TRUE),max(ind.handr,na.rm=TRUE),
          max(ind.swabs,na.rm=TRUE));

ind.smp.tp <- rep(NA,n.data);
for(k.smp in 1:n.data){
  tmp <- c(ind.septage[k.smp],
           ind.particu[k.smp] + sum(offs[1:1]),
           ind.largewa[k.smp] + sum(offs[1:2]),
           ind.smalldw[k.smp] + sum(offs[1:3]),
           ind.smallev[k.smp] + sum(offs[1:4]),
           ind.foods[k.smp] + sum(offs[1:5]),
           ind.flies[k.smp] + sum(offs[1:6]),
           ind.handr[k.smp] + sum(offs[1:7]),
           ind.swabs[k.smp] + sum(offs[1:8]));
  tmp <- tmp[!is.na(tmp)];
  if(length(tmp)==1) ind.smp.tp[k.smp] <- tmp;
}
n.smp.tp <- sum(offs);

combine <- function(comb,envt,smpt){
  tmp <- intersect(which(ind.env.tp==envt),which(ind.smp.tp==smpt));
  return(cbind(comb,envt,smpt,tmp,deparse.level=0));
}

all.comb <- cbind(c(),c());
for(k.env in 1:n.env.tp){
    for(k.smt in 1:n.smp.tp){
        if(dim(combine(1,k.env,k.smt))[2]!=3){
            all.comb <- rbind(all.comb,c(k.env,k.smt));
        }
    }
}
num.comb <- nrow(all.comb);

ind.smp <- combine(1,all.comb[1,1],all.comb[1,2]);
for(k.comb in 2:nrow(all.comb)){
  tst <- combine(k.comb,all.comb[k.comb,1],all.comb[k.comb,2])
  if(dim(tst)[2]!=3) ind.smp <- rbind(ind.smp,tst);
}
n.smpl <- nrow(ind.smp);

ind.env.smp <- array(NA,dim=c(nrow(ind.smp),ncol(ind.smp)+1));
for(k.smpl in 1:nrow(ind.smp)){
  ind.env.smp[k.smpl,1:4] <- ind.smp[k.smpl,];
  ind.env.smp[k.smpl,5] <- as.numeric(neigh[ind.smp[k.smpl,4]]);
}
# There are many missing neighbourhoods (NA): substitute 5
ind.env.smp[is.na(ind.env.smp[,5]),5] <- 5;
n.neighb <- 5;

# not occurring in the combined sample/environment types
# should be small now!
notused <- setdiff((1:n.data),sort(ind.env.smp[,4]));

# read equivalent sample volumes
ec.eqv <- cbind(sanidata$"ec.dil1",sanidata$"ec.dil2",sanidata$"ec.dil3",
                sanidata$"ec.dil1",sanidata$"ec.dil2",sanidata$"ec.dil3");
en.eqv <- cbind(sanidata$"en.dil1",sanidata$"en.dil2",sanidata$"en.dil3",
                sanidata$"en.dil1",sanidata$"en.dil2",sanidata$"en.dil3");
cm.eqv <- cbind(sanidata$"cm.dil1",sanidata$"cm.dil2",sanidata$"cm.dil3");

# corrections...
ec.eqv[1595,3] <- 1.00; ec.eqv[1595,6] <- 1.00;

refdate <- as.POSIXlt(strptime("04/28/2012 12:00:00 CEST", "%m/%d/%Y %H:%M:%S"));
# include starting dilution by sample type (CFU/1mg)
ec.stdil <- array(NA,dim=c(nrow(ec.eqv),ncol(ec.eqv)));
for(i in 1:ncol(ec.eqv)){
  ec.stdil[,i] <- ifelse(smp.tp==levels(smp.tp)[2],
                         ifelse(tdata>=refdate,500,250),
                         ifelse(smp.tp==levels(smp.tp)[3] | smp.tp==levels(smp.tp)[5], 1,
                                ifelse(smp.tp==levels(smp.tp)[1], 200, 
                                       ifelse(smp.tp==levels(smp.tp)[6], 
                                              ifelse(foods$type=="prepared food" | is.na(foods$type) | is.na(foods$wght),500, foods$wght[]*2),1))));
}

en.stdil <- array(NA,dim=c(nrow(en.eqv),ncol(en.eqv)));
for(i in 1:ncol(en.eqv)){
  en.stdil[,i] <- ifelse(smp.tp==levels(smp.tp)[8], 1, 
    ifelse(smp.tp==levels(smp.tp)[1], 200, 1));}

cm.stdil <- array(NA,dim=c(nrow(cm.eqv),ncol(cm.eqv)));
for(i in 1:ncol(cm.eqv)){
  cm.stdil[,i] <- ifelse(smp.tp==levels(smp.tp)[2],
    ifelse(tdata>=refdate,500,250),
    ifelse(smp.tp==levels(smp.tp)[3] | smp.tp==levels(smp.tp)[5], 1,
    ifelse(smp.tp==levels(smp.tp)[1], 200, 
    ifelse(smp.tp==levels(smp.tp)[6], 
        ifelse(foods$type=="prepared food" | is.na(foods$type) | is.na(foods$wght),500, foods$wght[]*2),1))));
}

ec.eqvol <- ec.eqv * ec.stdil;
en.eqvol <- en.eqv * en.stdil;
cm.eqvol <- cm.eqv * cm.stdil;

# read counts
ec.cnt <- cbind(sanidata$"ec.ecnt1",sanidata$"ec.ecnt2",sanidata$"ec.ecnt3",
                sanidata$"ec.ecnt1b",sanidata$"ec.ecnt2b",sanidata$"ec.ecnt3b");
en.cnt <- cbind(sanidata$"en.cnt1",sanidata$"en.cnt2",sanidata$"en.cnt3",
                sanidata$"en.cnt1b",sanidata$"en.cnt2b",sanidata$"en.cnt3b");
cm.cnt <- cbind(sanidata$"cm.ccnt1",sanidata$"cm.ccnt2",sanidata$"cm.ccnt3");

# count the number of replicate observations for each sample
mkrepl <- function(rawcnt,rawdil) {
  numsam <- length(rawcnt[,1]);
  replvec <- rep(NA,numsam);
  for(n in 1:numsam) {
    replvec[n] <- length(rawcnt[n,!is.na(rawcnt[n,])]);
  }
  return(round(replvec));
}

# generate dataset with three levels:
# n = sample number
# k = number aliquots tested (total of numaliq max)
# m = c(counted number, equivalent sample volume, indicator for censoring)
# if init =TRUE: same as previous function,
# but now with censored counts set at censorlimit+1
mkcnt.cens <- function(rawcnt,rawvol,numaliq,init=FALSE) {
  numsam <- length(rawcnt[,1]);
  replvec <- rep(NA,numaliq);
  cntdata <- array(NA,dim=c(numsam,numaliq,3));
  for(n in 1:numsam) { # loop through all samples
    for(k in 1:numaliq) { # initial fill of cntdata array with raw data
      cntdata[n,k,1] <- rawcnt[[n,k]];
      cntdata[n,k,2] <- rawvol[[n,k]];
      cntdata[n,k,3] <- NA; # ignore censoring now
    } # sort aliquot vectors: missing last
    cntdata[n,,] <- rbind(cntdata[n,!is.na(cntdata[n,,1]),],
                          cntdata[n,is.na(cntdata[n,,1]),]);
    for(k in 1:numaliq) {
      if(!is.na(cntdata[n,k,1])){ # count not missing?
        if(cntdata[n,k,1] > censorlimit){ # tntc?
          if(init){ # are we preparing init data?
            cntdata[n,k,1] <- censorlimit+1;
            cntdata[n,k,2] <- NA;
            cntdata[n,k,3] <- NA;
          }else{ # not preparing init data
            cntdata[n,k,1] <- NA;
            cntdata[n,k,3] <- TRUE;
          }
        }else{ # not tntc
          if(init){ # are we preparing init data?
            cntdata[n,k,1] <- NA;
            cntdata[n,k,2] <- NA;
            cntdata[n,k,3] <- NA;
          }else{ # not preparing init date
            cntdata[n,k,3] <- FALSE;
          }
        }
      }else{ # count missing!
        cntdata[n,k,2] <- NA; # then also no volume
      }
    }
  }
  return(cntdata);
}

censorlimit <- 200;

ec.repl <- mkrepl(ec.cnt,ec.eqvol);
ec.data <- mkcnt.cens(ec.cnt,ec.eqvol,6,FALSE);
ec.init <- mkcnt.cens(ec.cnt,ec.eqvol,6,TRUE);
ec.init[notused,,] <- NA;

en.repl <- mkrepl(en.cnt,en.eqvol);
en.data <- mkcnt.cens(en.cnt,en.eqvol,6,FALSE);
en.init <- mkcnt.cens(en.cnt,en.eqvol,6,TRUE);
en.init[notused,,] <- NA;

cm.repl <- mkrepl(cm.cnt,cm.eqvol);
cm.data <- mkcnt.cens(cm.cnt,cm.eqvol,3,FALSE);
cm.init <- mkcnt.cens(cm.cnt,cm.eqvol,3,TRUE);
cm.init[notused,,] <- NA;

sd.ec.logr <- 4;
sd.ec.loglambda <- 10;
sd.en.logr <- 4;
sd.en.loglambda <- 10;
sd.cm.logr <- 4;
sd.cm.loglambda <- 20;

mu.ec.logr <- rep(0,num.comb);
mu.en.logr <- rep(0,num.comb);
mu.cm.logr <- rep(0,num.comb);

mu.ec.loglambda <- rep(NA,num.comb);
mu.en.loglambda <- rep(NA,num.comb);
mu.cm.loglambda <- rep(NA,num.comb);

mkmn <- function(cntvec,eqvvec){
  tmp <- na.omit(c(cntvec)/c(eqvvec));
  if(length(tmp)!=0){
    return(mean(tmp));
  }else{
    return(NA);
  }
}

for(k.comb in 1:num.comb){
  ind.k <- ind.env.smp[ind.env.smp[,1]==k.comb,4];
  ec.tmp <- mkmn(ec.cnt[ind.k,],ec.eqvol[ind.k,]);
  en.tmp <- mkmn(en.cnt[ind.k,],en.eqvol[ind.k,]);
  cm.tmp <- mkmn(cm.cnt[ind.k,],cm.eqvol[ind.k,]);
  if(is.na(ec.tmp) | ec.tmp==0) ec.tmp <- 1;
  mu.ec.loglambda[k.comb] <- log(ec.tmp);
  if(is.na(en.tmp) | en.tmp==0) en.tmp <- 1;
  mu.en.loglambda[k.comb] <- log(en.tmp);
  if(is.na(cm.tmp) | cm.tmp==0) cm.tmp <- 1;
  mu.cm.loglambda[k.comb] <- log(cm.tmp);
}

cntdata <- list("sd.ec.logr"=sd.ec.logr,"sd.ec.loglambda"=sd.ec.loglambda,
                "sd.en.logr"=sd.en.logr,"sd.en.loglambda"=sd.en.loglambda,
                "sd.cm.logr"=sd.cm.logr,"sd.cm.loglambda"=sd.cm.loglambda,
                "mu.ec.logr"=mu.ec.logr,"mu.ec.loglambda"=mu.ec.loglambda,
                "mu.en.logr"=mu.en.logr,"mu.en.loglambda"=mu.en.loglambda,
                "mu.cm.logr"=mu.cm.logr,"mu.cm.loglambda"=mu.cm.loglambda,
                "n.smpl"=n.smpl, "num.comb"=num.comb, "n.neighb"=n.neighb,
                "censorlimit"=censorlimit,
                "ind.env.smp"=ind.env.smp,
                "ec.repl"=ec.repl, "ec.data"=ec.data,
                "en.repl"=en.repl, "en.data"=en.data,
                "cm.repl"=cm.repl, "cm.data"=cm.data);

cntinit <- list("ec.data"=ec.init, "en.data"=en.init, "cm.data"=cm.init);
