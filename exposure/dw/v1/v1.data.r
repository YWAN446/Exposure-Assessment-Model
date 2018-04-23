library(foreign)

basedir <- "~/stat/sanipath/data/sanipath24092013/";
dbname <- "Bx081513.dta";
childbx <- read.dta(paste(basedir,dbname,sep=""), convert.factors=TRUE,
                    convert.underscore=TRUE, warn.missing.labels=TRUE);

neighb <- childbx$neighb;
neighb.levels <- levels(neighb);
n.neighb <- nlevels(neighb);

ageyg <- childbx$"hhs.306";
ageunit <- factor(childbx$"hhs.306a");
age.yr <- ifelse(ageunit=="months",ageyg/12,ageyg);
age.gr <- ifelse(age.yr<=1,1,ifelse(age.yr<=2,2,3));
n.age <- nlevels(as.factor(age.gr));

dom <- factor(childbx$"dom");
domnum <- childbx$"domnum";
obs <- childbx$"obs";

mklevs <- function(vec){
  n <- length(vec);
  levs <- array(NA,dim=c(n,2));
  levs[1,] <- c(vec[1],mean(vec[1:2]));
  for(k in 2:(n-2)){
    levs[k,] <- c(levs[k-1,2],mean(c(vec[k],vec[k+1])));
  }
  levs[n-1,] <- c(levs[n-2,2],2*vec[n-1]-levs[n-2,2]);
  levs[n,] <- c(levs[n-1,2],Inf);
  return(levs);
}

### H2O consumption in ml
watdrink <- childbx$"hhs.201";
watqty <- childbx$"hhs.203";
watqty[which(watqty==999)] <- NA; #999 means negative response or nonexistent or unavailable data;
watunit <- factor(childbx$"hhs.203a");
# what is the meaning of the "999"s?
# there is no corresponding unit in hhs.203a
freqs<- table(watdrink);
freqs.neigh<- rbind(table(watdrink[which(neighb=="alajo")]),table(watdrink[which(neighb=="bukom")]),
              table(watdrink[which(neighb=="old fadama")]),table(watdrink[which(neighb=="shiabu")])); # frequencies of each factor in watdrink
freq.tap <- freqs[levels(watdrink)=="tap from pipe"];
freq.sac <- freqs[levels(watdrink)=="sachet"];
dw.tap <- cbind(age.gr[watunit=="cups"],
                watqty[watunit=="cups"]); # cups, use unit to select, not water source;
dw.sac <- cbind(age.gr[watunit=="sachets"],
                watqty[watunit=="sachets"]);        # sachets, use unit to select, not water source;
dw.tap <- dw.tap[which(!is.na(dw.tap[,2]) & !is.na(dw.tap[,1])),];
dw.sac <- dw.sac[which(!is.na(dw.sac[,2]) & !is.na(dw.sac[,1])),];
n.tap <- nrow(dw.tap);
n.sac <- nrow(dw.sac);
tap.levels <- mklevs(sort(unique(dw.tap[,2])));
sac.levels <- mklevs(sort(unique(dw.sac[,2])));
age.gr.tap <- dw.tap[,1];
age.gr.sac <- dw.sac[,1];
drink.tap <- array(NA,dim=c(n.tap,2));
for(k in 1:n.tap)
  drink.tap[k,] <- log(tap.levels[which(tap.levels[,1] <= dw.tap[k,2] &
                                        tap.levels[,2] > dw.tap[k,2]),]);
drink.sac <- array(NA,dim=c(n.sac,2));
for(k in 1:n.sac)
  drink.sac[k,] <- log(sac.levels[which(sac.levels[,1] <= dw.sac[k,2] &
                                        sac.levels[,2] > dw.sac[k,2]),]);
drink.tap.cens <- rep(1,n.tap);
drink.sac.cens <- rep(1,n.sac);

drink.tap.init <- (drink.tap[,1] + drink.tap[,2])/2;
drink.tap.init[drink.tap.init== Inf] <- log(tap.levels[nrow(tap.levels),1])+1;
drink.tap.init[drink.tap.init==-Inf] <- log(tap.levels[1,2])-1;
drink.sac.init <- (drink.sac[,1] + drink.sac[,2])/2;
drink.sac.init[drink.sac.init== Inf] <- log(sac.levels[nrow(sac.levels),1])+1;
drink.sac.init[drink.sac.init==-Inf] <- log(sac.levels[1,2])-1;

cup <- 237;    # ml
sachet <- 500; # ml

hyp.tap <- rbind(c(0,2),c(0.001,0.001));
hyp.sac <- rbind(c(0,2),c(0.001,0.001));

data <- list("n.age"=n.age,
             "age.gr.tap"=age.gr.tap,"n.tap"=n.tap,"hyp.tap"=hyp.tap,
             "drink.tap"=drink.tap,"drink.tap.cens"=drink.tap.cens,
             "age.gr.sac"=age.gr.sac,"n.sac"=n.sac,"hyp.sac"=hyp.sac,
             "drink.sac"=drink.sac,"drink.sac.cens"=drink.sac.cens);

data.init <- list("cons.tap"=drink.tap.init,"cons.sac"=drink.sac.init);
