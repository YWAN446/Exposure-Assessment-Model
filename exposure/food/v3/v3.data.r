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

dom <- factor(childbx$"dom");
domnum <- childbx$"domnum";
obs <- childbx$"obs";
# defecate <- childbx$"hhs.305";
# dispose.feces <- childbx$"hhs.307";
# publatrine <- childbx$"hhs.601";
# market <- factor(childbx$"hhs.602");
produce <- childbx$"hhs.603";
vended <- childbx$"hhs.604";
# beach <- childbx$"hhs.701";
# cldbeach <- childbx$"hhs.702";
# oceanwat <- childbx$"hhs.705";
# flood <- childbx$"hhs.801";
nsattend <- childbx$"hhs.901";
nsdays <- childbx$"hhs.902";
nshrs <- childbx$"hhs.903";
schfood <- childbx$"hhs.905";

fd.neighb <- neighb[!is.na(produce) | !is.na(vended)];
fd.prod <- produce[!is.na(produce) | !is.na(vended)];
fd.vend <- vended[!is.na(produce) | !is.na(vended)];
fd.prod[fd.prod=="no response"] <- NA;
fd.vend[fd.vend=="no response"] <- NA;
n.tot <- length(fd.neighb);
n.neighb <- nlevels(fd.neighb);
fd.neighb <- unclass(fd.neighb); # 1="alajo", 2="bukom",
                                 # 3="old fadama", 4="shiabu"
fd.prod <- unclass(fd.prod); # 1="everyday"; 2="few times/wk"
fd.vend <- unclass(fd.vend); # 3="once/wk" ; 4="never"
n.lev <- 4;

data <- list("n.neighb"=n.neighb,"fd.neighb"=fd.neighb,"n.lev"=n.lev,
             "n.tot"=n.tot,"fd.prod"=fd.prod,"fd.vend"=fd.vend);
