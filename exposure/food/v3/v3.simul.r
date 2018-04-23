library(triangle);

version <- "v3"

n.neighb <- 4; # 1=alajo; 2=bukom; 3=old fadama; 4=shiabu
n.fd <- 2; # 1=produce and 2=vended food
n.levs <- 4 # 1="everyday"; 2="few times/wk"; 3="once/wk"; 4="never"

basedir <- "~/stat/sanipath/exposure/food/";
filemc <- paste(basedir,version,"/output/",version,".mcmc.rda",sep="");
cat("loading food intake parameter estimates ",version,"\n");
load(filemc);

fd.prodsmpl <- function(k.neighb){
  k.iter <- sample((1:length(fdpars$pi.prod[1,1,])),size=1);
  res <- sample((1:n.levs),size=1,prob=fdpars$pi.prod[k.neighb,,k.iter]);
  if(res==1) return(7);
  if(res==2) return(sample((2:6),size=1));
  if(res==3) return(1);
  if(res==4) return(0);
}

fd.vendsmpl <- function(k.neighb){
  k.iter <- sample((1:length(fdpars$pi.vend[1,1,])),size=1);
  res <- sample((1:n.levs),size=1,prob=fdpars$pi.vend[k.neighb,,k.iter]);
  if(res==1) return(7);
  if(res==2) return(sample((2:6),size=1));
  if(res==3) return(1);
  if(res==4) return(0);
}

serving <- function(prod=TRUE){           # this should be adjusted...
  if(prod)  return(rtriangle(n=1,a=20,b=100,c=60)*1000);   #mg/serving;
  if(!prod) return(rtriangle(n=1,a=10,b=150,c=80)*1000);   #mg/serving;
}
