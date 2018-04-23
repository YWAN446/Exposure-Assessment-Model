ver <- "v15";

# subset <- c("hh",0);
here <- getwd();
# set the base directory
base <- "~/stat/sanipath/"

# load hazard rate estimates
path <- paste(base,"behav/childbx/rate/v13/",sep="");
setwd(path);
source("v13.extract.r");
setwd(here);

r <- rate.parameters$r;
lambda.mc <- rate.parameters$lambda.mc;

# reverse.slash <- function(string){
#   return(gsub("/","\\\\",string));
# }

neighbourhoods <- c("alajo","bukom","old-fadama","shiabu")
compartments <-c("dirt","floor","off grnd","SWATA","drain");
behaviours <- c("play-sit","sleep","handw","bathe","defec","eat");

n.neighb <- length(neighbourhoods);
n.comp <- length(compartments);
n.behav <-length(behaviours);
n.iter <- 3000;

# "comp.mask" indicating behaviours never observed by compartment.
# or behaviours that we consider very unlikely by compartment.
# comp1.mask <- c(1,0,1,1,1,1);
# comp2.mask <- c(1,1,1,1,1,1);
# comp3.mask <- c(1,1,1,1,1,1);
# comp4.mask <- c(1,0,0,0,0,1);
# comp5.mask <- c(1,0,0,0,0,0);
# comp.mask <- rbind(comp1.mask,comp2.mask,comp3.mask,
#                    comp4.mask,comp5.mask);
