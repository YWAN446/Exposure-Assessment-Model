ver <- "v15";

dur.day <- 14*60;
st.comp <- 3; # Off ground
st.behav <- 2; # Sleep

n.sim <- 1000;
freq.day <- array(0,dim=c(n.neighb,n.comp,n.behav,n.sim));
time.day <- array(0,dim=c(n.neighb,n.comp,n.behav,n.sim));
freq.mn <- array(NA,dim=c(n.neighb,n.comp,n.behav));
time.mn <- array(NA,dim=c(n.neighb,n.comp,n.behav));

for(k.neighb in 1:n.neighb){
  cat(neighbourhoods[k.neighb]," ");
  for(k.sim in 1:n.sim){
    # cat(format(k.sim,width=4));
    act.day <- cbind(gen.period.seq(k.neighb,st.comp,st.behav,dur.day));
    # cat("\b\b\b\b");
    for(k.comp in 1:n.comp){
      for(k.behav in 1:n.behav){
        freq.day[k.neighb,k.comp,k.behav,k.sim] <-
          length(act.day[3,act.day[1,]==k.comp & act.day[2,]==k.behav]);
        time.day[k.neighb,k.comp,k.behav,k.sim] <-
          sum(act.day[3,act.day[1,]==k.comp & act.day[2,]==k.behav]);
      }
    }
  }
}
cat("\n");
for(k.neighb in 1:n.neighb){
  for(k.comp in 1:n.comp){
    for(k.behav in 1:n.behav){
       freq.mn[k.neighb,k.comp,k.behav] <-
         mean(freq.day[k.neighb,k.comp,k.behav,]);
       time.mn[k.neighb,k.comp,k.behav] <-
         mean(time.day[k.neighb,k.comp,k.behav,]);
    }
  }
}

sim <- list("freq.day"=freq.day,"time.day"=time.day);
file.sim <- paste("./output/",subset[1],subset[2],"sim.rda",sep="");
save(sim,file=file.sim,ascii=TRUE);

# sim.nb <- function(k.neighb){
#   cat(neighbourhoods[k.neighb]," ");
#   for(k.sim in 1:n.sim){
#     cat(format(k.sim,width=4));
#     act.day <- cbind(gen.period.seq(k.neighb,st.comp,st.behav,dur.day));
#     cat("\b\b\b\b");
#     for(k.comp in 1:n.comp){
#       for(k.behav in 1:n.behav){
#         freq.day[k.neighb,k.comp,k.behav,k.sim] <-
#           length(act.day[3,act.day[1,]==k.comp & act.day[2,]==k.behav]);
#         time.day[k.neighb,k.comp,k.behav,k.sim] <-
#           sum(act.day[3,act.day[1,]==k.comp & act.day[2,]==k.behav]);
#       }
#     }
#   }
# }

# aggr.nb <- function(k.neighb){
#   for(k.comp in 1:n.comp){
#     for(k.behav in 1:n.behav){
#        freq.mn[k.neighb,k.comp,k.behav] <-
#          mean(freq.day[k.neighb,k.comp,k.behav,]);
#        time.mn[k.neighb,k.comp,k.behav] <-
#          mean(time.day[k.neighb,k.comp,k.behav,]);
#     }
#   }
# }

