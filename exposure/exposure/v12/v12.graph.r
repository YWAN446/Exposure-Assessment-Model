library(Hmisc);
#source("minticks.r");
epsw <- 10;
epsh <- 8;

for (k.neighb in 1:n.neighb){
  epsfile <- function(name) paste("./output/",name,".",neighb.label[k.neighb],subset[2],".eps",sep="");
  
  sens_rsk <- cor(corr.data[,33,k.neighb],
                  cbind(corr.data[,1,k.neighb],corr.data[,2,k.neighb],corr.data[,3,k.neighb],corr.data[,4,k.neighb],
                        corr.data[,5,k.neighb],corr.data[,6,k.neighb],corr.data[,7,k.neighb],corr.data[,8,k.neighb],
                        corr.data[,9,k.neighb],corr.data[,10,k.neighb],corr.data[,11,k.neighb],corr.data[,12,k.neighb],
                        corr.data[,13,k.neighb],corr.data[,14,k.neighb],corr.data[,15,k.neighb],corr.data[,16,k.neighb],
                        corr.data[,17,k.neighb],corr.data[,18,k.neighb],corr.data[,19,k.neighb],corr.data[,20,k.neighb],
                        corr.data[,21,k.neighb],corr.data[,22,k.neighb],corr.data[,23,k.neighb],corr.data[,24,k.neighb],
                        corr.data[,25,k.neighb],corr.data[,26,k.neighb],corr.data[,27,k.neighb],corr.data[,28,k.neighb],
                        corr.data[,29,k.neighb],corr.data[,30,k.neighb],corr.data[,31,k.neighb],corr.data[,32,k.neighb]),use="complete.obs");
  
  labs <- rbind(c("HW duration (average)","grey"),
                c("Bathe duration (average)","grey"),
                c("HW freq","grey"),
                c("Bathe freq","grey"),
                c("HW freq with soap","grey"),
                c("Bathe freq with soap","grey"),
                c("Defecate freq","grey"),
                c("Defecate with touching surface freq","grey"),
                c("Defecate with contacting feces freq","grey"),
                c("Eating freq","grey"),
                c("Eating-breast milk freq","grey"),
                c("Eating-raw produce freq","grey"),
                c("Eating-vendor/prepared food freq","grey"),
                c("Food weight (average)","grey"),
                c("Eating with hands freq","grey"),
                c("Contamination on hand (auc)","grey"),
                c("Contamination on hand (max)","grey"),
                c("Amount tap water","grey"),
                c("Amount sachet water","grey"),
                c("Contacting drain and SWATA freq","grey"),
                c("Contacting SWATA freq","grey"),
                c("Contacting drain freq","grey"),
                c("Duration on dirt floor","grey"),
                c("Duration on concrete floor","grey"),
                c("Duration on off floor surface","grey"),
                c("Betweenness for hands","grey"),
                c("Degree of hands","grey"),
                c("Degree of food","grey"),
                c("Degree of mouth","grey"),
                c("Degree of HW","grey"),
                c("Degree of bathe","grey"),
                c("Weighted betweenness of hands","grey"));
  
  ord <- order(sens_rsk);
  sens <- sens_rsk[ord];
  levels(sens) <- labs[ord,1];
  
  setEPS();
  postscript(epsfile("corr"),width=epsw,height=epsh);
  par(mar=c(4,15,0.5,1.5));
  barplot(as.numeric(sens),horiz=TRUE,names.arg=labs[ord,1],las=1,
          col=labs[ord,2]);
  dev.off();
}
