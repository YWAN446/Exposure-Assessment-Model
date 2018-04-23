library(Hmisc);
source("minticks.r");
epsw <- 5;
epsh <- 5;

epsfile <- function(name) paste("./output/",name,"-",scenario,".eps",sep="");

if(scenario == "0"){
  sens_salmo_exp <- cor(salmo,
                        cbind(chickbrst_salmo,# 10^chickbrst_salmo_lc,
                              # chickbrst_cut,
                              # chickbrst_first,
                              cc_transfer,
                              # salmo_chickbrst,
                              lettuce_portion,lettuce_homegrown_salmo,
                              lettuce_whole_salmo,lettuce_precut_salmo,
                              red_rinse,
                              # cut_ingred,
                              lettuce_salmo[,1],
                              lettuce_salmo[,2]+lettuce_salmo[,3],
                              lettuce_salmo[,4]+lettuce_salmo[,5],
                              transfer_salmo[,1],
                              transfer_salmo[,2],transfer_salmo[,3]));
  sens_campy_exp <- cor(campy,
                        cbind(chickbrst_campy,# 10^chickbrst_campy_lc,
                              # chickbrst_cut,
                              # chickbrst_first,
                              cc_transfer,
                              # campy_chickbrst,
                              lettuce_portion,lettuce_homegrown_campy,
                              lettuce_whole_campy,lettuce_precut_campy,
                              red_rinse,
                              # cut_ingred,
                              lettuce_campy[,1],
                              lettuce_campy[,2]+lettuce_campy[,3],
                              lettuce_campy[,4]+lettuce_campy[,5],
                              transfer_campy[,1],
                              transfer_campy[,2],transfer_campy[,3]));
  labs <- rbind(c("pathogen on chickbrst","black"),
                # c("reduction in storage","blue"),
                # c("self cut chicken","red"),
                # c("cut meat first","red"),
                c("transfer efficiency","grey"),
                # c("pathogen transferred","white"),
                c("lettuce portion","grey"),
                c("pathogen on homegrown","black"),
                c("pathogen on whole","black"),
                c("pathogen on precut","black"),
                c("reduction by rinsing","grey"),
                # c("add cut ingred","red"),
                c("homegrown lettuce","white"),
                # c("whole lettuce rinsed","white"),
                # c("whole lettuce not rinsed","white"),
                c("whole lettuce","white"),
                # c("precut lettuce rinsed","white"),
                # c("precut lettuce not rinsed","white"),
                c("precut lettuce","white"),
                c("transferred to homegrown","white"),
                c("transferred to whole","white"),
                c("transferred to precut","white"));

  sens_salmo_rsk <- cor(risk.salmo,
                        cbind(chickbrst_salmo,# 10^chickbrst_salmo_lc,
                              # chickbrst_cut,
                              # chickbrst_first,
                              cc_transfer,
                              # salmo_chickbrst,
                              lettuce_portion,lettuce_homegrown_salmo,
                              lettuce_whole_salmo,lettuce_precut_salmo,
                              red_rinse,
                              # cut_ingred,
                              lettuce_salmo[,1],
                              lettuce_salmo[,2]+lettuce_salmo[,3],
                              lettuce_salmo[,4]+lettuce_salmo[,5],
                              transfer_salmo[,1],
                              transfer_salmo[,2],transfer_salmo[,3]));
  sens_campy_rsk <- cor(risk.campy,
                        cbind(chickbrst_campy,# 10^chickbrst_campy_lc,
                              # chickbrst_cut,
                              # chickbrst_first,
                              cc_transfer,
                              # campy_chickbrst,
                              lettuce_portion,lettuce_homegrown_campy,
                              lettuce_whole_campy,lettuce_precut_campy,
                              red_rinse,
                              # cut_ingred,
                              lettuce_campy[,1],
                              lettuce_campy[,2]+lettuce_campy[,3],
                              lettuce_campy[,4]+lettuce_campy[,5],
                              transfer_campy[,1],
                              transfer_campy[,2],transfer_campy[,3]));

  ord <- order(sens_salmo_rsk);
  # sens <- t(rbind(sens_salmo_rsk[ord],sens_salmo_exp[ord]));
  sens <- sens_salmo_rsk[ord];
  levels(sens) <- labs[ord,1];

  setEPS();
  postscript(epsfile("salmo-corr"),width=epsw,height=epsh);
  par(mar=c(4,11,0.2,0.2));
  # barplot(t(sens),beside=TRUE,horiz=TRUE,names.arg=labs[ord],las=1);
  barplot(as.numeric(sens),horiz=TRUE,names.arg=labs[ord,1],las=1,
          col=labs[ord,2]);
  dev.off();

  ord <- order(sens_campy_rsk);
  # sens <- t(rbind(sens_campy_rsk[ord],sens_campy_exp[ord]));
  sens <- sens_campy_rsk[ord];
  levels(sens) <- labs[ord,1];

  setEPS();
  postscript(epsfile("campy-corr"),width=epsw,height=epsh);
  par(mar=c(4,11,0.2,0.2));
  # barplot(t(sens),beside=TRUE,horiz=TRUE,names.arg=labs[ord],las=1);
  barplot(as.numeric(sens),horiz=TRUE,names.arg=labs[ord,1],las=1,
          col=labs[ord,2]);
  dev.off();
}

postscript(epsfile("exp-salmo-z"),width=1.25*epsw,height=0.75*epsh);
par(mar=c(4,4,0.5,0.5));
salmo_dist <- cbind(salmo,lettuce_salmo,transfer_salmo);
f0 <- rep(NA,ncol(salmo_dist));
for(k.bp in 1:ncol(salmo_dist))
  f0[k.bp] <- 1-length(which(salmo_dist[,k.bp]>0))/nmc;
barplot(f0,ylim=c(0,1),ylab="fraction zero",
        names.arg=c("all","a","b1","b2","c1","c2","d1","d2","d3"));
dev.off();

postscript(epsfile("exp-salmo-nz"),width=1.25*epsw,height=0.75*epsh);
par(mar=c(4,4,0.2,0.2));
salmo_dist <- cbind(salmo,lettuce_salmo,transfer_salmo);
boxplot(log10(salmo_dist[salmo_dist[,1]>0,1]),at=1,main="",yaxt="n",
        xlim=c(0.5,ncol(salmo_dist)+0.5),ylim=c(-6,1),outline=FALSE,
        ylab="dose");
for(k.bp in 2:ncol(salmo_dist))
  boxplot(log10(salmo_dist[salmo_dist[,k.bp]>0,k.bp]),add=TRUE,at=k.bp,
          outline=FALSE,yaxt="n");
ticks.log10(2);
axis(side=1,at=(1:ncol(salmo_dist)),
    labels=c("all","a","b1","b2","c1","c2","d1","d2","d3"));
#     labels=c("hg","wh;r","wh;nr","pc;r","pc;nr",
#              "tr;hg","tr;wh","tr;pc"));
dev.off();

postscript(epsfile("exp-campy-z"),width=1.25*epsw,height=0.75*epsh);
par(mar=c(4,4,0.5,0.5));
campy_dist <- cbind(campy,lettuce_campy,transfer_campy);
f0 <- rep(NA,ncol(campy_dist));
for(k.bp in 1:ncol(campy_dist))
  f0[k.bp] <- 1-length(which(campy_dist[,k.bp]>0))/nmc;
barplot(f0,ylim=c(0,1),ylab="fraction zero",
        names.arg=c("all","a","b1","b2","c1","c2","d1","d2","d3"));
dev.off();

postscript(epsfile("exp-campy-nz"),width=1.25*epsw,height=0.75*epsh);
par(mar=c(4,4,0.2,0.2));
campy_dist <- cbind(campy,lettuce_campy,transfer_campy);
boxplot(log10(campy_dist[campy_dist[,1]>0,1]),at=1,main="",yaxt="n",
        xlim=c(0.5,ncol(campy_dist)+0.5),ylim=c(-6,1),outline=FALSE,
        ylab="dose");
for(k.bp in 2:ncol(campy_dist))
  boxplot(log10(campy_dist[campy_dist[,k.bp]>0,k.bp]),add=TRUE,at=k.bp,
          outline=FALSE,yaxt="n");
ticks.log10(2);
axis(side=1,at=(1:ncol(salmo_dist)),
    labels=c("all","a","b1","b2","c1","c2","d1","d2","d3"));
#     labels=c("hg","wh;r","wh;nr","pc;r","pc;nr",
#              "tr;hg","tr;wh","tr;pc"));
dev.off();

postscript(epsfile("risk-salmo"),width=0.8*epsw,height=0.8*epsh);
par(mar=c(4,4,0.2,0.2));
plot(density(log10(risk.salmo)),xlim=c(-8,0),main="",xaxt="n",xlab="P(ill)");
lines(x=c(log10(risk.salmo_pe),log10(risk.salmo_pe)),y=c(0,0.36))
ticks.log10(1);
dev.off();

postscript(epsfile("risk-campy"),width=0.8*epsw,height=0.8*epsh);
par(mar=c(4,4,0.2,0.2));
plot(density(log10(risk.campy)),xlim=c(-8,0),main="",xaxt="n",xlab="P(ill)");
lines(x=c(log10(risk.campy_pe),log10(risk.campy_pe)),y=c(0,1.05))
ticks.log10(1);
dev.off();
