corr.trace<-array(NA,dim=c(num.mc,33,n.neighb));

for (k.neighb in 1:n.neighb){
  for (k.mc in 1:num.mc){
    for (k.factor in 1:32){
      corr.trace[k.mc,k.factor,k.neighb]<-cor(corr.data[1:k.mc,33,k.neighb],corr.data[1:k.mc,k.factor,k.neighb])
    }
  }
}

corr.trace.sm<-array(NA,dim=c(num.mc,33,n.neighb));

for (k.neighb in 1:n.neighb){
  for (k.mc in 1:num.mc){
    for (k.factor in 1:32){
      corr.trace.sm[k.mc,k.factor,k.neighb]<-cor(corr.data[1:k.mc,33,k.neighb],corr.data[1:k.mc,k.factor,k.neighb],method="spearman")
    }
  }
}

num.mc=5000
par(mar=c(4.1,5.1,4.1,1));
for (k.factor in 1:32){
  #plot(c(1:num.mc),corr.trace[1:num.mc,k.factor,k.neighb],type="l",ylim=(c(-1,1)),main=labs[k.factor,1])
  #plot(corr.data[1:num.mc,33,k.neighb],corr.data[1:num.mc,k.factor,k.neighb],main=labs[k.factor,1])
  #plot(log10(corr.data[1:num.mc,33,k.neighb]),log10(corr.data[1:num.mc,k.factor,k.neighb]),main=labs[k.factor,1])
  plot(log10(corr.data[1:num.mc,33,k.neighb]),corr.data[1:num.mc,k.factor,k.neighb],main=labs[k.factor,1])
}

num.mc=5000
par(mar=c(4.1,5.1,4.1,1));
for (k.factor in 1:32){
  plot(c(1:num.mc),corr.trace.sm[1:num.mc,k.factor,k.neighb],type="l",ylim=(c(-1,1)),main=labs[k.factor,1])
  #plot(corr.data[1:num.mc,33,k.neighb],corr.data[1:num.mc,k.factor,k.neighb],main=labs[k.factor,1])
  #plot(log10(corr.data[1:num.mc,33,k.neighb]),log10(corr.data[1:num.mc,k.factor,k.neighb]),main=labs[k.factor,1])
  #plot(log10(corr.data[1:num.mc,33,k.neighb]),corr.data[1:num.mc,k.factor,k.neighb],main=labs[k.factor,1])
}
