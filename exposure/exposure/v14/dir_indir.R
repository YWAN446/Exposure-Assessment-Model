source("~/stat/sanipath/exposure/exposure/v14/v6.load0.r")
source("~/stat/sanipath/exposure/exposure/v14/v14.main2.r")

source("~/stat/sanipath/exposure/exposure/v14/v6.load1.r")
source("~/stat/sanipath/exposure/exposure/v14/v14.main2.r")

source("~/stat/sanipath/exposure/exposure/v14/v6.load2.r")
source("~/stat/sanipath/exposure/exposure/v14/v14.main2.r")

dir.indir.HH.1.n1 <- HH.1[[21]][1,,]
dir.indir.HH.1.n2 <- HH.1[[21]][2,,]
dir.indir.HH.1.n3 <- HH.1[[21]][3,,]
dir.indir.HH.1.n4 <- HH.1[[21]][4,,]
dir.indir.HH.2.n1 <- HH.2[[21]][1,,]
dir.indir.HH.2.n2 <- HH.2[[21]][2,,]
dir.indir.HH.2.n3 <- HH.2[[21]][3,,]
dir.indir.HH.2.n4 <- HH.2[[21]][4,,]
dir.indir.HH.3.n1 <- HH.3[[21]][1,,]
dir.indir.HH.3.n2 <- HH.3[[21]][2,,]
dir.indir.HH.3.n3 <- HH.3[[21]][3,,]
dir.indir.HH.3.n4 <- HH.3[[21]][4,,]

dir.indir.HH.n1 <- cbind(dir.indir.HH.1.n1,dir.indir.HH.2.n1,dir.indir.HH.3.n1)
dir.indir.HH.n2 <- cbind(dir.indir.HH.1.n2,dir.indir.HH.2.n2,dir.indir.HH.3.n2)
dir.indir.HH.n3 <- cbind(dir.indir.HH.1.n3,dir.indir.HH.2.n3,dir.indir.HH.3.n3)
dir.indir.HH.n4 <- cbind(dir.indir.HH.1.n4,dir.indir.HH.2.n4,dir.indir.HH.3.n4)

dir.indir.HH.1 <- cbind(dir.indir.HH.1.n1,dir.indir.HH.1.n2,dir.indir.HH.1.n3,dir.indir.HH.1.n4)
dir.indir.HH.2 <- cbind(dir.indir.HH.2.n1,dir.indir.HH.2.n2,dir.indir.HH.2.n3,dir.indir.HH.2.n4)
dir.indir.HH.3 <- cbind(dir.indir.HH.3.n1,dir.indir.HH.3.n2,dir.indir.HH.3.n3,dir.indir.HH.3.n4)

dir.indir.HH <- cbind(dir.indir.HH.1.n1,dir.indir.HH.1.n2,dir.indir.HH.1.n3,dir.indir.HH.1.n4,
                      dir.indir.HH.2.n1,dir.indir.HH.2.n2,dir.indir.HH.2.n3,dir.indir.HH.2.n4,
                      dir.indir.HH.3.n1,dir.indir.HH.3.n2,dir.indir.HH.3.n3,dir.indir.HH.3.n4)

dir_percent <- function(dat){
  return(round(sum(dat[1,])/sum(dat),2))
}

dir_percent_total <- function(dat1, dat2){
  return(round(sum(dat1[1,])/sum(dat2),2))
}

dir_percent(dir.indir.HH.1.n1)
dir_percent(dir.indir.HH.1.n2)
dir_percent(dir.indir.HH.1.n3)
dir_percent(dir.indir.HH.1.n4)
dir_percent(dir.indir.HH.2.n1)
dir_percent(dir.indir.HH.2.n2)
dir_percent(dir.indir.HH.2.n3)
dir_percent(dir.indir.HH.2.n4)
dir_percent(dir.indir.HH.3.n1)
dir_percent(dir.indir.HH.3.n2)
dir_percent(dir.indir.HH.3.n3)
dir_percent(dir.indir.HH.3.n4)

dir_percent(dir.indir.HH.1)
dir_percent(dir.indir.HH.2)
dir_percent(dir.indir.HH.3)

dir_percent(dir.indir.HH.n1)
dir_percent(dir.indir.HH.n2)
dir_percent(dir.indir.HH.n3)
dir_percent(dir.indir.HH.n4)

dir_percent(dir.indir.HH)

dir_percent_total(dir.indir.HH.1.n1,HH.1[[20]][1,])
dir_percent_total(dir.indir.HH.1.n2,HH.1[[20]][2,])
dir_percent_total(dir.indir.HH.1.n3,HH.1[[20]][3,])
dir_percent_total(dir.indir.HH.1.n4,HH.1[[20]][4,])
dir_percent_total(dir.indir.HH.2.n1,HH.2[[20]][1,])
dir_percent_total(dir.indir.HH.2.n2,HH.2[[20]][2,])
dir_percent_total(dir.indir.HH.2.n3,HH.2[[20]][3,])
dir_percent_total(dir.indir.HH.2.n4,HH.2[[20]][4,])
dir_percent_total(dir.indir.HH.3.n1,HH.3[[20]][1,])
dir_percent_total(dir.indir.HH.3.n2,HH.3[[20]][2,])
dir_percent_total(dir.indir.HH.3.n3,HH.3[[20]][3,])
dir_percent_total(dir.indir.HH.3.n4,HH.3[[20]][4,])



save(HH.1,file="HH.dir.indir0-1.rda",ascii=TRUE);
save(HH.2,file="HH.dir.indir1-2.rda",ascii=TRUE);
save(HH.3,file="HH.dir.indir2-5.rda",ascii=TRUE);

nsim=1000
per.dir.res <- array(NA,dim=c(nsim,8))
sample_sum <- function(dat1,dat2,nsim){
  for (i in 1:nsim){
    k.sample <- sample(1:10000, size = 356)
    per.dir <- dat1[1,k.sample]/(dat1[1,k.sample]+dat1[2,k.sample])
    per.dir.res[i,1:4] <- c(mean(per.dir,na.rm=T),quantile(per.dir, probs = c(0.025,0.5,0.975),na.rm=T))
    per.dir.total <- dat1[1,k.sample]/dat2[k.sample]
    per.dir.res[i,5:8] <- c(mean(per.dir.total,na.rm=T),quantile(per.dir.total, probs = c(0.025,0.5,0.975),na.rm=T))
  }
  return(colMeans(per.dir.res))
}

round(sample_sum(dir.indir.HH.1.n1,HH.1[[20]][1,],nsim=1000),3)
round(sample_sum(dir.indir.HH.1.n2,HH.1[[20]][2,],nsim=1000),3)
round(sample_sum(dir.indir.HH.1.n3,HH.1[[20]][3,],nsim=1000),3)
round(sample_sum(dir.indir.HH.1.n4,HH.1[[20]][4,],nsim=1000),3)

round(sample_sum(dir.indir.HH.2.n1,HH.2[[20]][1,],nsim=1000),3)
round(sample_sum(dir.indir.HH.2.n2,HH.2[[20]][2,],nsim=1000),3)
round(sample_sum(dir.indir.HH.2.n3,HH.2[[20]][3,],nsim=1000),3)
round(sample_sum(dir.indir.HH.2.n4,HH.2[[20]][4,],nsim=1000),3)

round(sample_sum(dir.indir.HH.3.n1,HH.3[[20]][1,],nsim=1000),3)
round(sample_sum(dir.indir.HH.3.n2,HH.3[[20]][2,],nsim=1000),3)
round(sample_sum(dir.indir.HH.3.n3,HH.3[[20]][3,],nsim=1000),3)
round(sample_sum(dir.indir.HH.3.n4,HH.3[[20]][4,],nsim=1000),3)


