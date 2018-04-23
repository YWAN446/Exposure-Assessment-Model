###By neighborhoods###
#percentile of zero for exposure difference distribution for two neighborhoods
ecdf(log10(non0(int.total3[4,]))-log10(non0(int.total3[1,])))(0)
ecdf(log10(non0(int.total3[3,]))-log10(non0(int.total3[1,])))(0)
ecdf(log10(non0(int.total3[2,]))-log10(non0(int.total3[1,])))(0)
ecdf(log10(non0(int.total3[4,]))-log10(non0(int.total3[2,])))(0)
ecdf(log10(non0(int.total3[3,]))-log10(non0(int.total3[2,])))(0)
ecdf(log10(non0(int.total3[4,]))-log10(non0(int.total3[3,])))(0)

ecdf(log10(non0(int.total2[4,]))-log10(non0(int.total2[1,])))(0)
ecdf(log10(non0(int.total2[3,]))-log10(non0(int.total2[1,])))(0)
ecdf(log10(non0(int.total2[2,]))-log10(non0(int.total2[1,])))(0)
ecdf(log10(non0(int.total2[4,]))-log10(non0(int.total2[2,])))(0)
ecdf(log10(non0(int.total2[3,]))-log10(non0(int.total2[2,])))(0)
ecdf(log10(non0(int.total2[4,]))-log10(non0(int.total2[3,])))(0)

ecdf(log10(non0(int.total1[4,]))-log10(non0(int.total1[1,])))(0)
ecdf(log10(non0(int.total1[3,]))-log10(non0(int.total1[1,])))(0)
ecdf(log10(non0(int.total1[2,]))-log10(non0(int.total1[1,])))(0)
ecdf(log10(non0(int.total1[4,]))-log10(non0(int.total1[2,])))(0)
ecdf(log10(non0(int.total1[3,]))-log10(non0(int.total1[2,])))(0)
ecdf(log10(non0(int.total1[4,]))-log10(non0(int.total1[3,])))(0)

#assumed paired --- Wilcoxon Signed Rank Test
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total3[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total3[2,])),log10(non0(int.total3[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total3[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[3,])),paired=TRUE)

wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total2[3,])),log10(non0(int.total2[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total2[2,])),log10(non0(int.total2[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total2[3,])),log10(non0(int.total2[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[3,])),paired=TRUE)

wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total1[3,])),log10(non0(int.total1[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total1[2,])),log10(non0(int.total1[1,])),paired=TRUE)
wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total1[3,])),log10(non0(int.total1[2,])),paired=TRUE)
wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[3,])),paired=TRUE)

#assumed non-paired --- Wilcoxon Rank Sum Test
#I think exposure for two different neighborhoods are individual.
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total3[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[2,])),log10(non0(int.total3[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total3[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total3[3,])),paired=FALSE)

wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[3,])),log10(non0(int.total2[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[2,])),log10(non0(int.total2[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[3,])),log10(non0(int.total2[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total2[3,])),paired=FALSE)

wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total1[3,])),log10(non0(int.total1[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total1[2,])),log10(non0(int.total1[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total1[3,])),log10(non0(int.total1[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total1[4,])),log10(non0(int.total1[3,])),paired=FALSE)

##################################################################################################
###by age###
ecdf(log10(non0(int.total3[1,]))-log10(non0(int.total2[1,])))(0)
ecdf(log10(non0(int.total3[1,]))-log10(non0(int.total1[1,])))(0)
ecdf(log10(non0(int.total2[1,]))-log10(non0(int.total1[1,])))(0)

ecdf(log10(non0(int.total3[2,]))-log10(non0(int.total2[2,])))(0)
ecdf(log10(non0(int.total3[2,]))-log10(non0(int.total1[2,])))(0)
ecdf(log10(non0(int.total2[2,]))-log10(non0(int.total1[2,])))(0)

ecdf(log10(non0(int.total3[3,]))-log10(non0(int.total2[3,])))(0)
ecdf(log10(non0(int.total3[3,]))-log10(non0(int.total1[3,])))(0)
ecdf(log10(non0(int.total2[3,]))-log10(non0(int.total1[3,])))(0)

ecdf(log10(non0(int.total3[4,]))-log10(non0(int.total2[4,])))(0)
ecdf(log10(non0(int.total3[4,]))-log10(non0(int.total1[4,])))(0)
ecdf(log10(non0(int.total2[4,]))-log10(non0(int.total1[4,])))(0)

#assumed non-paired --- Wilcoxon Rank Sum Test
#I think exposure for two different age groups are individual.
wilcox.test(log10(non0(int.total3[1,])),log10(non0(int.total2[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[1,])),log10(non0(int.total1[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[1,])),log10(non0(int.total1[1,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[2,])),log10(non0(int.total2[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[2,])),log10(non0(int.total1[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[2,])),log10(non0(int.total1[2,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total2[3,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[3,])),log10(non0(int.total1[3,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[3,])),log10(non0(int.total1[3,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total2[4,])),paired=FALSE)
wilcox.test(log10(non0(int.total3[4,])),log10(non0(int.total1[4,])),paired=FALSE)
wilcox.test(log10(non0(int.total2[4,])),log10(non0(int.total1[4,])),paired=FALSE)