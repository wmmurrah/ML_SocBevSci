
library(lavaan);library(corrplot);library(psych)
HS <- data.frame(scale(HolzingerSwineford1939[,7:15]))
corrplot(cor(HS))


pca.out1 <- principal(HS,2,rotate="none")
pca.out2 <- principal(HS,2,rotate="varimax")
loadings(pca.out1)
loadings(pca.out2)


plot(loadings(pca.out1),xlim=c(-1,1),ylim=c(-1,1),xlab="PC1",ylab="PC2")
abline(0,0)
abline(v=0)


plot(loadings(pca.out2),xlim=c(-1,1),ylim=c(-1,1),xlab="PC1",ylab="PC2")
abline(0,0)
abline(v=0)



fa.out1 <- fa(HS,2,rotate="none")
fa.out2 <- fa(HS,2,rotate="varimax")
fa.out3 <- fa(HS,2,rotate="oblimin")

plot(loadings(fa.out1),xlim=c(-1,1),ylim=c(-1,1),xlab="FA1",ylab="FA2",cex.lab=1.5)
abline(0,0)
abline(v=0)


plot(loadings(fa.out2),xlim=c(-1,1),ylim=c(-1,1),xlab="FA1",ylab="FA2",cex.lab=1.5)
abline(0,0)
abline(v=0)


plot(loadings(fa.out3),xlim=c(-1,1),ylim=c(-1,1),xlab="FA1",ylab="FA2",cex.lab=1.5)
abline(0,0)
abline(v=0)

fa.out3$rot.mat
fa.out2$rot.mat
 