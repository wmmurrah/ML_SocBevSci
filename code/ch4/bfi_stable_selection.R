


library(psych);library(xtable)

# recode age into 4 bins
attach(bfi)
age2 = age
age2[age < 18] = 1
age2[age >= 18 & age < 23] =  2
age2[age < 45 & age >= 23] = 3
age2[age >=45] = 4

bfi$age2 = age2

library(dummies)
dum = dummy("age2",bfi)

dum.sub = dum[complete.cases(bfi),2:4] # reference is young
colnames(dum.sub) <- c("college","mid","old")
bfi2 = bfi[complete.cases(bfi),1:26]
bfi2$gender = bfi2$gender - 1
bfi3 <- data.frame(bfi2,dum.sub)


agree <- rowSums(bfi3[,1:5])  # sum score for demonstration, outcome
bfi4 <- bfi3[,-c(1:5)]
bfi4$agree <- agree


# stability


# stability
library(stabs)

YY <- bfi4$agree
XX <- bfi4[,1:24]
XX[,1:20] <- data.matrix(scale(XX[,1:20]))

stab.out11 <- stabsel(XX,YY,cutoff=0.9,PFER=1,B=1000,fitfun=glmnet.lasso)
stab.out11
plot(stab.out11,)

library(xtable)
xtable(data.matrix(stab.out11$max))


# glmnet

par.out <- matrix(NA,25,3)
for(j in 1:3){
  set.seed(j)
  ids <- sample(1:nrow(XX),nrow(XX),replace=T)
  cv.out <- cv.glmnet(data.matrix(XX[ids,]),data.matrix(YY[ids]))
  par.out[,j] = as.numeric(coef(cv.out,cv.out$lambda.1se))
}


row.names(par.out) <- row.names(coef(cv.out,cv.out$lambda.1se))

xtable(par.out)
