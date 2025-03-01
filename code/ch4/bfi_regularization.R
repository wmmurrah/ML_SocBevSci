library(psych)
data(bfi)



set.seed(11)
bfi.comp <- bfi[complete.cases(bfi),]



# try all predictors
nrow(bfi.comp)

lm.comp <- lm(age ~ ., bfi.comp)
summary(lm.comp)

library(stargazer)
stargazer(lm.comp,column.sep.width="1pt",single.row=TRUE,style="aer")



library(QuantPsyc)
lm.beta(lm.comp)


library(glmnet)
XX <- data.matrix(data.frame(scale(bfi.comp[,1:25]),ed =scale(bfi.comp[,27])))
YY <- as.numeric(scale(bfi.comp[,28]))

cv.out <- cv.glmnet(XX,YY)
round(coef(cv.out,cv.out$lambda.1se),2)

#library(xtable)


#library(rtable)
#library(ReporteRs)
#tab=as.FlexTable(xtable(lm.comp))

#library(texreg)

lm.comp2 <- lm(YY ~ XX)

str(summary(lm.comp))

round(lm.comp2$coefficients,3)

ps <- round(summary(lm.comp2)$coefficients[2:27,4],3)

library(ggplot2)

datt <-  data.frame(colnames(XX),round(lm.comp2$coefficients[2:27],3),
                    round(coef(cv.out,cv.out$lambda.1se)[2:27],2))

colnames(datt) <- c("var","OLS","Lasso")
library(reshape2)
datt.long <- melt(datt)

colnames(datt.long) <- c("Variable","Method","Coefficient")

ggplot(datt.long, aes(x = Variable, y = Coefficient, shape = Method))+
  geom_point() + geom_hline(yintercept=0) +
  scale_shape_manual(values = c('OLS' = 1, 'Lasso' = 4))





# interactions



library(hierNet)
fit=hierNet.path(XX,YY,diagonal=FALSE)
hierCV <- hierNet.cv(fit,XX,YY)
plot(hierCV)
print(hierCV)
#coef(fit,hierCV$lamhat.1se)

fit.final = hierNet(XX,YY,hierCV$lamhat.1se)
fit.final



# stability
library(stabs)
stab.out11 <- stabsel(XX,YY,cutoff=0.75,PFER=1)
stab.out11
plot(stab.out11,col=gray.colors(50,start=.3,end=.31))





# group lasso

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


# select first 200 cases

bfi4 = bfi4[1:150,]


# semi partial multiple correlation

lm.d = lm(agree ~ gender+college+mid+old,bfi4)
r2.d = summary(lm.d)$r.squared

lm.dc = lm(agree ~ gender+college+mid+old+C1+C2+C3+C4+C5,bfi4)
r2.dc = summary(lm.dc)$r.squared
r2.dc - r2.d # 7%

# calculate F

f = ((r2.dc - r2.d)/(1-r2.dc)) * (2225/5)
1 - pf(f,5,2225) # 0



# another way to calculate
anova(lm.d,lm.dc)


# add demographics

lm.dco = lm(agree ~ gender+college+mid+old+C1+C2+C3+C4+C5+O1+O2+O3+O4+O5,bfi4)
r2.dco = summary(lm.dco)$r.squared

f2 = ((r2.dco - r2.dc)/(1-r2.dco)) * (2221/4)
1 - pf(f2,4,2221) # 0


# another way
anova(lm.d,lm.dc,lm.dco)


# add neuroticism

lm.dcon = lm(agree ~ gender+college+mid+old+C1+C2+C3+C4+C5+O1+O2+O3+O4+O5+N1+N2+N3+N4+N5,bfi4)
xtable(anova(lm.d,lm.dc,lm.dco,lm.dcon))


# add null
lm.null = lm(agree ~ 1,bfi4)
xtable(anova(lm.null,lm.d,lm.dc,lm.dco,lm.dcon))


# get added r squared
library(lmSupport)
vals = c(modelCompare(lm.null,lm.d)$DeltaR2,
         modelCompare(lm.d,lm.dc)$DeltaR2,
         modelCompare(lm.dc,lm.dco)$DeltaR2,
         modelCompare(lm.dco,lm.dcon)$DeltaR2)

xtable(cbind(c(1,2,3,4),vals))



# group lasso

library(grpreg)
y.agree <- bfi4$agree
X <- bfi4[,-c(6:10,25)]
vars <- c(rep("c",5),rep("n",5),rep("o",5),rep("demo",4))

#fit.grp <- grpreg(X, y.agree, vars, penalty="grLasso",family="gaussian")
#select(fit.grp,"BIC")

set.seed(123)
fit.grp <- cv.grpreg(X, y.agree, vars, penalty="grLasso",family="gaussian")
coef(fit.grp)


set.seed(123)
fit.grp2 <- cv.grpreg(X, y.agree, vars, penalty="cMCP",family="gaussian")
coef(fit.grp2)


# look at reliability
psych::alpha(bfi4[,1:5])



library(SGL)

dat.list <- list(
  y = y.agree,
  x = X
)
vars2 <- ceiling(rank(vars)/4)

fit.sgl <- cvSGL(dat.list,vars2,nlam=50)
plot(fit.sgl)

fit.sgl$fit$beta[,25]
log(fit.sgl$lambdas) == -4











# -----------

# various ways to perform CV



bfi2 <- bfi.comp[sample(1:nrow(bfi.comp),300),c(1:25,28)]


# leave predictors as is
hist(bfi2$age,breaks=15)
hist(log(bfi2$age),breaks=15)


lm.bfi1 <- lm(age ~ ., bfi2)
summary(lm.bfi1)

lm.bfi2 <- lm(log(age) ~ ., bfi2)
summary(lm.bfi2)

# turn predictors to categorical
library(magrittr)
library(dplyr)
cols <- colnames(bfi2[,1:25])
bfi3 = bfi2
bfi3 %<>%
  mutate_at(cols,funs(factor(.)))
str(bfi3)

lm.bfi3 <- lm(log(age) ~ ., bfi3)
summary(lm.bfi3)


# cross-validation
library(caret)

folds <- createFolds(1:nrow(bfi2))

rsquared <- rep(NA,10)
for(i in 1:10){
  train <- bfi2[-folds[[i]],]
  test <- bfi2[folds[[i]],]
  lm.out <- lm(age ~ ., train)
  preds <- predict(lm.out,test)
  rsquared[i] = cor(preds,test$age)**2
}

# bootstrapping
resamps <- createResample(1:nrow(bfi2),20)

rsquared2 <- rep(NA,20)
for(i in 1:20){
  train <- bfi2[resamps[[i]],]
  test <- bfi2[-resamps[[i]],]
  lm.out <- lm(age ~ ., train)
  preds <- predict(lm.out,test)
  rsquared2[i] = cor(preds,test$age)**2
}

# compare to train
cont <- trainControl(method="repeatedcv",number=10)
lm.train <- train(log(age) ~ ., bfi2,trControl=cont,method="lm")
lm.train

# dummy codes does worse because of CV

library(dummies)
bfi3.dummy = dummy.data.frame(bfi3)

lm.train.dummy <- train(log(age) ~ ., bfi3.dummy,trControl=cont,method="lm")
lm.train.dummy



# regularization
library(glmnet)
XX <- data.matrix(bfi2[,1:25])
XX2 <- data.matrix(bfi3.dummy[,1:169])
YY <- as.numeric(log(bfi2[,26]))

glmnet.out <- cv.glmnet(XX2,YY)
coef(glmnet.out,glmnet.out$lambda.1se)


#stable selection
library(stabs)
stab.out <- stabsel(XX2,YY,cutoff=0.75,PFER=1)
stab.out



# try lavaan

library(lavaan)

bfi3$age <- log(bfi3$age)
hist(bfi2$age)

mod <- "
A =~ NA*A1+A2+A3+A4+A5
C =~ NA*C1+C2+C3+C4+C5
O =~ NA*O1+O2+O3+O4+O5
E =~ NA*E1+E2+E3+E4+E5
N =~ NA*N1+N2+N3+N4+N5
age ~ A+C+O+E+N
A~~1*A;C~~1*C;O~~1*O;E~~1*E;N~~1*N"
mod.out <- sem(mod,bfi3,ordered=colnames(bfi3[,1:25]))
summary(mod.out,rsquare=T)



# categorical item


library(lavaan)



mod <- "
A =~ NA*A1+A2+A3+A4+A5
C =~ NA*C1+C2+C3+C4+C5
O =~ NA*O1+O2+O3+O4+O5
E =~ NA*E1+E2+E3+E4+E5
N =~ NA*N1+N2+N3+N4+N5
gender ~ A+C+O+E+N
A~~1*A;C~~1*C;O~~1*O;E~~1*E;N~~1*N"
mod.out <- sem(mod,bfi,ordered=colnames(bfi[,1:26]))
summary(mod.out,rsquare=T)




