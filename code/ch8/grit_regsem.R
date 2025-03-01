grit.kev = read.csv("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch10_Groups/semtree/data_kev.csv",header=T,sep=",")
grit.kev = read.csv("/Users/rjacobuc/Documents/GitHub/edm_book/ch10_Groups/semtree/data_kev.csv",header=T,sep=",")
# set of models
# factor score for grit, use regression
# factor score for grit, use boosting
# sem model with predictors, latent variable for grit
# sem model with latent variable for predictors, latent variable for grit
# sem trees with grit as latent variable
# sem forests with grit as latent variable

colnames(grit.kev)

grit2 <- grit.kev[,c(3:14,31:92)]
grit2$familysize[grit2$familysize > 40] = NA


ind99 = grit2[,c(1:12,25:74)] == 0 & is.na(grit2[,c(1:12,25:74)]) ==F
grit2[,c(1:12,25:74)][ind99] <- NA

grit2$GS2  = 6 - grit2$GS2
grit2$GS3  = 6 - grit2$GS3
grit2$GS5  = 6 - grit2$GS5
grit2$GS7  = 6 - grit2$GS7
grit2$GS8  = 6 - grit2$GS8
grit2$GS11 = 6 - grit2$GS11


grit2 = data.matrix(grit2)
grit2[,74] = grit2[,74] - 2


grit3 = grit2[complete.cases(grit2),]

# take sample of 1000 for each
set.seed(1)
ids1 = sample(1:nrow(grit3),2000)
grit.train = grit3[ids1[1:1000],]
grit.test = grit3[ids1[1001:2000],]
grit.pop = grit3[-ids1,]


library(lavaan)

mod.grit <- "
grit =~ GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12
"
cfa.out = cfa(mod.grit,grit.train)
summary(cfa.out,fit=T)

library(psych)

parallel.out = fa.parallel(grit.train[,1:12])

fa.out = fa(grit.train[,1:12],2)
summary(fa.out)
fa.out$loadings


mod.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
"
cfa.out2 = cfa(mod.grit2,grit.train)
summary(cfa.out2,fit=T)

semPlot::semPaths(cfa.out2)

library(xtable)
xtable(inspect(cfa.out2, "est")$lambda)

fscore.train = lavPredict(cfa.out2)
fscore.test = lavPredict(cfa.out2,newdata=grit.test)

grit.train2 = data.frame(grit.train[,13:74])
grit.test2 = data.frame(grit.test[,13:74])

grit.train2$grit1 = fscore.train[,1]
grit.train2$grit2 = fscore.train[,2]
grit.test2$grit1 = fscore.test[,1]
grit.test2$grit2 = fscore.test[,2]


# use lm

lm.out <- lm(grit1 ~ .  -grit2, grit.train2)
summary(lm.out)

lm.out2 <- lm(grit2 ~ .  -grit1, grit.train2)
summary(lm.out2)

# train
cor(predict(lm.out,grit.train2),grit.train2$grit1)**2
cor(predict(lm.out2,grit.train2),grit.train2$grit2)**2

# test
cor(predict(lm.out,grit.test2),grit.test2$grit1)**2
cor(predict(lm.out2,grit.test2),grit.test2$grit2)**2


# try lasso to reduce
library(glmnet)
X.train = data.matrix(grit.train2[,1:62])
X.test = data.matrix(grit.test2[,1:62])

grit1.train = as.numeric(grit.train2$grit1)
grit2.train = as.numeric(grit.train2$grit2)
grit1.test = as.numeric(grit.test2$grit1)
grit2.test = as.numeric(grit.test2$grit2)

glmnet.out1 <- cv.glmnet(X.train,grit1.train)
glmnet.out2 <- cv.glmnet(X.train,grit2.train)

coef(glmnet.out1,s=glmnet.out1$lambda.1se)


cor(predict(glmnet.out1,X.train,s=glmnet.out1$lambda.1se),grit.train2$grit1)**2
cor(predict(glmnet.out2,X.train,s=glmnet.out2$lambda.1se),grit.train2$grit2)**2
cor(predict(glmnet.out1,X.test,s=glmnet.out1$lambda.1se),grit.test2$grit1)**2
cor(predict(glmnet.out2,X.test,s=glmnet.out2$lambda.1se),grit.test2$grit2)**2

# try SEM model

mimic.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
grit1 + grit2 ~ education+urban+gender+engnat+age+hand+religion+orientation+race+voted+married+familysize+
A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+
C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+
O1+O2+O3+O4+O5+O6+O7+O8+O9+O10+
E1+E2+E3+E4+E5+E6+E7+E8+E9+E10+
N1+N2+N3+N4+N5+N6+N7+N8+N9+N10
"
mimic.out2 = cfa(mimic.grit2,grit.train)
summary(mimic.out2,fit=T,rsquare=T)


library(semPlot)
semPaths(mimic.out2,exoVar=F,exoCov=F)

# get on test sample

partable1 = parTable(mimic.out2)
# only estimate residual variances
partable1$free = 0

partable1[151:170,]

partable1$free[152] = 1 # just the latent variable variance
partable1$free[153] = 1 # just the latent variable variance

est_mimic_test = cfa(partable1,grit.test)

summary(est_mimic_test,rsquare=T)


# use regsem and ridge to reduce parameter estimates

library(regsem)

cv.out <- cv_regsem(mimic.out2,type="lasso",mult.start=T,#fit.ret2="boot",#mult.start=T,
                    jump=.01,n.lambda=20,pars_pen=c("regressions"))

setwd('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts')
saveRDS(cv.out,"lasso1_200.rds")
#cv.out = readRDS("lasso1_200.rds")

cv.out = lasso1_200

plot(cv.out,col="grey",lwd=1)

plot(cv.out$fits[cv.out$fits[,"conv"] == 0,"lambda"],
     cv.out$fits[cv.out$fits[,"conv"] == 0,"BIC"],xlab="Lambda",ylab="BIC")


partable2 = partable1


idd = which(cv.out$fits[,"conv"] == 0)
rsq1 = matrix(NA,20,2)
rsq2 = matrix(NA,20,2)

for(i in idd){

partable2[c(2:7,9:154),"est"] = cv.out$parameters[i,] # ridge1$final_pars
partable2$free[152] = 1 # just the latent variable variance
partable2$free[153] = 1 # just the latent variable variance

est_mimic_reg_train = cfa(partable2,grit.train)

summary(est_mimic_reg_train,rsquare=T)

rsq1[i,1] = lavInspect(est_mimic_reg_train,"rsquare")[13]
rsq2[i,1] = lavInspect(est_mimic_reg_train,"rsquare")[14]

# test
est_mimic_reg_test = cfa(partable2,grit.test)

summary(est_mimic_reg_test,rsquare=T)

rsq1[i,2] = lavInspect(est_mimic_reg_test,"rsquare")[13]
rsq2[i,2] = lavInspect(est_mimic_reg_test,"rsquare")[14]

}

plot(cv.out$fits[idd,"lambda"],rsq2[idd,1],type="l",ylim=c(.6,.7),xlab="Lambda",ylab="R-Squared")
lines(cv.out$fits[idd,"lambda"],rsq2[idd,2],lty=2)

plot(cv.out$fits[idd,"lambda"],rsq1[idd,1],type="l",ylim=c(.35,.55),xlab="Lambda",ylab="R-Squared")
lines(cv.out$fits[idd,"lambda"],rsq1[idd,2],lty=2)


# higher penalties


#cv.out2 <- cv_regsem(mimic.out2,type="ridge",fit.ret2="boot",mult.start=T,
#                    jump=.015,n.lambda=20,pars_pen=c("regressions"))
#
#setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit')
#saveRDS(cv.out2,"ridge2_100.rds")
#cv.out2 = readRDS("ridge2.rds")

#plot(cv.out)

#plot(ridge1$fits[,"lambda"],ridge1$fits[,"chisq"],xlab="lambda",ylab="Chi-Square")


# use of lavaan model as fixed parameter estimates
#length(ridge1$final_pars)


#partable2 = partable1



#rsq1 = matrix(NA,20,2)
#rsq2 = matrix(NA,20,2)

#for(i in c(1:20)){

#  partable2[c(2:7,9:154),"est"] = cv.out2$parameters[i,] # ridge1$final_pars
#  partable2$free[152] = 1 # just the latent variable variance
#  partable2$free[153] = 1 # just the latent variable variance

#  est_mimic_reg_train = cfa(partable2,grit.train)

#  summary(est_mimic_reg_train,rsquare=T)

#  rsq1[i,1] = lavInspect(est_mimic_reg_train,"rsquare")[13]
#  rsq2[i,1] = lavInspect(est_mimic_reg_train,"rsquare")[14]

  # test
#  est_mimic_reg_test = cfa(partable2,grit.test)

#  summary(est_mimic_reg_test,rsquare=T)

#  rsq1[i,2] = lavInspect(est_mimic_reg_test,"rsquare")[13]
#  rsq2[i,2] = lavInspect(est_mimic_reg_test,"rsquare")[14]

#}

#plot(cv.out2$fits[,"lambda"],rsq2[,1],type="l",ylim=c(.45,.8),xlab="Lambda",ylab="R-Squared")
#lines(cv.out2$fits[,"lambda"],rsq2[,2],lty=2)

#plot(cv.out2$fits[,"lambda"],rsq1[,1],type="l",ylim=c(.45,.8),xlab="Lambda",ylab="R-Squared")
#lines(cv.out2$fits[,"lambda"],rsq1[,2],lty=2)
