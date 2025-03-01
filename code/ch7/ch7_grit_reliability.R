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


# work with grit.train

library(psych)

grit.scale <- grit.train[,1:12]

alpha(grit.scale)

# calculate by hand
k = 12
cov.mat = cov(grit.scale)

(k/(k-1))*(1-(sum(diag(cov.mat))/sum(cov.mat)))







omega.out = omega(grit.scale)

fscore.bif = omega.out$score

#predict(omega.out$model)

library(MBESS)
#ci.reliability(grit.scale,type="omega")
#ci.reliability(grit.scale,type="hierarchical",interval.type="bca",B=1000)


parallel.out = fa.parallel(grit.scale)

fa.out = fa(grit.scale,2)
summary(fa.out)
fa.out$loadings


# have to load script first
fa.rel = factor.score.reliability(fa.out$loadings,matrix(c(1,.42,.42,1),2,2),Estimators="Regression")

fa.rel



# principal components

pr2 = principal(grit.scale,2)
plot(pr2)

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
as.numeric(fa.out$loadings)


mod.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS10+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
"
cfa.out2 = cfa(mod.grit2,grit.train)
summary(cfa.out2,fit=T,std=T,rsq=T)

semPlot::semPaths(cfa.out2)



library(xtable)
xtable(inspect(cfa.out2, "est")$lambda)

fscore.train = lavPredict(cfa.out2)
fscore.test = lavPredict(cfa.out2,newdata=grit.test)



# pull out factor scores from bifactor model
# get at "s" components


#bif.out = fa(grit.scale,5,rotate="bifactor")

bif.grit2 <- "
g =~ NA*GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12
s1 =~ NA*GS2+GS3+GS5+GS7+GS8+GS11
s2 =~ NA*GS1+GS4+GS10
s3 =~ NA*GS3 + GS11
s4 =~ NA*GS6 + GS12
#g~~0*s1;g~~0*s2;s1~~0*s2
#g~~1*g;s1~~1*s1;s2~~1*s2
"
bif.out2 = cfa(bif.grit2,grit.train,orthogonal=T,std.lv=T)
summary(bif.out2,fit=T,std=T,rsq=T)

modindices(bif.out2)

fscore.train2 = lavPredict(bif.out2)
fscore.test2 = lavPredict(bif.out2,newdata=grit.test)

# factor score reliability

# from http://www.ccsenet.org/journal/index.php/ijsp/article/view/63934
# first load factor.score.reliability

lavInspect(cfa.out2,"est")

fa.rel2 = factor.score.reliability(lavInspect(cfa.out2,"est")$lambda,
                                  lavInspect(cfa.out2,"est")$psi,Estimators="Regression")

fa.rel2


# coefficient of determination
lambda = lavInspect(cfa.out2,"est")$lambda
psi = lavInspect(cfa.out2,"est")$psi

sqrt(diag(t(psi)%*%t(lambda)%*%solve(cov(grit.scale))%*%lambda%*%psi))



# predict conscientousnous
grit.train2 = as.data.frame(grit.train)
grit.test2 = as.data.frame(grit.test)

grit.train2$C <- rowSums(grit.train2[,55:64])
grit.test2$C <- rowSums(grit.test2[,55:64])

grit.train2$fscore1 = fscore.train[,1]
grit.train2$fscore2 = fscore.train[,2]
grit.test2$fscore1 = fscore.test[,1]
grit.test2$fscore2 = fscore.test[,2]

grit.train2$grit_sum = rowSums(grit.train2[,1:12])
grit.test2$grit_sum = rowSums(grit.test2[,1:12])



library(caret)

gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10),
                        n.trees = (10)*50, 
                        shrinkage = seq(.0005, .05,.0005),
                        n.minobsinnode = 10) # you can also put something


library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)



#just summed scores



lm.sum.train <- train(C ~ grit_sum,grit.train2,method="glm")
lm.sum.train
min($RMSE)

Metrics::rmse(grit.test2$C,predict(lm.sum.train,grit.test2))


gbm.sum.train <- train(C ~ grit_sum,grit.train2,method="gbm")
gbm.sum.train
min(gbm.sum.train$results$RMSE)

Metrics::rmse(grit.test2$C,predict(gbm.sum.train,grit.test2))


lm.obs.train <- train(C ~ GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12,grit.train2,method="glm")
lm.obs.train
min(lm.obs.train$results$RMSE)

Metrics::rmse(grit.test2$C,predict(lm.obs.train,grit.test2))

cor(predict(lm.obs.train,grit.test2),grit.test2$C)**2

gbm.obs.train <- train(C ~ GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12,
                       grit.train2,method="gbm",tuneGrid = gbmGrid)
gbm.obs.train
min(gbm.obs.train$results$RMSE)

Metrics::rmse(grit.test2$C,predict(gbm.obs.train,grit.test2))


lm.fs.train <- train(C ~ fscore1 + fscore2,grit.train2,method="glm")
lm.fs.train
min(lm.fs.train$results$RMSE)
cor(predict(lm.fs.train,grit.test2),grit.test2$C)**2

Metrics::rmse(grit.test2$C,predict(lm.fs.train,grit.test2))


gbm.fs.train <- train(C ~ fscore1 + fscore2,grit.train2,method="gbm",tuneGrid = gbmGrid)
gbm.fs.train
min(gbm.fs.train$results$RMSE)
cor(predict(gbm.fs.train,grit.test2),grit.test2$C)**2

Metrics::rmse(grit.test2$C,predict(gbm.fs.train,grit.test2))


gbm.comb.train <- train(C ~ fscore1+fscore2+GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12,
                        grit.train2,method="gbm",tuneGrid = gbmGrid)
gbm.comb.train
min(gbm.comb.train$results$RMSE)
cor(predict(gbm.comb.train,grit.test2),grit.test2$C)**2

Metrics::rmse(grit.test2$C,predict(gbm.comb.train,grit.test2))

varImp(gbm.comb.train)


# ridge regression
ridgeGrid <-  expand.grid(lambda = seq(0.00001,.5,by = 0.01)) # you can also put something
lm.comb.train <- train(C ~ fscore1+fscore2+GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12,
                        grit.train2,method="ridge",tuneGrid=ridgeGrid)

min(lm.comb.train$results$RMSE)
Metrics::rmse(grit.test2$C,predict(lm.comb.train,grit.test2))



#bifactor results
gbmGrid2 <-  expand.grid(interaction.depth = c(1,2,3),
                        n.trees = c(3,7,10)*70, 
                        shrinkage = seq(.0005, .02),
                        n.minobsinnode = c(10,50)) # you can also put something
gbm.bif.train <- train(fscore.train2,grit.train2$C,method="gbm",tuneGrid=gbmGrid2)
gbm.bif.train$results




# network models


library(psychonetrics)
vars = c("GS1","GS2","GS3","GS4","GS5","GS6",
         "GS7","GS8","GS9","GS10","GS11","GS12")
model <- ggm(grit.train, vars = vars, omega = "full")
library(qgraph)
mat <- getmatrix(model, "omega")
qgraph(mat, labels = vars,layout="groups")
#save.image("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch7_measurement/grit_rel_gbm.RData")


library(bootnet)

network = bootnet(grit.train)


library("psych")
data(bfi)
net_modSelect <- estimateNetwork(grit.scale, 
                                 default = "pcor")
plot(net_modSelect,threshold=0,edge.color="black")

centrality(net_modSelect)$OutDegree

cor(lavInspect(cfa.out2,"rsquare")[c(9,1,2,10,3,11,4,5,6,7,8,12)],centrality(net_modSelect)$OutDegree)


cor(fa.out$communalities,centrality(net_modSelect)$OutDegree)








# -----------------------------------------------------
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
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS10+GS11
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

partable1$free[153] = 1 # just the latent variable variance
partable1$free[154] = 1 # just the latent variable variance

est_mimic_test = cfa(partable1,grit.test)

summary(est_mimic_test,rsquare=T)


# use regsem and ridge to reduce parameter estimates

library(regsem)

cv.out <- cv_regsem(mimic.out2,type="alasso",#fit.ret2="boot",#mult.start=T,
                    jump=.002,n.lambda=10,pars_pen=c("regressions"))

#setwd('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch8_MLandSEM/scripts')
#saveRDS(cv.out,"lasso1_200.rds")
#cv.out = readRDS("lasso1_200.rds")

cv.out$fits[,"conv"] == 0

plot(cv.out$fits[cv.out$fits[,"conv"] == 0,"lambda"],cv.out$fits[cv.out$fits[,"conv"] == 0,"BIC"],xlab="lambda",ylab="BIC")

plot(cv.out)

partable2 = partable1


id.conv = which(cv.out$fits[,"conv"] == 0)
rsq1 = matrix(NA,20,2)
rsq2 = matrix(NA,20,2)

#id.conv=1:7

for(i in id.conv){

partable2[c(2:8,10:155),"est"] = cv.out$parameters[i,] # ridge1$final_pars
partable2$free[153] = 1 # just the latent variable variance
partable2$free[154] = 1 # just the latent variable variance
partable2$free[155] = 1 # just the latent variable variance

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

plot(cv.out$fits[id.conv[1:10],"lambda"],rsq2[id.conv[1:10],1],type="l",ylim=c(.5,.7),xlab="Lambda",ylab="R-Squared")
lines(cv.out$fits[id.conv[1:10],"lambda"],rsq2[id.conv[1:10],2],lty=2)

plot(cv.out$fits[id.conv[1:10],"lambda"],rsq1[id.conv[1:10],1],type="l",ylim=c(.2,.6),xlab="Lambda",ylab="R-Squared")
lines(cv.out$fits[id.conv[1:10],"lambda"],rsq1[id.conv[1:10],2],lty=2)


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
