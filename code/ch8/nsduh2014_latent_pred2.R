setwd("G:/My Drive")
library(foreign)

# codebook
# http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/NSDUH-2014/NSDUH-2014-datasets/NSDUH-2014-DS0001/NSDUH-2014-DS0001-info/NSDUH-2014-DS0001-info-codebook.pdf
dat <- read.spss("36361-0001-Data.sav",to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)
vars <- c("DSTNRV12","DSTHOP12","DSTRST12","DSTCHR12","DSTEFF12",
          "DSTNGD12","IMPREMEM","IMPCONCN","IMPGOUT",
          "IMPPEOP","IMPSOC","IMPHHLD","IMPRESP","IMPWORK","IMPDYFRQ",
          "IRSEX","IRMARIT","CATAG6","NEWRACE2","SUICTHNK")
dat.sub <- dat[,vars]

dat.sub2 <- dat.sub[is.na(dat.sub$SUICTHNK)==FALSE & dat.sub$CATAG6 !=1,]


library(psych)
race <- dummy.code(dat.sub2$NEWRACE2) 
race2 <- race[,2:7] # white as reference
colnames(race2) <- c("black","native","pac","asian","multi","hisp")
dat.sub3 <- data.frame(dat.sub2[,-43],race2)
dat.sub3$SUICTHNK <- (dat.sub3$SUICTHNK*-1)+2

# dummy code marriage
marr <- dummy.code(dat.sub3$IRMARIT)
marr2 <- marr[,2:4] # married as reference
colnames(marr2) <- c("marr2","marr3","marr4")
dat.sub4 <- data.frame(dat.sub3[,-41],marr2)

# dummy code cat -- age
catt <- dummy.code(dat.sub4$CATAG6)
catt2 <- catt[,2:5] # 12-17 as reference
colnames(catt2) <- c("age2","age3","age4","age5")
dat.sub5 <- data.frame(dat.sub4[,-41],catt2)

sel1 = which(dat.sub5$SUICTHNK == 1)
sel0 = which(dat.sub5$SUICTHNK == 0)

set.seed(1)
ids11 <- sample(sel1,1000)
ids10 <- sample(sel0,1000)

dat.sub11 <- dat.sub5[ids11,]
dat.sub10 <- dat.sub5[ids10,]
dat2000 <- rbind(dat.sub11,dat.sub10)


#dat2000[,24:27] <- data.frame(scale(dat2000[,24:27]))


# impute missing

library(mice)
dat2000.fac <- apply(dat2000,2,as.factor)
mod <- mice(dat2000.fac,m=1,maxit=5,seed=1)
dat2000.1 = complete(mod)

# create subsample of 1000 for train, 1000 for test
set.seed(1)
ids.train <- sample(1:2000,1000)
dat.train <- dat2000.1[ids.train,]
dat.test <- dat2000.1[-ids.train,]


# find latent factor of predictors, without suicide and demographics

lat.pred <- dat.train[,c("DSTNRV12","DSTHOP12","DSTRST12","DSTCHR12","DSTEFF12",
                         "DSTNGD12","IMPREMEM","IMPCONCN","IMPGOUT",
                         "IMPPEOP","IMPSOC","IMPHHLD","IMPRESP","IMPWORK","IMPDYFRQ")]
lat.pred.test <- dat.test[,c("DSTNRV12","DSTHOP12","DSTRST12","DSTCHR12","DSTEFF12",
                             "DSTNGD12","IMPREMEM","IMPCONCN","IMPGOUT",
                             "IMPPEOP","IMPSOC","IMPHHLD","IMPRESP","IMPWORK","IMPDYFRQ")]

lat.pred <- data.frame(apply(lat.pred,2,as.numeric))
lat.pred.test <- data.frame(apply(lat.pred.test,2,as.numeric))
library(psych)
describe(lat.pred)

corrplot(cor(lat.pred))



#lat.pred2 = lat.pred[,!colnames(lat.pred)  %in% c("ADWRLOSE","ADWRENRG","ADWRJINO","ARXMDEYR","ADWRDEPR","ADWRWRTH")]

#lat.pred2.test = lat.pred.test[,!colnames(lat.pred.test)  %in% c("ADWRLOSE","ADWRENRG","ADWRJINO","ARXMDEYR","ADWRDEPR","ADWRWRTH")]



#lat.pred3 = lat.pred[,!colnames(lat.pred)  %in% c("ADWRLSIN","ADWRDISC","ADWRELES","ADWRENRG","ADWRJINO","ATXMDEYR","ARXMDEYR","ADWRDEPR","ADWRWRTH","AMDETXRX")]

#lat.pred3.test = lat.pred.test[,!colnames(lat.pred.test)  %in% c("ADWRLSIN","ADWRDISC","ADWRELES","ADWRENRG","ADWRJINO","ATXMDEYR","ARXMDEYR","ADWRDEPR","ADWRWRTH","AMDETXRX")]


# categorical factor analysis with lavaan

library(semTools)


efa1 <- efaUnrotate(lat.pred,nf=1,start=F)
summary(efa1,fit=T)

efa2 <- efaUnrotate(lat.pred,nf=2,start=F)
summary(efa2,fit=T)


efa3 <- efaUnrotate(lat.pred,nf=3,start=F)
summary(efa3,fit=T)

ob.out = oblqRotate(efa2, method = "quartimin")

efa4 <- efaUnrotate(lat.pred,nf=4,start=F)
summary(efa4,fit=T)

#ob.out = oblqRotate(efa4, method = "quartimin")

#library(semPlot)


#semPaths(ob.out)

loads = round(ob.out@loading,2)
loads


library(xtable)
xtable(loads)


# create factor scores
library(gbm)

fscores = predict(efa2)
head(fscores)

fscores.test = predict(efa2,newdata=lat.pred.test)


comb.train <- data.frame(fscores, dat.train[,c("SUICTHNK","black","native","pac","asian","multi",
                                               "hisp","marr2","marr3","marr4",   
                                               "age2","age3","age4","age5")])
comb.test <- data.frame(fscores.test, dat.test[,c("SUICTHNK","black","native","pac","asian","multi",
                                               "hisp","marr2","marr3","marr4",   
                                               "age2","age3","age4","age5")])


glm.lv.train <- glm(SUICTHNK ~ ., comb.train,family="binomial")
summary(glm.lv.train)

pred.lv1 = predict(glm.lv.train,comb.test,type="response")
library(caret)

library(pROC)
roc_obj1 <- roc(comb.test$SUICTHNK, pred.lv1)
roc_obj1
plot(roc_obj1)

pred1 = as.factor(pred.lv1 > 0.5)
levels(pred1) = c("0","1")
confusionMatrix(as.factor(comb.test$SUICTHNK), pred1)


comb.train.man <- data.frame(lat.pred, dat.train[,c("SUICTHNK","black","native","pac","asian","multi",
                                               "hisp","marr2","marr3","marr4",   
                                               "age2","age3","age4","age5")])
comb.test.man <- data.frame(lat.pred.test, dat.test[,c("SUICTHNK","black","native","pac","asian","multi",
                                                  "hisp","marr2","marr3","marr4",   
                                                  "age2","age3","age4","age5")])


glm.man.train <- glm(SUICTHNK ~ ., comb.train.man,family="binomial")
summary(glm.man.train)

# problems with collinearity, need to use glmnet
library(glmnet)
Y.train = as.numeric(comb.train.man$SUICTHNK)
X.train = comb.train.man
X.train$SUICTHNK <- NULL
X.train = data.matrix(X.train)

X.test = comb.test.man
X.test$SUICTHNK <- NULL
X.test = data.matrix(X.test)

glmnet.man.train <- cv.glmnet(X.train,Y.train,alpha=0,family="binomial")
coef(glmnet.man.train,glmnet.man.train$lambda.1se)

pred.glmnet.man = predict(glmnet.man.train,newx=X.test,s=glmnet.man.train$lambda.1se,type="response")

roc_obj2 <- roc(comb.test$SUICTHNK, as.numeric(pred.glmnet.man))
roc_obj2
plot(roc_obj2)

pred2 = as.factor(as.numeric(pred.glmnet.man) > 0.5)
levels(pred2) = c("0","1")
confusionMatrix(as.factor(comb.test$SUICTHNK), pred2)




# try boosting

library(caret)


boost.lv.train <- train(as.factor(SUICTHNK) ~ ., comb.train,method="gbm")
summary(boost.lv.train)

plot(varImp(boost.lv.train))

pred.lv2 = predict(boost.lv.train,comb.test,type="prob")
library(caret)

library(pROC)
roc_obj3 <- roc(comb.test$SUICTHNK, pred.lv2[,2])
roc_obj3
plot(roc_obj3)

pred3 = as.factor(pred.lv2[,2] > 0.5)
levels(pred3) = c("0","1")
confusionMatrix(as.factor(comb.test$SUICTHNK), pred3)



boost.man.train <- train(as.factor(SUICTHNK) ~ ., comb.train.man,method="gbm")
summary(boost.man.train)
plot(varImp(boost.man.train))

pred.boost.man = predict(boost.man.train,comb.test.man,type="prob")
library(caret)

library(pROC)
roc_obj4 <- roc(comb.test$SUICTHNK, pred.boost.man[,2])
roc_obj4
plot(roc_obj4)

pred4 = as.factor(pred.boost.man[,2] > 0.5)
levels(pred4) = c("0","1")
confusionMatrix(as.factor(comb.test$SUICTHNK), pred4)

# partial dependence plots


modd = boost.man.train$finalModel

plot(modd,2,type="response")
plot(modd,6,type="response")
plot(modd,4,type="response")

plot(modd,c("DSTHOP12","DSTNGD12"),type="response",col.regions=grDevices::gray.colors(50,.2,.8))

plot(modd,c("DSTHOP12","DSTCHR12","DSTNGD12"),type="response",level.plot=T,contour=T)#,col.regions=grDevices::gray0;grDevices::gray110)



# interaction
interact.gbm(modd, comb.train.man, i.var = c("DSTHOP12","DSTNGD12"))




# ---------------------------------------------------

# combine OV and FS

comb.comb.train <- data.frame(comb.train.man,comb.train[,1:2])

boost.comb.train <- train(as.factor(SUICTHNK) ~ ., comb.comb.train,method="gbm")
summary(boost.comb.train)
boost.comb.train
1-3plot(varImp(boost.comb.train))



comb.comb.test <- data.frame(comb.test.man,comb.test[,1:2])
pred.boost.comb = predict(boost.comb.train,comb.comb.test,type="prob")
roc_obj5 <- pROC::roc(comb.test$SUICTHNK, pred.boost.comb[,2]) # not working
roc_obj5
plot(roc_obj5)

library(ROCR)
predss = prediction(pred.boost.comb[,2],comb.test$SUICTHNK)
perf = performance(predss,"tpr","fpr")

plot(perf)
str(performance(predss,"auc"))

pred5 = as.factor(pred.boost.comb[,2] > 0.5)
levels(pred5) = c("0","1")
confusionMatrix(as.factor(comb.test$SUICTHNK), pred5)
 and 