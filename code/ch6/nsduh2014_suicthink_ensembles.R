setwd("C:/Users/rjacobuc/Desktop")
setwd("/Users/rjacobuc/Desktop")
setwd("G:/My Drive")
library(foreign)
dat <- read.spss("36361-0001-Data.sav",to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)
dat <- read.spss("NSDUH_2014.SAV",to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)

vars <- c("ADWRDEPR","ADWRDISC","ADWRLSIN","ADWRPLSR","ADWRELES","ADWREMOR","ADWRGAIN",
          "ADWRGROW","ADWRPREG","ADWRLOSE","ADWRDIET","ADWRSLEP","ADWRSMOR","ADWRENRG",
          "ADWRSLOW","ADWRSLNO","ADWRJITT","ADWRJINO","ADWRTHOT","ADWRCONC","ADWRDCSN",
          "ADWRNOGD","ADWRWRTH","ADPBINTF","ADRXHLP","ADTMTHLP","ADPBDLYA","ATXMDEYR","ARXMDEYR",
          "AMDETXRX","ADOCMDE","AOMDMDE","APSY1MDE","ASOCMDE","ACOUNMDE","AOMHMDE","ANURSMDE","ARELMDE",
          "AHBCHMDE","IRSEX","IRMARIT","CATAG6","NEWRACE2","SUICTHNK")
dat.sub <- dat[,vars]

#library(stringr)
#dat[,str_detect(colnames(dat), "SUI")]

# remove those with missing on suicide
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



# recode missingness
dat.sub5$ADPBINTF[is.na(dat.sub5$ADPBINTF)] <- 0
dat.sub5$ADRXHLP[is.na(dat.sub5$ADRXHLP)] <- 0
dat.sub5$ADTMTHLP[is.na(dat.sub5$ADTMTHLP)] <- 0
dat.sub5$ADPBDLYA[is.na(dat.sub5$ADPBDLYA)] <- 5

dat.sub5[,1:23][is.na(dat.sub5[,1:23])] <- 2

dat.sub5[is.na(dat.sub5)] <- 0

# select for suicide to increase proportion

sel1 = which(dat.sub5$SUICTHNK == 1)
sel0 = which(dat.sub5$SUICTHNK == 0)

set.seed(1)
ids11 <- sample(sel1,1000)
ids10 <- sample(sel0,1000)

dat.sub11 <- dat.sub5[ids11,]
dat.sub10 <- dat.sub5[ids10,]
dat2000 <- rbind(dat.sub11,dat.sub10)

# recode variables 
dat2000[,1:23] <- dat2000[,1:23] - 1
dat2000[,24:27] <- data.frame(scale(dat2000[,24:27]))


# recode SUICTHNK
dat2000$SUICTHNK <- as.factor(dat2000$SUICTHNK)
levels(dat2000$SUICTHNK) <- c("No","Yes")




# create subsample of 1000 for train, 1000 for test
set.seed(1)
ids.train <- sample(1:2000,1000)
dat.train <- dat2000[ids.train,]
dat.test <- dat2000[-ids.train,]

head(dat.train)


summary(dat.train)

library(caret)

# logistic regression

glm.out <- train(SUICTHNK ~ ., dat.train,method="glm",#tuneLength=10,
                 trControl=trainControl(summaryFunction=twoClassSummary,classProbs=T))
glm.out
#glm.out$finalModel

max(glm.out$results[,"ROC"])

# bagging
# go with default of 500 trees

bag.grid <- expand.grid(mtry=53)

bag.out <- train(SUICTHNK ~ ., dat.train,tuneGrid=bag.grid,method="rf",
                 trControl=trainControl(summaryFunction=twoClassSummary,classProbs=T))
bag.out


# random forests
library(doParallel)
cl <- makePSOCKcluster(4)
registerDoParallel(cl)


rf.grid <- expand.grid(mtry=c(3,10,20,35))

rf.out <- train(SUICTHNK ~ ., dat.train,tuneGrid=rf.grid,method="rf",
                 trControl=trainControl(summaryFunction=twoClassSummary,classProbs=T))
rf.out

max(rf.out$results[,"ROC"])

# boosting
# same parallel registration will work here


gbmGrid <-  expand.grid(interaction.depth = c(1,2,3,5), 
                         n.trees = c(100,500,2000), 
                         shrinkage = c(.001,.01,.1),
                         n.minobsinnode = 20)

gbm.out <- train(SUICTHNK ~ ., dat.train,tuneGrid=gbmGrid,method="gbm",
                trControl=trainControl(summaryFunction=twoClassSummary,classProbs=T))
gbm.out
plot(gbm.out) + scale_fill_grey()

ggplot(gbm.out) + scale_color_grey(start=.1,end=0.7) + theme_minimal()

which(gbm.out$results[,"ROC"] == max(gbm.out$results[,"ROC"]))

# choose final model
library(gbm)
new.gbm = update(gbm.out, param = list(interaction.depth = 2, 
                             n.trees = c(2000), 
                             shrinkage = c(.001),
                             n.minobsinnode = 20))

plot(varImp(new.gbm)) + 




# extreme gradient boosting

xgb.out <- train(SUICTHNK ~ ., dat.train,method="xgbTree",
                 trControl=trainControl(summaryFunction=twoClassSummary,classProbs=T))
xgb.out

max(xgb.out$results[,"ROC"])


# AUC plot on Test sample

glm.pred.test <- predict(glm.out,dat.test,type="prob")
bag.pred.test <- predict(bag.out,dat.test,type="prob")
rf.pred.test <- predict(rf.out,dat.test,type="prob")
gbm.pred.test <- predict(gbm.out,dat.test,type="prob")
xgb.pred.test <- predict(xgb.out,dat.test,type="prob")

library(pROC)

roc(dat.test$SUICTHNK, glm.pred.test[,2])
roc(dat.test$SUICTHNK, bag.pred.test[,2])
roc(dat.test$SUICTHNK, rf.pred.test[,2])
roc(dat.test$SUICTHNK, gbm.pred.test[,2])
roc(dat.test$SUICTHNK, xgb.pred.test[,2])

roc1 <- plot(roc(dat.test$SUICTHNK, glm.pred.test[,2]), print.auc = F, col = "black")
plot(roc(dat.test$SUICTHNK, bag.pred.test[,2]), print.auc = F, col = "black",add=T,lty=2)
plot(roc(dat.test$SUICTHNK, rf.pred.test[,2]), print.auc = F, col = "black",add=T,lty=3)
plot(roc(dat.test$SUICTHNK, gbm.pred.test[,2]), print.auc = F, col = "black",add=T,lty=4)
plot(roc(dat.test$SUICTHNK, xgb.pred.test[,2]), print.auc = F, col = "black",add=T,lty=5)

legend(0.3,0.5,legend=c("GLM","Bagging","RF","GBM","XGBoost"),lty=1:5)

#save.image("C:/Users/rjacobuc/Documents/Github/edm_book/ch6_ensembles/scripts/nsduh.RData")




# interpret boosting


library(pdp)

partial1 = pdp::partial(rf.out,pred.var="ADWRWRTH")
autoplot(partial1,smooth=F,plot.pdp=F,rug=T,train=dat.train,alpha=0.5,xlab="ADWRWRTH")

partial2 = pdp::partial(rf.out,pred.var="ADPBINTF")
autoplot(partial2,smooth=F,plot.pdp=F,rug=T,train=dat.train,alpha=0.5,xlab="ADPBINTF")

library(DALEX)

explainer_gbm <- explain(new.gbm, label = "gbm",
                               data = dat.train)

pdp1  <- model_profile(explainer_gbm, variable = "ADPBINTF", type = "partial")
plot(pdp1)
table(dat.train$ADPBINTF)


pdp2  <- model_profile(explainer_gbm, variable = "ADWRWRTH", type = "partial")
plot(pdp2)

table(dat.train$ADWRWRTH)


library(iml)

mod = Predictor$new(new.gbm, data = dat.train)
# Compute the interactions
ia = Interaction$new(mod)
plot(ia)


eff = FeatureEffect$new(mod, feature = c("ADPBINTF","ADWRWRTH"),method="pdp")
plot(eff) + scale_fill_grey()

library(dismo)
gbm.plot(new.gbm$finalModel)
inter <- gbm.interactions(new.gbm$finalModel)
inter$interactions
inter$rank.list
