#setwd("C:/Users/rjacobuc/Desktop")
#setwd("/Users/rjacobuc/Desktop")
setwd("G:/My Drive")
setwd("/Volumes/GoogleDrive/My Drive/")
library(foreign)
dat <- read.spss("36361-0001-Data.sav",to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)
vars <- c("ADWRDEPR","ADWRDISC","ADWRLSIN","ADWRPLSR","ADWRELES","ADWREMOR","ADWRGAIN",
          "ADWRGROW","ADWRPREG","ADWRLOSE","ADWRDIET","ADWRSLEP","ADWRSMOR","ADWRENRG",
          "ADWRSLOW","ADWRSLNO","ADWRJITT","ADWRJINO","ADWRTHOT","ADWRCONC","ADWRDCSN",
          "ADWRNOGD","ADWRWRTH","ADPBINTF","ADRXHLP","ADTMTHLP","ADPBDLYA","ATXMDEYR","ARXMDEYR",
          "AMDETXRX","ADOCMDE","AOMDMDE","APSY1MDE","ASOCMDE","ACOUNMDE","AOMHMDE","ANURSMDE","ARELMDE",
          "AHBCHMDE","IRSEX","IRMARIT","CATAG6","NEWRACE2","SUICTHNK")
dat.sub <- dat[,vars]

table(dat.sub$SUICTHNK)/nrow(dat.sub)

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

# create subsample of 1000 for train, 1000 for test
set.seed(1)
ids.train <- sample(1:2000,1000)
dat.train <- dat2000[ids.train,]
dat.test <- dat2000[-ids.train,]


summary(dat.train)

##### create plots

# simulate data

x <- rnorm(100)
y <- ifelse(x>0,1,0)
x <- x + rnorm(100,0,.3)

glm.sim <- glm(y ~ x, family="binomial")
summary(glm.sim)



plot(x,y,
     xlab="Y",ylab="X") # plot with body size on x-axis 
curve(predict(glm.sim,data.frame(x=x),type="resp"),add=TRUE)


library(pROC)
plot(roc(y,predict(glm.sim,type="response")))


sui = dat.train$SUICTHNK[1:150]
adp = rowSums(dat.train[1:150,24:27]) + rnorm(150)

glm.simp <- glm(sui ~ adp, family="binomial")
summary(glm.simp)

plot(adp,sui,
     xlab="Depression",ylab="Suicidal Thoughts") # plot with body size on x-axis 
curve(predict(glm.simp,data.frame(adp=x),type="resp"),add=TRUE)

# interpret

intercept <- glm(SUICTHNK ~ 1, dat.train,family="binomial")
summary(intercept)
exp(-0.02)

adp = (rowSums(dat.train[,24:27]) - mean(rowSums(dat.train[,24:27]))) / (sd(rowSums(dat.train[,24:27])))
one.pred <- glm(SUICTHNK ~ adp, dat.train,family="binomial")
summary(one.pred)
exp(coef(one.pred))


preds11 = as.numeric(predict(one.pred,type="response") > .5)
caret::confusionMatrix(dat.train$SUICTHNK,preds11)

plot(adp,dat.train$SUICTHNK,
     xlab="Depression",ylab="Suicidal Thoughts") # plot with body size on x-axis 
curve(predict(one.pred,data.frame(adp=x),type="resp"),add=TRUE)

# run model


glm.train <- glm(SUICTHNK ~ ., dat.train,family="binomial")
summary(glm.train)

preds = round(predict(glm.train,type="response"),0)
library(caret)
confusionMatrix(dat.train$SUICTHNK,preds)

# on holdout

preds.test = round(predict(glm.train,dat.test,type="response"),0)
library(caret)
confusionMatrix(dat.test$SUICTHNK,preds.test)


library(glmnet)

y.train <- as.numeric(dat.train[,"SUICTHNK"])
x.train <- data.matrix(dat.train[,-41])
y.test <- as.numeric(dat.test[,"SUICTHNK"])
x.test <- data.matrix(dat.test[,-41])

lasso.out <- cv.glmnet(x.train,y.train)
coef(lasso.out,lasso.out$lambda.1se)
preds.lasso <- round(predict(lasso.out,x.train,s=lasso.out$lambda.1se),0)
confusionMatrix(dat.train$SUICTHNK,preds.lasso)
preds.lasso.test <- round(predict(lasso.out,x.test,s=lasso.out$lambda.1se),0)
confusionMatrix(dat.test$SUICTHNK,preds.lasso.test)