setwd("C:/Users/rjacobuc/Desktop")
setwd("/Users/rjacobuc/Desktop")
library(foreign)

# http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/NSDUH-2014/NSDUH-2014-datasets/NSDUH-2014-DS0001/NSDUH-2014-DS0001-info/NSDUH-2014-DS0001-info-codebook.pdf




dat <- read.spss("36361-0001-Data.sav",to.data.frame=TRUE,use.value.labels=FALSE,reencode=FALSE)
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

race <- as.factor(dat.sub2$NEWRACE2)
levels(race) <- c("white","black","native","pac","asian","multi","hisp")
race = as.character(race)
SUICTHNK <- as.factor((dat.sub2$SUICTHNK*-1)+2)


# dummy code cat -- age
age <- as.factor(dat.sub2$CATAG6)
levels(age) <- c("18-25","26-34","35-49","50-64","65 or older")



# recode missingness
dep = (dat.sub2$ADWRDEPR*-1)+2
interfere = as.factor(dat.sub2$ADPBINTF)
worthless = dat.sub2$ADWRWRTH
worthless[worthless==2] = 0
sex = dat.sub2$IRSEX - 1


dat.comb =  data.frame(SUICTHNK, sex, age, race, dep, interfere, worthless)
dat.comp = dat.comb[complete.cases(dat.comb),]
# select for suicide to increase proportion

sel1 = which(dat.comp$SUICTHNK == 1)
sel0 = which(dat.comp$SUICTHNK == 0)

set.seed(1)
ids11 <- sample(sel1,1000)
ids10 <- sample(sel0,1000)

dat.sub11 <- dat.comp[ids11,]
dat.sub10 <- dat.comp[ids10,]
dat2000 <- rbind(dat.sub11,dat.sub10)

set.seed(1)
ids.train <- sample(1:2000,1000)
dat.train <- dat2000[ids.train,]
dat.test <- dat2000[-ids.train,]


# rpart on train data

# create subset of related predictors
library(rpart);library(rpart.plot)
tree.out = rpart(SUICTHNK ~ . +  -age, dat.train,control=rpart.control(maxdepth=2))
printcp(tree.out)

rpart.plot(tree.out)

tree0 = prune(tree.out,0.17)
rpart.plot(tree0,box.palette="white")

tree1 = prune(tree.out,0.03)
rpart.plot(tree1,box.palette="white")

tree2 = prune(tree.out,0.015)
rpart.plot(tree2,box.palette="white")

tree.out2 = rpart(SUICTHNK ~ ., dat.train[,c(1,2,4,5,6,7)],control=rpart.control(maxdepth=4, cp=0.000001))
printcp(tree.out2)

tree3 = prune(tree.out2,0.016)
rpart.plot(tree3,box.palette="white")

tree4 = prune(tree.out2,0.0282)
rpart.plot(tree4,box.palette="white")


(tree44 = prune(tree.out2,0.03))
cp = printcp(tree.out)
library(xtable)
xtable(cp)

library(rpart.utils)
list.rules.rpart(tree3)
rpart.rules(tree3)



