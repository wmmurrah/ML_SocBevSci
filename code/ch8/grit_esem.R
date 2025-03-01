grit1 = read.csv("C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/data.csv",header=T,sep="")

# set of models
# factor score for grit, use regression
# factor score for grit, use boosting
# sem model with predictors, latent variable for grit
# sem model with latent variable for predictors, latent variable for grit
# sem trees with grit as latent variable
# sem forests with grit as latent variable

levels(grit1$O10)

grit1$O10[grit1$O10 == "iOS" | grit1$O10 == "Linux"| grit1$O10 == "Macintosh" | grit1$O10 == "Windows"] = NA

grit2 <- grit1[complete.cases(grit1),c(3:14,31:92)]

grit2 = data.matrix(grit2)
grit2[,74] = grit2[,74] - 2

# take sample of 1000 for each
set.seed(1)
ids1 = sample(1:nrow(grit2),400)
grit.train = grit2[ids1[1:200],]
grit.test = grit2[ids1[201:400],]

library(lavaan)

mod.grit <- "
grit =~ GS1+GS2+GS3+GS4+GS5+GS6+GS7+GS8+GS9+GS10+GS11+GS12
"
cfa.out = cfa(mod.grit,grit.train)
summary(cfa.out,fit=T)

library(psych)

fa.out = fa(grit.train[,1:12],2)
summary(fa.out)
fa.out$loadings


mod.grit2 <- "
grit1 =~ GS2+GS3+GS5+GS7+GS8+GS9+GS11
grit2 =~ GS1+GS4+GS6+GS8+GS9+GS10+GS11+GS12
"
cfa.out2 = cfa(mod.grit2,grit.train)
summary(cfa.out2,fit=T)

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

# test
cor(predict(lm.out,grit.test2),grit.test2$grit1)**2
cor(predict(lm.out2,grit.test2),grit.test2$grit2)**2


# try ridge to reduce
library(glmnet)
X.train = data.matrix(grit.train2[,1:62])
X.test = data.matrix(grit.test2[,1:62])

grit1.train = as.numeric(grit.train2$grit1)
grit2.train = as.numeric(grit.train2$grit2)
grit1.test = as.numeric(grit.test2$grit1)
grit2.test = as.numeric(grit.test2$grit2)

glmnet.out1 <- cv.glmnet(X.train,grit1.train)
glmnet.out2 <- cv.glmnet(X.train,grit2.train)

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

library(MplusAutomation)



setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/mplus')
dir.create("grit1")
setwd("grit1")


script <- mplusObject(
  TITLE = "Using ESEM to Grit;",
  VARIABLE = "USEVARIABLES = education urban gender engnat age 
hand religion orientation race voted married familysize 
A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 
C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 
O1 O2 O3 O4 O5 O6 O7 O8 O9 O10 
E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 
N1 N2 N3 N4 N5 N6 N7 N8 N9 N10
GS2 GS3 GS5 GS7 GS8 GS9 GS11
GS1 GS4 GS6 GS10 GS12;",
  MODEL = 
    "grit1 BY GS2 GS3 GS5 GS7 GS8 GS9 GS11;
     grit2 BY GS1 GS4 GS6 GS8 GS9 GS10 GS11 GS12;
    f1-f5 BY A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 
        C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 
        O1 O2 O3 O4 O5 O6 O7 O8 O9 O10 
        E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 
        N1 N2 N3 N4 N5 N6 N7 N8 N9 N10 (*1);
  grit1 ON f1-f5 education urban gender engnat age 
hand religion orientation race voted married familysize;
  grit2 ON f1-f5 education urban gender engnat age 
hand religion orientation race voted married familysize;",
usevariables=c("education","urban","gender","engnat","age",
"hand","religion","orientation","race","voted","married","familysize",
"A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
"C1","C2","C3","C4","C5","C6","C7","C8","C9","C10",
"O1","O2","O3","O4","O5","O6","O7","O8","O9","O10",
"E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
"N1","N2","N3","N4","N5","N6","N7","N8","N9","N10",
"GS2","GS3","GS5","GS7","GS8","GS9","GS11",
"GS1","GS4","GS6","GS10","GS12"),
  rdata = data.frame(grit.train),
OUTPUT=c("sampstat","standardized"))


mod = mplusModeler(script,run=1L,modelout = "Model.inp")
summary(mod,verbose=T)
showSummaryTable(mod)

mod$parameters$std.standardized

mod$results$parameters$r2

# check to see if get same result as lavaan


setwd('C:/Users/rjacobuc/Documents/GitHub/sem_prediction/grit/mplus')
dir.create("grit2")
setwd("grit2")


script2 <- mplusObject(
  TITLE = "Using ESEM to Grit;",
  VARIABLE = "USEVARIABLES = education urban gender engnat age 
  hand religion orientation race voted married familysize 
  A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 
  C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 
  O1 O2 O3 O4 O5 O6 O7 O8 O9 O10 
  E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 
  N1 N2 N3 N4 N5 N6 N7 N8 N9 N10
  GS2 GS3 GS5 GS7 GS8 GS9 GS11
  GS1 GS4 GS6 GS10 GS12;",
  MODEL = 
    "grit1 BY GS2 GS3 GS5 GS7 GS8 GS9 GS11;
  grit2 BY GS1 GS4 GS6 GS8 GS9 GS10 GS11 GS12;
  grit1 ON education urban gender engnat age 
  hand religion orientation race voted married familysize 
  A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 
  C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 
  O1 O2 O3 O4 O5 O6 O7 O8 O9 O10 
  E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 
  N1 N2 N3 N4 N5 N6 N7 N8 N9 N10;
  grit2 ON education urban gender engnat age 
  hand religion orientation race voted married familysize
  A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 
  C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 
  O1 O2 O3 O4 O5 O6 O7 O8 O9 O10 
  E1 E2 E3 E4 E5 E6 E7 E8 E9 E10 
  N1 N2 N3 N4 N5 N6 N7 N8 N9 N10;",
  usevariables=c("education","urban","gender","engnat","age",
                 "hand","religion","orientation","race","voted","married","familysize",
                 "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                 "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10",
                 "O1","O2","O3","O4","O5","O6","O7","O8","O9","O10",
                 "E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                 "N1","N2","N3","N4","N5","N6","N7","N8","N9","N10",
                 "GS2","GS3","GS5","GS7","GS8","GS9","GS11",
                 "GS1","GS4","GS6","GS10","GS12"),
  rdata = data.frame(grit.train),
  OUTPUT=c("sampstat","standardized"))


mod2 = mplusModeler(script2,run=1L,modelout = "Model2.inp")
summary(mod2)

mod2$results$parameters$r2
