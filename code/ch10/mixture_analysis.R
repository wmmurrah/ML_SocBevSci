# read in data_kev.csv
# grit1 = read.csv("C:\\Users\\kjgrimm\\Documents\\ASU\\Data Mining Book\\Chapter 9\\data.csv", header=T, sep=",")
grit1 <- read.csv("code/ch10/data_kev.csv", header = TRUE, sep = ",")
summary(grit1)
dim(grit1)


#install.packages('poLCA')
library(poLCA)

summary(grit1[,c('GS1','GS2','GS3','GS4','GS5','GS6','GS7','GS8','GS9','GS10','GS11','GS12')])

ind99 = grit1[,c(3:14,43:92)] == 0 & is.na(grit1[,c(3:14,43:92)]) ==F
grit1[,c(3:14,43:92)][ind99] <- NA
head(grit1)

grit2 = grit1[,c(3:14,31:92)]
grit3 = na.omit(grit2)
dim(grit3)
head(grit3)

#Reverse coding inversely valenced items
grit3$GS1  = 6 - grit3$GS1
grit3$GS4  = 6 - grit3$GS4
grit3$GS6  = 6 - grit3$GS6
grit3$GS9  = 6 - grit3$GS9
grit3$GS10 = 6 - grit3$GS10
grit3$GS12 = 6 - grit3$GS12

grit3$E2   = 6 - grit3$E2
grit3$E4   = 6 - grit3$E4
grit3$E6   = 6 - grit3$E6
grit3$E8   = 6 - grit3$E8
grit3$E10  = 6 - grit3$E10

grit3$N2   = 6 - grit3$N2
grit3$N4   = 6 - grit3$N4

grit3$A1   = 6 - grit3$A1
grit3$A3   = 6 - grit3$A3
grit3$A5   = 6 - grit3$A5
grit3$A7   = 6 - grit3$A7

grit3$C2   = 6 - grit3$C2
grit3$C4   = 6 - grit3$C4
grit3$C6   = 6 - grit3$C6
grit3$C8   = 6 - grit3$C8

grit3$O2   = 6 - grit3$O2
grit3$O4   = 6 - grit3$O4
grit3$O6   = 6 - grit3$O6

# Creating sum scores for Big-5
grit3$ex = (grit3$E1 + grit3$E2 + grit3$E3 + grit3$E4 + grit3$E5 + grit3$E6 + grit3$E7 + grit3$E8 + grit3$E9 + grit3$E10)/10
grit3$nu = (grit3$N1 + grit3$N2 + grit3$N3 + grit3$N4 + grit3$N5 + grit3$N6 + grit3$N7 + grit3$N8 + grit3$N9 + grit3$N10)/10
grit3$ag = (grit3$A1 + grit3$A2 + grit3$A3 + grit3$A4 + grit3$A5 + grit3$A6 + grit3$A7 + grit3$A8 + grit3$A9 + grit3$A10)/10
grit3$co = (grit3$C1 + grit3$C2 + grit3$C3 + grit3$C4 + grit3$C5 + grit3$C6 + grit3$C7 + grit3$C8 + grit3$C9 + grit3$C10)/10
grit3$op = (grit3$O1 + grit3$O2 + grit3$O3 + grit3$O4 + grit3$O5 + grit3$O6 + grit3$O7 + grit3$O8 + grit3$O9 + grit3$O10)/10

head(grit3)
# Setting the seed for replication purposes
set.seed(20191709)

# Taking random sample of 1000
ids1       = sample(1:nrow(grit3),1000)
grit.train = grit3[ids1[1:500],]
grit.test  = grit3[ids1[501:1000],]

cor(grit.train[,c('GS1','GS2','GS3','GS4','GS5','GS6','GS7','GS8','GS9','GS10','GS11','GS12')])

# 1-class Model without predictors
model.1 = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ 1, grit.train, nclass = 1, na.rm = FALSE, nrep = 10, verbose=FALSE)
model.1

# 2-class Model without predictors
model.2 = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ 1, grit.train, nclass = 2, na.rm = FALSE, nrep = 10, verbose=FALSE)
model.2

vars = as.matrix(cbind(grit.train$GS1, grit.train$GS2, grit.train$GS3, grit.train$GS4, grit.train$GS5, grit.train$GS6, grit.train$GS7, grit.train$GS8, grit.train$GS9, grit.train$GS10, grit.train$GS11, grit.train$GS12))
probs = poLCA.posterior(model.2, vars)
probs = na.omit(probs)

for (i in 1:nrow(probs)){
  if (probs[i,1]==1) probs[i,1] = .99999
  if (probs[i,2]==1) probs[i,2] = .99999

  if (probs[i,1]==0) probs[i,1] = .00001
  if (probs[i,2]==0) probs[i,2] = .00001
}

total   = probs[,1]*log(probs[,1]) + probs[,2]*log(probs[,2])
entropy = 1 + (sum(total)/(log(2) * nrow(probs))) 
entropy
#0.854

# 3-class Model without predictors
model.3 = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ 1, grit.train, nclass = 3, na.rm = FALSE, nrep = 10, verbose=FALSE)
model.3

probs = poLCA.posterior(model.3, vars)
probs = na.omit(probs)
for (i in 1:nrow(probs)){
  if (probs[i,1]==1) probs[i,1] = .99999
  if (probs[i,2]==1) probs[i,2] = .99999
  if (probs[i,3]==1) probs[i,3] = .99999
  
  if (probs[i,1]==0) probs[i,1] = .00001
  if (probs[i,2]==0) probs[i,2] = .00001
  if (probs[i,3]==0) probs[i,3] = .00001
}

total   = probs[,1]*log(probs[,1]) + probs[,2]*log(probs[,2]) + probs[,3]*log(probs[,3])
entropy = 1 + (sum(total)/(log(3) * nrow(probs))) 
entropy
#0.867

# 4-class Model without predictors
model.4 = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ 1, grit.train, nclass = 4, na.rm = FALSE, nrep = 10, verbose=FALSE)
model.4

probs = na.omit(poLCA.posterior(model.4, vars))

for (i in 1:nrow(probs)){
  if (probs[i,1]==1) probs[i,1] = .99999
  if (probs[i,2]==1) probs[i,2] = .99999
  if (probs[i,3]==1) probs[i,3] = .99999
  if (probs[i,4]==1) probs[i,4] = .99999
  
  if (probs[i,1]==0) probs[i,1] = .00001
  if (probs[i,2]==0) probs[i,2] = .00001
  if (probs[i,3]==0) probs[i,3] = .00001
  if (probs[i,4]==0) probs[i,4] = .00001
}

total   = probs[,1]*log(probs[,1]) + probs[,2]*log(probs[,2]) + probs[,3]*log(probs[,3]) + probs[,4]*log(probs[,4])
entropy = 1 + (sum(total)/(log(4) * nrow(probs))) 
entropy
#0.865

# 5-class Model without predictors
model.5 = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ 1, grit.train, nclass = 5, na.rm = FALSE, nrep = 10, verbose=FALSE, maxiter = 2000)
model.5

probs = na.omit(poLCA.posterior(model.5, vars))

for (i in 1:nrow(probs)){
  if (probs[i,1]==1) probs[i,1] = .99999
  if (probs[i,2]==1) probs[i,2] = .99999
  if (probs[i,3]==1) probs[i,3] = .99999
  if (probs[i,4]==1) probs[i,4] = .99999
  if (probs[i,5]==1) probs[i,5] = .99999
  
  if (probs[i,1]==0) probs[i,1] = .00001
  if (probs[i,2]==0) probs[i,2] = .00001
  if (probs[i,3]==0) probs[i,3] = .00001
  if (probs[i,4]==0) probs[i,4] = .00001
  if (probs[i,5]==0) probs[i,5] = .00001
}

total   = probs[,1]*log(probs[,1]) + probs[,2]*log(probs[,2]) + probs[,3]*log(probs[,3]) + probs[,4]*log(probs[,4]) + probs[,5]*log(probs[,5])
entropy = 1 + (sum(total)/(log(5) * nrow(probs))) 
entropy

# #Item 1
# .50 * 5 + .30 * 4 + .17 * 3 + .03 * 2 + .00 * 1
# .32 * 5 + .21 * 4 + .36 * 3 + .09 * 2 + .03 * 1
# .13	* 5 + .30 * 4 + .46 * 3 + .09 * 2 + .01 * 1
# 
# #Item 2
# .11 * 5 + .21 * 4 + .35 * 3 + .22 * 2 + .12 * 1
# .02 * 5 + .03 * 4 + .09 * 3 + .14 * 2 + .73 * 1
# .01	* 5 + .11 * 4 + .30 * 3 + .41 * 2 + .18 * 1
# 
# #Item 3
# .12 * 5 + .43 * 4 + .26 * 3 + .11 * 2 + .08 * 1
# .02 * 5 + .01 * 4 + .12 * 3 + .24 * 2 + .60 * 1
# .01	* 5 + .23 * 4 + .25 * 3 + .36 * 2 + .12 * 1
# 
# #Item 4
# .30 * 5 + .32 * 4 + .22 * 3 + .12 * 2 + .04 * 1
# .15 * 5 + .15 * 4 + .32 * 3 + .24 * 2 + .15 * 1
# .00	* 5 + .21 * 4 + .37 * 3 + .34 * 2 + .08 * 1
# 
# #Item 5
# .16 * 5 + .34 * 4 + .29 * 3 + .13 * 2 + .08 * 1
# .02 * 5 + .00 * 4 + .00 * 3 + .24 * 2 + .74 * 1
# .05	* 5 + .16 * 4 + .28 * 3 + .35 * 2 + .16 * 1
# 
# #Item 6
# .72 * 5 + .21 * 4 + .06 * 3 + .00 * 2 + .00 * 1
# .29 * 5 + .28 * 4 + .19 * 3 + .18 * 2 + .07 * 1
# .19	* 5 + .37 * 4 + .27 * 3 + .15 * 2 + .02 * 1
# 
# #Item 7
# .18 * 5 + .49 * 4 + .26 * 3 + .06 * 2 + .02 * 1
# .00 * 5 + .07 * 4 + .07 * 3 + .31 * 2 + .54 * 1
# .02	* 5 + .20 * 4 + .38 * 3 + .37 * 2 + .03 * 1
# 
# #Item 8
# .22 * 5 + .43 * 4 + .22 * 3 + .10 * 2 + .03 * 1
# .00 * 5 + .07 * 4 + .10 * 3 + .20 * 2 + .64 * 1
# .00	* 5 + .12 * 4 + .30 * 3 + .45 * 2 + .13 * 1
# 
# #Item 9
# .44 * 5 + .37 * 4 + .17 * 3 + .02 * 2 + .00 * 1
# .11 * 5 + .15 * 4 + .35 * 3 + .26 * 2 + .13 * 1
# .02	* 5 + .22 * 4 + .49 * 3 + .26 * 2 + .01 * 1
# 
# #Item 10
# .51 * 5 + .23 * 4 + .19 * 3 + .06 * 2 + .02 * 1
# .11 * 5 + .10 * 4 + .22 * 3 + .29 * 2 + .27 * 1
# .08	* 5 + .22 * 4 + .27 * 3 + .32 * 2 + .11 * 1
# 
# #Item 11
# .03 * 5 + .31 * 4 + .31 * 3 + .21 * 2 + .14 * 1
# .02 * 5 + .02 * 4 + .15 * 3 + .34 * 2 + .47 * 1
# .08	* 5 + .21 * 4 + .45 * 3 + .24 * 2 + .02 * 1
# 
# #Item 12
# .57 * 5 + .28 * 4 + .13 * 3 + .02 * 2 + .00 * 1
# .27 * 5 + .19 * 4 + .32 * 3 + .14 * 2 + .07 * 1
# .08	* 5 + .33 * 4 + .45 * 3 + .13 * 2 + .01 * 1


# 3-class Model with predictors

model.3p = poLCA(cbind(GS1, GS2, GS3, GS4, GS5, GS6, GS7, GS8, GS9, GS10, GS11, GS12) ~ ex + nu + ag + co + op, grit.train, nclass = 3, na.rm = FALSE, nrep = 10, verbose=FALSE)
model.3p

