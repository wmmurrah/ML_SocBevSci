
#wisc <- read.table("C:/Users/rjacobuc/Documents/GitHub/EDM_Labs/2015/wisc4vpe.dat")
wisc <- read.table("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch9_MLandHLM/scripts/wisc4vpe.dat")

#wisc <- read.table(file.choose()) # use directory to find wisc4vpe.dat
names(wisc)<- c("V1","V2","V4","V6","P1","P2","P4", "P6", "Moeducat")

# install longRPart2 from source



wisc.verb <- wisc[,c(1:4,9)]

# create subset for plotting
ntot <- nrow(wisc.verb)    # total number of observations
wisc.verb.sel <- wisc.verb[sample(ntot, 30), ]

wisc.long <- reshape(wisc.verb, varying = c("V1", "V2", "V4", "V6"), v.names = "verbal",
                     times = c(1, 2, 4, 6), direction = "long")

wisc.long.sel <- reshape(wisc.verb.sel, varying = c("V1", "V2", "V4", "V6"),
                         v.names = "verbal", times = c(1, 2, 4, 6),
                         direction = "long")
head(wisc.long,3)
names(wisc.long)[2] <- "grade"
names(wisc.long.sel)[2] <- "grade"


library(lattice)
xyplot(verbal ~ grade, groups = id, data = wisc.long.sel, type = "o", col = "black", 
       xlab = "Grade of Testing", ylab = "Verbal[t]")



# add noise predictors
library(MASS)
npreds = 20
set.seed(1)
noise <- mvrnorm(nrow(wisc),rep(0,npreds),diag(npreds))
noise2 = rbind(noise,noise,noise,noise)


### works well


library(lmmlasso)
x = as.matrix(cbind(rep(1,nrow(wisc.long)),wisc.long$grade,wisc.long$Moeducat,noise2))
y = as.matrix(wisc.long$verbal)

# z means we are only estimating random effects for intercept and grade
# weights means we don't penalize both intercept and grade (NA), but fully penalize mother's education
# lambda is penalty, different scaling than in regsem
# pdMat allows us to estimate correlation between random effects
pens = 1:20
ret.list <- list()
for(i in 1:length(pens)){
  ret.list[[i]] <- lmmlasso(x, y, z = x[,1:2],grp=wisc.long$id,weights=c(NA,NA,rep(1,21)),lambda=pens[i],pdMat="pdSym")
}


#lmm.out <- lmmlasso(x, y, z = x[,1:2],grp=wisc.long$id,weights=c(NA,NA,rep(1,21)),lambda=1,pdMat="pdSym")


zero_pen <- lmmlasso(x, y, z = x[,1:2],grp=wisc.long$id,
                          weights=c(NA,NA,rep(1,21)),lambda=0,pdMat="pdSym")
summary(zero_pen)

summary(ret.list[[8]])

# look at parameter trajectories
par(mar = c(0, 0, 0, 0))

coefs = matrix(NA,20,21)
for(i in 1:20){
  coefs[i,] = ret.list[[i]]$coefficients[3:23]
}

plot(pens,coefs[,1],type="l",ylim=c(-.9,3.3),ylab="Parameter Coefficient",xlab="Penalty")

for(j in 2:21){
  lines(coefs[,j])
}


bic = rep(NA,20)
for(i in 1:20){
  bic[i] = ret.list[[i]]$bic
}
plot(pens,bic,ylab="BIC",xlab="Penalty")


# compare

mix1 <- nlme::lme(fixed = verbal ~ grade, random = ~ grade | id,
                  data = wisc.long, method="ML" )
summary(mix1) # get same esti




# extended to 
#install.packages("glmmixedlasso", repos="http://R-Forge.R-project.org")
library(glmmixedlasso)

#### doesn't do what we want
library(glmmLasso)

wisc.long2 = data.frame(wisc.long,noise2)
#wisc.long2$grade <- as.factor(wisc.long$grade)
glm1 <- glmmLasso(verbal ~ grade + X1 + X2 + X3 + Moeducat,data=wisc.long2, 
                  rnd=list(grade=~1,id=~1), lambda=4000,
                  control=glmmLassoControl(standardize=FALSE,center=FALSE))
summary(glm1)
