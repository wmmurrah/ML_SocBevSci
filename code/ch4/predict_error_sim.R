library(psych);library(MASS)

bfi.comp <- bfi[complete.cases(bfi),]
X <- data.frame(scale(bfi.comp[,1:25]))
X2 <- data.frame(X,sex=bfi.comp[,26]-1,education=scale(bfi.comp$education))
Y <- as.numeric(scale(bfi.comp$age))



samps <- seq(40,1000,10)
mean.ret <- matrix(NA,length(samps),2)

for(j in 1:length(samps)){

ret.matrix = matrix(NA,500,2)
for(i in 1:500){

ids <- sample(1:nrow(bfi.comp),samps[j])

x.train <- data.matrix(X2[ids,])
x.test <- data.matrix(X2[-ids,])
y.train <- Y[ids]
y.test <- Y[-ids]

dat.train <- data.frame(x.train,y.train)
dat.test <- data.frame(x.test,y.test)
lm.out <- lm(y.train ~ .-1, dat.train)
y.hat <- predict(lm.out,dat.test)
ret.matrix[i,1] = cor(y.hat,y.test)**2



glm.out <- cv.glmnet(x.train,y.train,alpha=0)
pred.x = x.test
y.hat <-  cbind(rep(1,nrow(pred.x)),pred.x) %*% coef(glm.out,glm.out$lambda.min)
#ret.matrix[i,2] = cor(y.hat,y.test)**2

ret.matrix[i,2] = cor(y.hat@x,y.test)**2

#ridge.out <- lm.ridge(y.train ~ ., dat.train,lambda=1)
#pred.x = x.test
#y.hat <-  pred.x %*% c(ridge.out$coef)
#ret.matrix[i,2] = cor(y.hat,y.test)**2
}

mean.ret[j,] = round(colMeans(ret.matrix[complete.cases(ret.matrix),]),3)

}

library(ggplot2)
qplot(samps,mean.ret[,2],ylim=c(0,.12),xlab="Sample Size",ylab="R-squared") +
  geom_smooth()



plot(samps,mean.ret[,1],ylim=c(0,.12),type="l",xlab="Sample Size",ylab="R-squared")
abline(0,0)
lines(samps,mean.ret[,2],lty=2)
legend(600, y=0.04, legend=c("OLS", "Ridge"),
       lty=1:2, cex=1)
