library(lavaan);library(MASS)

set.seed(5600)

Nobs <- 1000
reps <- 1000

coefs.lm <- matrix(NA,reps,5)
coefs.ridge1 <- matrix(NA,reps,5)
coefs.ridge2 <- matrix(NA,reps,5)

for(i in 1:reps){

#sim.mod <- "y ~ 0.2*x1 + 0.2*x2 + 0.2*x3 + 0.2*x4 + 0.2*x5 + 0.2*x6 + 0.2*x7 + 0.2*x8 + 0.2*x9 + 0.2*x10"
sim.mod <- "y ~ 0.5*x1 + 0.25*x2 + 0.125*x3 + 0.0*x4 + 0.0*x5"

#fit.mod <- paste()

dat <- simulateData(sim.mod,sample.nobs=Nobs,seed=i)
summary(dat)

lm.out <- lm(y ~ ., dat)
summary(lm.out)


dat.sub <- dat[1:40,]

lm.out2 <- lm(y ~ ., dat.sub)
coefs.lm[i,] = coef(lm.out2)[-1]

ridge.out <- lm.ridge(y ~ ., dat.sub,lambda=10)
coefs.ridge1[i,] = coef(ridge.out)[-1]


ridge.out2 <- lm.ridge(y ~ ., dat.sub,lambda=50)
coefs.ridge2[i,] = coef(ridge.out2)[-1]

}


Fro <- apply(coefs.lm,2,mean)
lm.sd <- apply(coefs.lm,2,sd)
ridge1.mean <- apply(coefs.ridge1,2,mean)
ridge1.sd <- apply(coefs.ridge1,2,sd)
ridge2.mean <- apply(coefs.ridge2,2,mean)
ridge2.sd <- apply(coefs.ridge2,2,sd)

library(reshape)
means <- cbind(lm.mean,ridge1.mean,ridge2.mean)
means.long = melt(means)
sd <- cbind(lm.sd,ridge1.sd,ridge2.sd)
sd.long = melt(sd)
means.long$Method = c(rep("OLS",5),rep("Ridge1",5),rep("Ridge2",5))
means.long$se = sd.long$value
library(ggplot2)

means.long$variable <- as.factor(means.long$X1)


#Rogier mods
#add true values
means.long$true<-rep(c(0.5,0.25,0.125,0.0,0.0),times=3) 
#Plot
pl <- ggplot(means.long,aes(variable,value)) + geom_point() + geom_errorbar(aes(ymin=value-se,ymax=value+se))+
  theme_minimal()+xlab("Predictor")+ylab("Estimate")+facet_wrap(~Method)+geom_point(aes(x=variable,y=true),shape=8,size=2,colour='black')#+theme_set(theme_grey(base_size = 22)) #increase text size
pl
ggsave(filename = "bias-variance.pdf",plot = pl)

