data.1 = read.table('/Users/Ross/Google Drive/edm_book/ch1_intro/scripts/apexpos.dat')
data.1 = read.table("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch1_intro/scripts/apexpos.dat")
data.1 = read.table("/Users/rjacobuc/Documents/GitHub/edm_book/ch1_intro/scripts/apexpos.dat")
data.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch3_practices/scripts/apexpos.dat')
names(data.1) = c('id', 'apexpos', 'fsiq7')
head(data.1)

data.test = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch3_practices/scripts/apexpos_test.dat')
data.test = read.table("/Users/rjacobuc/Documents/GitHub/edm_book/ch1_intro/scripts/apexpos_test.dat")
names(data.test) = c('id', 'apexpos', 'fsiq7')


# Sort Data
data.2 <- data.1[order(data.1$apexpos),]
data.test <- data.test[order(data.test$apexpos),]



coef.lm = matrix(NA,100,2)
coef.poly = matrix(NA,100,5)
rsq = matrix(NA,100,4)

for(i in 1:100){

samp1 = data.2[sample(1:314,50),]

lm.1 = lm(fsiq7 ~apexpos,samp1)
coef.lm[i,] = coef(lm.1)
rsq[i,1] = cor(samp1$fsiq7,predict(lm.1))**2
rsq[i,2] = cor(data.test$fsiq7,predict(lm.1,data.test))**2

lm.2 = lm(fsiq7 ~poly(apexpos,4),samp1)
coef.poly[i,] = coef(lm.2)
rsq[i,3] = cor(samp1$fsiq7,predict(lm.2))**2
rsq[i,4] = cor(data.test$fsiq7,predict(lm.2,data.test))**2
}

colMeans(coef.lm)


library(reshape)
means <- c(colMeans(coef.lm), colMeans(coef.poly))
means.long = melt(means)
sd <- c(apply(coef.lm,2,sd), apply(coef.poly,2,sd))
sd.long = melt(sd)
means.long$Method = c("lm.b0","lm.b1","poly.b0","poly.b1","poly.b2","poly.b3","poly.b4")
#means.long$variable = c("int","b","int","b","b2","b3","b4")
means.long$se = sd.long$value
library(ggplot2)


pl <- ggplot(means.long,aes(Method,value)) + geom_point() + geom_errorbar(aes(ymin=value-(se*2),ymax=value+(se*2)))
pl +  theme_minimal()+xlab("Parameter")+ylab("Estimate") + geom_abline(intercept=0,slope=0) +  theme(axis.text.x = element_text(angle = -45, hjust = 0))



library(reshape)
means <- c(colMeans(rsq))
means.long = melt(means)
sd <- c(apply(rsq,2,sd))
sd.long = melt(sd)
means.long$Method = c("lm.train","lm.test","poly.train","poly.test")
#means.long$variable = c("int","b","int","b","b2","b3","b4")
means.long$se = sd.long$value
library(ggplot2)


pl <- ggplot(means.long,aes(Method,value)) + geom_point() + geom_errorbar(aes(ymin=value-(se*2),ymax=value+(se*2)))
pl +  theme_minimal()+xlab("Parameter")+ylab("Estimate") + geom_abline(intercept=0,slope=0) +  theme(axis.text.x = element_text(angle = -45, hjust = 0))





# different way to characterize




#rsq.train = matrix(NA,1000,10)
#rsq.test = matrix(NA,1000,10)
mse.train = matrix(NA,1000,10)
mse.test = matrix(NA,1000,10)

for(i in 1:1000){
  
  samp1 = data.2[sample(1:314,50),]
  
  for(j in 1:10){
    lm.out = lm(fsiq7 ~poly(apexpos,j),samp1)
    mse.train[i,j] = mean((samp1$fsiq7-predict(lm.out))**2)
    mse.test[i,j] = mean((data.test$fsiq7-predict(lm.out,data.test))**2)
    #rsq.train[i,j] = cor(samp1$fsiq7,predict(lm.out))**2
    #rsq.test[i,j] = cor(data.test$fsiq7,predict(lm.out,data.test))**2
  }
  
}

library(DescTools)

mse.train = Winsorize(mse.train,probs=c(0.10,0.90))
mse.test = Winsorize(mse.test,probs=c(0.10,0.90)) # super crazy values

mean.train = colMeans(mse.train)
mean.test = colMeans(mse.test)

var.train = round(apply(mse.train,2,var),2)
var.test = round(apply(mse.test,2,var),2)




# MSE

mean.train2 = data.frame(power=1:8,mse = mean.train[1:8])
mean.test2 = data.frame(power=1:8,mse = mean.test[1:8])

means = data.frame(rbind(mean.train2,mean.test2))
means$Data = c(rep("Train",8),rep("Test",8))

p = ggplot() + 
  geom_line(data = means, aes(x = power, y = mse,col=Data)) +
 # geom_line(data = mean.test2, aes(x = power, y = rsq), color = "red") +
  xlab('Power') +
  ylab('Mean MSE') +
  theme_minimal() + scale_colour_grey(start = 0, end = 0.7)

print(p)


var.train2 = data.frame(power=1:8,mse = var.train[1:8])
var.test2 = data.frame(power=1:8,mse = var.test[1:8])
vars = data.frame(rbind(var.train2,var.test2))
vars$Data = c(rep("Train",8),rep("Test",8))

p = ggplot() + 
  geom_line(data = vars, aes(x = power, y = mse,col=Data)) +
  # geom_line(data = var.test2, aes(x = power, y = rsq), color = "red") +
  xlab('Power') +
  ylab('MSE Variance') +
  theme_minimal() + scale_colour_grey(start = 0, end = 0.7)

print(p)
