## Read Data into R

data.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch3_principles/scripts/apexpos.dat')
apexpos_test = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch3_principles/scripts/apexpos_test.dat')
data.1 = read.table('/Users/Ross/Google Drive/edm_book/ch1_intro/scripts/apexpos.dat')
data.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch3_practices/scripts/apexpos.dat')
apexpos_test = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch3_practices/scripts/apexpos_test.dat')
names(data.1) = c('id', 'apexpos', 'fsiq7')
names(apexpos_test) = c('id', 'apexpos', 'fsiq7')
head(data.1)

# Sort Data
data.2 <- data.1[order(data.1$apexpos),]
data.2 <- data.1[order(data.1$apexpos),]

attach(data.2)

## Plotting empirical data ##

plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')

## linear regression model ##

lm.1 = lm(fsiq7 ~ poly(apexpos,20))

summary(lm.1)
lines(apexpos,predict(lm.1))

#plot(apexpos, fitted(lm.1), ylab = 'Predicted Full Scale IQ', xlab = 'PHE Exposure')

#plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')
#abline(lm(fsiq7 ~ apexpos), col="red")

library(ggplot2)

plot.1 = ggplot(data.1, aes(x=apexpos, y=fsiq7)) + geom_point() +
              stat_smooth(method='lm', formula = y ~ x, size = 1) +
              xlab('PHE Exposure') + ylab('Age 7 Full Scale IQ')
print(plot.1)


## Regression Diagnostics on Linear Regression Model ##

library(car)

#Outlier Test
outlierTest(lm.1) # Bonferonni p-value for most extreme obs

#QQ Plot
qqPlot(lm.1, main="QQ Plot") #qq plot for studentized resid

# Cook?s D
cook = cooks.distance(lm.1)
plot(cook,ylab="Cooks distances")

# Plots
plot(apexpos, lm.1$res)
plot(lm.1$fitted, lm.1$res)

# Leverage Plot
leverage = hat(model.matrix(lm.1))
plot(leverage)


# quadratic regression model
apexpos2 = apexpos^2

lm.2 = lm(fsiq7 ~ apexpos + apexpos2)

summary(lm.2)

#plot(apexpos, fitted(lm.2))

#plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')

#Save model estimates for plotting curve
#b0 = lm.2$coefficients[1]
#b1 = lm.2$coefficients[2]
#b2 = lm.2$coefficients[3]

#curve(b0 + b1*x + b2*x^2, from = min(apexpos), to = max(apexpos), n = 100, col = "red", lwd = 2, ann = F, add = T)



plot.2 = ggplot(data.1, aes(x=apexpos, y=fsiq7)) + geom_point() +
              stat_smooth(method='lm', formula = y ~ x + I(x^2), size = 1) +
              xlab('PHE Exposure') + ylab('Age 7 Full Scale IQ')
print(plot.2)




# let's keep increasing the order

order = 20


results <- matrix(NA,3,20)
mse_temp = matrix(NA,100,20)


for (i in 1:order) {
  prediction_matrix <- matrix(NA,nrow(apexpos_test),100)
  for(j in 1:100){
    ids = sample(1:nrow(data.2),nrow(data.2),replace=T)
    lm.poly <- lm(fsiq7 ~ poly(apexpos,i),data.2[ids,])
    prediction_matrix[,j] = predict(lm.poly,apexpos_test)
    mse_temp[j, i] <- mean((predict(lm.poly,apexpos_test) - apexpos_test$fsiq7)^2)
  }
  var_matrix <- apply(prediction_matrix, 1, FUN = var)
  bias_matrix <- apply(prediction_matrix, 1, FUN = mean)
  
  squared_bias <- (bias_matrix - apexpos_test$fsiq7)^2
  
  results[1, i] <- mean(var_matrix)
  results[2, i] <- mean(squared_bias)

}

results[3,1:20] <- apply(mse_temp, 2, FUN = mean)



plot(r.square, ylab = 'R^2', xlab = 'Order of the Polynomial', ylim=c(0.5,1), xlim=c(1,20), type='l')
plot(adj.r.square, ylab = 'Adjusted R^2', xlab = 'Order of the Polynomial', ylim=c(0.5,1), xlim=c(1,20), type='l')

variance2 = scale(variance)
bias2 = scale(bias)
plot(variance2, ylab = 'Adjusted R^2', xlab = 'Order of the Polynomial',ylim=c(-3,3), xlim=c(1,20), type='l')
lines(bias2)

# https://www.statworx.com/at/blog/simulating-the-bias-variance-tradeoff-in-r/
dd = data.frame(cbind(1:20,t(results)))
colnames(dd) <- c("Order","Variance","Bias","MSE")

library(ggplot2)
p = ggplot() + 
  geom_line(data = dd, aes(x = Order, y = Variance,colour="Variance")) +
  geom_line(data = dd, aes(x = Order, y = Bias,colour="Bias")) +
  geom_line(data = dd, aes(x = Order, y = MSE,colour="MSE")) +
  xlab('Polynomial Order') +
  ylab('Value') + 
  ggtitle("APE Exposure Bias-Variance Tradeoff") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_manual(name="Line Color",
      values=c(Variance="red", Bias="blue", MSE="green"))

print(p)


# K-fold Cross-Validation

#Randomly shuffle the data

data.2<-data.1[sample(nrow(data.1)),]

K = 10

#Create 10 equally size folds

folds <- cut(seq(1,nrow(data.2)),breaks=K,labels=FALSE)

r.square = matrix(NA,K,order)

#Perform 10-fold cross validation
for(i in 1:K){
    #Segement your data by fold using the which() function
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data.2[testIndexes, ]
    trainData <- data.2[-testIndexes, ]
    #Use the test and train data partitions

    for (j in 1:order){
		fit.train = lm(fsiq7 ~ poly(apexpos,j), data = trainData)
            fit.test = predict(fit.train, newdata=testData)
		r.square[i,j] = cor(fit.test, testData$fsiq7, use='complete')^2
	}
}

colMeans(r.square)
plot(colMeans(r.square), type='l')




# 10-fold CV leads to a fifth degree polynomial

myplot = ggplot(data.1, aes(x=apexpos, y=fsiq7)) + geom_point() +
         stat_smooth(method='lm', formula = y ~ poly(x,5), size = 1) +
         xlab('PHE Exposure') + ylab('Age 7 Full Scale IQ')

print(myplot)

lm.final = lm(fsiq7~poly(apexpos,5), data=data.1)

## Bring in Official Test Data ##
data.test = read.table('/Users/Ross/Google Drive/edm_book/ch1_intro/scripts/apexpos_test.dat')
names(data.test) = c('id', 'apexpos', 'fsiq7')

cor(data.test$fsiq7, predict(lm.final, newdata=data.test))^2

