data.1 = read.table('/Users/Ross/Google Drive/edm_book/ch1_intro/scripts/apexpos.dat')
data.1 = read.table("C:/Users/rjacobuc/Documents/GitHub/edm_book/ch6_ensembles/scripts/apexpos.dat")
names(data.1) = c('id', 'apexpos', 'fsiq7')
head(data.1)

# Sort Data
data.2 <- data.1[order(data.1$apexpos),]

attach(data.2)

## Plotting empirical data ##

plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')



library(rpart);library(rpart.plot)
rpart.out <- rpart(fsiq7 ~ apexpos, data.2,control=rpart.control(cp=.001))
printcp(rpart.out)

tree1 <- prune(rpart.out,0.0423339)
tree2 <- prune(rpart.out,0.0295617)
tree3 <- prune(rpart.out,0.0038974)

rpart.plot(tree1)
rpart.plot(tree2)
rpart.plot(tree3)

attach(data.2)
plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')
lines(apexpos,predict(tree1),lwd=3,lty=1)
lines(apexpos,predict(tree2),lwd=3,lty=2)
lines(apexpos,predict(tree3),lwd=3,lty=3)

legend(18, 115, legend=c("Depth = 1", "Depth = 2","Depth = 3"),
       lty=1:3, cex=1,lwd=3)

detach(data.2)

# bagging
#library(randomForest)
#rf.out1 = randomForest(fsiq7 ~ apexpos,data=data.2,ntree=1,mtry=1)
#predict(rf.out1)

library(ipred)
bag.out1 = bagging(fsiq7 ~ apexpos,data=data.2,nbagg=1,method="standard")

plot(apexpos, fsiq7, ylab = 'Full Scale IQ', xlab = 'PHE Exposure')
lines(apexpos,predict(bag.out1,data.2$fsiq7),lwd=3,lty=1)
