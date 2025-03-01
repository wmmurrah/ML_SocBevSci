library(psych);library(rpart)

data(bfi)
head(bfi)

dat2 <- bfi[1:1000,c(6:20,26)]
dat2$gender <- as.factor(dat2$gender)
levels(dat2$gender) <- c("male","female")

tree.out2 <- rpart(gender ~ ., dat2,control=rpart.control(minsplit=3))
plot(tree.out2);text(tree.out2)

printcp(tree.out2)

tree0 <- prune(tree.out2,cp=0.016)
rpart.plot(tree0,box.palette="white")


tree.out22 <- rpart(gender ~ ., dat2,control=rpart.control(minbucket=1))
printcp(tree.out22)
rpart.plot(tree.out22,box.palette="white")
#plot(tree3);text(tree3)
plot(prune(tree.out22,depth=2))
library(rpart.plot)
rpart.plot(tree3,box.palette="white")

attach(dat2)


