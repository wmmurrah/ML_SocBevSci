library(rpart);library(lavaan);library(party);library(evtree);library(rpart.plot)
HS <- HolzingerSwineford1939[,c(6:12)]
HS1 <- HS[1:100,]
HS2 <- HS[101:200,]
out <- rpart(grade ~ .,data=HS1,control=rpart.control(maxdepth=2))
rpart.plot(out,box.palette="white")

out2 <- rpart(grade ~ .,data=HS2,control=rpart.control(maxdepth=2))
rpart.plot(out2,box.palette="white")

out3 <- rpart(grade ~ .,data=HS,control=rpart.control(maxdepth=2))
rpart.plot(out3,box.palette="white")


lm.out1 <- lm(grade ~ ., data=HS1)
summary(lm.out1)

lm.out2 <- lm(grade ~ ., data=HS2)
summary(lm.out2)

lm.out3 <- lm(grade ~ ., data=HS)
summary(lm.out3)

library(MASS)
step1 <- stepAIC(lm.out1,direction="backward")
step1

step2 <- stepAIC(lm.out2,direction="backward")
step2

step3 <- stepAIC(lm.out3,direction="backward")
step3
