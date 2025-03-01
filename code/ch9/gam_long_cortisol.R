
cort <- read.table("C:/Users/rjacobuc/Documents/GitHub/edm_book/random/cortisol_sim.dat")
names(cort) <- c("y1","y2","y3","y4","y5","y6","y7","y8","y9")


cort.long <- reshape(cort, varying = c("y1","y2","y3","y4","y5","y6","y7","y8","y9"), 
                     v.names = "y",
                     times =1:9, direction = "long")

library(lattice)
xyplot(y ~ time, groups = id, data = cort.long, type = "o", col = "black", 
       xlab = "Time", ylab = "Cortisol[t]")

library(mgcv)

# s() by default uses thin plated splines
# set gamma=1.4 based on p.200 in Wood to prevent overfitting
# bs="ts" is thin plate, but can zero out
out <- mgcv::gam(y ~ s(time,k=9,bs="ts"), data=cort.long,gamma=1.4) # k sets max df
summary(out)
plot(out)


preds.gam <- predict(out)

pred.out1 <- rep(NA,9)
for(i in 1:9){
  pred.out1[i] <- preds.gam[i + (34*(i-1))]
}
plot(1:9,pred.out1,type="l")



library(caret)

train.out <- train(y ~ time,cort.long, method="earth",tuneLength=5)
train.out
train.out$finalModel
coef(train.out$finalModel)
plot(train.out$finalModel)

preds.earth <- predict(train.out$finalModel)

pred.out2 <- rep(NA,9)
for(i in 1:9){
  pred.out2[i] <- preds.earth[i + (34*(i-1))]
}
plot(1:9,pred.out2,type="l")


# cart

rpart.out <- rpart(y ~ time, cort.long)

pred.rpart <- predict(rpart.out)

pred.out3 <- rep(NA,9)
for(i in 1:9){
  pred.out3[i] <- mean(pred.rpart[(34*(i-1)+1):(34*(i))])
}
plot(1:9,pred.out3,type="l")


# random forests



train.rf <- train(y ~ time,cort.long, method="rf",tuneLength=5)


preds.rf <- predict(train.rf$finalModel)

pred.out4 <- rep(NA,9)
for(i in 1:9){
  pred.out4[i] <- mean(preds.rf[(34*(i-1)+1):(34*(i))])
}
plot(1:9,pred.out4,type="l")




# generalized additive mixed models


# would be best to do cross validation

library(mgcv);library(gamm4)
gamm.out <- gamm(y ~ s(time, bs = "cr", k =5),
                  # group = ~ id,
                  random=list(time=~1),
                  data=cort.long)
gamm.out

preds.gamm <- predict(gamm.out)$gam


pred.out5 <- rep(NA,9)
for(i in 1:9){
  pred.out5[i] <- mean(preds.gamm[(34*(i-1)+1):(34*(i))])
}
plot(1:9,pred.out5,type="l")
plot(gamm.out$gam)
