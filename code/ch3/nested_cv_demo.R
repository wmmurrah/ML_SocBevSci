library(caret);library(AppliedPredictiveModeling)
set.seed(12345)
lat.var1 = rnorm(1000)
lat.var2 = rnorm(1000)


y = 0.5*lat.var1 + 0.5*lat.var2 + rnorm(1000,0,1)


summary(lm(y ~ lat.var1 + lat.var2))

dat1 = data.frame(y=y,x1=lat.var1,x2=lat.var2)




  folds.samp1 = createFolds(dat1$y,k=5,list=F)
  rsq = rep(NA,5)
  method = rep(NA,5)
  for(j in 1:5){
    lm1 = train(y ~., dat1[folds.samp1!=j,],method="lm",trControl=trainControl(method="cv"))
    
    
    rf1 = train(y ~., dat1[folds.samp1!=j,],method="earth",trControl=trainControl(method="cv"))
    
    if (lm1$results$Rsquared > max(rf1$results$Rsquared)){
      rsq[j] = cor(dat1[folds.samp1==j,"y"],predict(lm1,dat1[folds.samp1==j,]))**2
      method[j] = "lm"
    }else{
      rsq[j] = cor(dat1[folds.samp1==j,"y"],predict(rf1,dat1[folds.samp1==j,]))**2
      method[j] = "rf"
    }
    
  }
  

