library(caret);library(AppliedPredictiveModeling)
set.seed(12345)
lat.var1 = rnorm(30000)
lat.var2 = rnorm(30000)


y = 0.5*lat.var1 + 0.5*lat.var2 + rnorm(30000,0,1)


summary(lm(y ~ lat.var1 + lat.var2))

dat1 = data.frame(y=y,x1=lat.var1,x2=lat.var2)

ret = matrix(NA,200,2)

for(i in 1:200){

samp1 = dat1[sample(1:30000,100),]
samp2 = dat1[sample(1:30000,100),]

lm1 = train(y ~., samp1,method="lm",trControl=trainControl(method="cv"))
ret[i,1] = lm1$results$Rsquared

lm2 = train(y ~., samp2,method="lm",trControl=trainControl(method="cv"))
ret[i,2] = lm2$results$Rsquared

}

psych::describe(ret)
rowM = apply(ret, 1, function(x) max(x, na.rm = TRUE))
mean(rowM)




# nested CV demo -- 200 is overkill for the outer loop. Done for demonstration purposes
ret2 = rep(NA,200)

for(i in 1:200){
  
  samp1 = dat1[sample(1:30000,100),]
  samp2 = dat1[sample(1:30000,100),]
  
  
  folds.samp1 = createFolds(samp1$y,k=5,list=F)
  folds.samp2 = createFolds(samp2$y,k=5,list=F)
  rsq = rep(NA,5)
  for(j in 1:5){
    lm1 = train(y ~., samp1[folds.samp1!=j,],method="lm",trControl=trainControl(method="cv"))
    
    
    lm2 = train(y ~., samp2[folds.samp2!=j,],method="lm",trControl=trainControl(method="cv"))
    
    if (lm1$results$Rsquared > lm2$results$Rsquared){
      rsq[j] = cor(samp1[folds.samp1==j,"y"],predict(lm1,samp1[folds.samp1==j,]))**2
    }else{
      rsq[j] = cor(samp2[folds.samp2==j,"y"],predict(lm2,samp2[folds.samp2==j,]))**2
    }
    
  }
  
  ret2[i] = mean(rsq)
  
}
summary(ret2)
