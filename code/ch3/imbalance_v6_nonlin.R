#install.packages( "DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
rm(list=ls())
whichRep <- as.numeric(Sys.getenv("SGE_TASK_ID"))
nSlots <- as.numeric(Sys.getenv("NSLOTS"))
if(is.na(whichRep)) whichRep <- 1


sim.fun <- function(index){
  
  
  library(forcats);library(PRROC)
  library(caret)
  P=grid[index,"P"]
  N=grid[index,"N"]
  b0=grid[index,"b0"]
  var_error = grid[index,"var_error"]
  before = grid[index,"before"]
  
  resamp= ifelse(grid[index,"method"] == 1,"cv","boot")
  alg= ifelse(grid[index,"alg"] == 1,"bayesglm","rf")
  
  
  
  pop_N = 50000
  
  e=rnorm(pop_N,0,sqrt(var_error))
  
  beta = t(as.matrix(c(b0,rep(.2,P/10),rep(.1,P/10),rep(.05,P/10),rep(0,P - (P/10)*3))))
  
  X <- t(cbind(rep(1,pop_N),matrix(rnorm(P*pop_N),pop_N,P)))
  
  #var_model = var(t(beta %*% X))
  x_nonlin1 = rnorm(pop_N)
  x_nonlin2 = rnorm(pop_N)
  
  y = t(beta %*% X) + scale(cos(x_nonlin1),scale=F) + scale(sin(x_nonlin2),scale=F) + scale(tan(0.1*x_nonlin1*x_nonlin2),scale=F) + e
  #var_y = var(y)
  
  pi=exp(y)/(1+exp(y))
  y=rbinom(pop_N,1,prob = pi)
  
  
  #exp_var = var_model/var_y
  
  y_cat = as.factor(y)
  levels(y_cat) <- c("neg","pos")
  y_cat = fct_rev(y_cat)
  
  
  # get simulated values
  fg1 = pi[y_cat=="pos"]
  bg1 = pi[y_cat=="neg"]
  roc1<-PRROC::roc.curve(scores.class0 = fg1, scores.class1 = bg1,curve=T)
  sim_auc = roc1$auc
  pr1<-pr.curve(scores.class0 = fg1, scores.class1 = bg1,curve=T)
  sim_auprc = pr1$auc.integral
  
  
  
  data <- data.frame(t(X)[,-1],x_nonlin1,x_nonlin2)
  colnames(data) <- c(paste("x",1:P,sep=""),"x_nonlin1","x_nonlin2")
  data$y = y_cat
  
  data_train = data[1:N,]
  data_test = data[(N+1):pop_N,]
  
  
  
  trainIndex <- createDataPartition(data_train$y, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  
  data_train_train = data_train[trainIndex,]
  data_test_test = data_train[-trainIndex,]
  
  
  #lm.out = lm(y ~ ., data)
  #summary(lm.out)
  #glm.out = glm(y~.,data,family="binomial")
  #summary(glm.out)
  #rcompanion::nagelkerke(glm.out)
  
  
  
  #mean(y)
  #y_cat = as.factor(y)
  #levels(y_cat) <- c("neg","pos")
  #y_cat = fct_rev(y_cat)
  
  #pROC::auc(y,plogis(predict(glm.out)))
  
  classes=c("pos","neg")
  MySummary  <- function(data, lev = classes, model = NULL){
    a1 <- defaultSummary(data, lev, model)
    b1 <- twoClassSummary(data, lev, model)
    c1 <- prSummary(data, lev, model)
    out <- c(a1, b1, c1)
    out}
  
  
  
  
  if(before == 1){
    
    train.out1 = try(caret::train(y ~ ., data_train_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    probabilities2 = predict(train.out1,data_test_test,type="prob")[,"pos"]
    fg2 = probabilities2[data_test_test$y=="pos"]
    bg2 = probabilities2[data_test_test$y=="neg"]
    roc2<-PRROC::roc.curve(scores.class0 = fg2, scores.class1 = bg2,curve=F)
    none_auc = roc2$auc
    pr2<-pr.curve(scores.class0 = fg2, scores.class1 = bg2,curve=F)
    none_auprc = pr2$auc.integral
    
  }else{
    train.out1 = try(caret::train(y ~ ., data_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    if(inherits(train.out1, "try-error")){
      none_auc = NA
      none_auprc=NA
    }else{
      none_auc=max(train.out1$results[,c("ROC")])
      none_auprc=max(train.out1$results[,c("AUC")])
    }
  }
  
  
  
  if(before == 1){
    
    down_train <- downSample(x = data_train_train[, -ncol(data_train_train)],
                             y = data_train_train$y)
    
    train.out2 = try(caret::train(Class ~ ., down_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    probabilities3 = predict(train.out2,data_test_test,type="prob")[,"pos"]
    fg3 = probabilities3[data_test_test$y=="pos"]
    bg3 = probabilities3[data_test_test$y=="neg"]
    roc3<-PRROC::roc.curve(scores.class0 = fg3, scores.class1 = bg3,curve=F)
    down_auc = roc3$auc
    pr3<-pr.curve(scores.class0 = fg3, scores.class1 = bg3,curve=F)
    down_auprc = pr3$auc.integral
    
  }else{
    train.out2 = try(caret::train(y ~ ., data_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,sampling="down",allowParallel=F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    if(inherits(train.out2, "try-error")){
      down_auc = NA
      down_auprc=NA
    }else{
      down_auc=max(train.out2$results[,c("ROC")])
      down_auprc=max(train.out2$results[,c("AUC")])
    }
  }
  
  
  if(before == 1){
    
    up_train <- upSample(x = data_train_train[, -ncol(data_train_train)],
                         y = data_train_train$y)
    
    train.out3 = try(caret::train(Class ~ ., up_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    probabilities4 = predict(train.out3,data_test_test,type="prob")[,"pos"]
    fg4 = probabilities4[data_test_test$y=="pos"]
    bg4 = probabilities4[data_test_test$y=="neg"]
    roc4<-PRROC::roc.curve(scores.class0 = fg4, scores.class1 = bg4,curve=F)
    up_auc = roc4$auc
    pr4<-pr.curve(scores.class0 = fg4, scores.class1 = bg4,curve=F)
    up_auprc = pr4$auc.integral
    
  }else{
    
    train.out3 = try(caret::train(y ~ ., data_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,sampling="up",allowParallel=F,
                                                         summaryFunction=MySummary,classProbs=T)))
    if(inherits(train.out3, "try-error")){
      up_auc = NA
      up_auprc=NA
    }else{
      up_auc=max(train.out3$results[,c("ROC")])
      up_auprc=max(train.out3$results[,c("AUC")])
    }
  }
  
  if(before == 1){
    library(DMwR)
    smote_train <- SMOTE(y ~ ., data_train_train) 
    
    train.out4 = try(caret::train(y ~ ., smote_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    probabilities5 = predict(train.out4,data_test_test,type="prob")[,"pos"]
    fg5 = probabilities5[data_test_test$y=="pos"]
    bg5 = probabilities5[data_test_test$y=="neg"]
    roc5<-PRROC::roc.curve(scores.class0 = fg5, scores.class1 = bg5,curve=F)
    smote_auc = roc5$auc
    pr5<-pr.curve(scores.class0 = fg5, scores.class1 = bg5,curve=F)
    smote_auprc = pr5$auc.integral
    
  }else{
    
    
    train.out4 = try(caret::train(y ~ ., data_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,sampling="smote",allowParallel=F,
                                                         summaryFunction=MySummary,classProbs=T)))
    if(inherits(train.out4, "try-error")){
      smote_auc = NA
      smote_auprc=NA
    }else{
      smote_auc=max(train.out4$results[,c("ROC")])
      smote_auprc=max(train.out4$results[,c("AUC")])
    }
  }
  
  
  if(before == 1){
    library(ROSE)
    rose_train <- ROSE(y ~ ., data_train_train)$data
    
    train.out5 = try(caret::train(y ~ ., rose_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,allowParallel = F,
                                                         summaryFunction=MySummary,classProbs=T)))
    
    probabilities6 = predict(train.out5,data_test_test,type="prob")[,"pos"]
    fg6 = probabilities6[data_test_test$y=="pos"]
    bg6 = probabilities6[data_test_test$y=="neg"]
    roc6<-PRROC::roc.curve(scores.class0 = fg6, scores.class1 = bg6,curve=F)
    rose_auc = roc6$auc
    pr6<-pr.curve(scores.class0 = fg6, scores.class1 = bg6,curve=F)
    rose_auprc = pr6$auc.integral
    
  }else{
    
    
    
    train.out5 = try(caret::train(y ~ ., data_train,method=alg,metric="AUC",tuneLength=1,
                                  trControl=trainControl(method=resamp,sampling="rose",allowParallel=F,
                                                         summaryFunction=MySummary,classProbs=T)))
    if(inherits(train.out5, "try-error")){
      rose_auc = NA
      rose_auprc=NA
    }else{
      rose_auc=max(train.out5$results[,c("ROC")])
      rose_auprc=max(train.out5$results[,c("AUC")])
    }
  }
  
  
  
  
  trainIndex2 <- createDataPartition(data_test$y, p = .5, 
                                     list = FALSE, 
                                     times = 1)
  
  data_holdout1 = data_test[trainIndex2,]
  data_holdout2 = data_test[-trainIndex2,]
  
  train.out11 = try(caret::train(y ~ ., data_holdout1,method=alg,metric="AUC",tuneLength=1,
                                 trControl=trainControl(method=resamp,allowParallel = F,
                                                        summaryFunction=MySummary,classProbs=T)))
  
  probabilities7 = predict(train.out11,data_holdout2,type="prob")[,"pos"]
  
  fg7 = probabilities7[data_holdout2$y=="pos"]
  
  bg7 = probabilities7[data_holdout2$y=="neg"]
  
  
  roc7<-PRROC::roc.curve(scores.class0 = fg7, scores.class1 = bg7,curve=T)
  test_auc = roc7$auc
  pr7<-pr.curve(scores.class0 = fg7, scores.class1 = bg7,curve=T)
  test_auprc = pr7$auc.integral
  
  
  
  res = c(grid[index,],dist=mean(y),#exp_var=exp_var,
          none_auc=none_auc,none_auprc=none_auprc,
          down_auc=down_auc,down_auprc=down_auprc,
          up_auc=up_auc,up_auprc=up_auprc,
          smote_auc=smote_auc,smote_auprc=smote_auprc,
          rose_auc=rose_auc,rose_auprc=rose_auprc,
          test_auc=test_auc,test_auprc=test_auprc,
          sim_auc=sim_auc,sim_auprc=sim_auprc)
  
}

Ps = c(30,70)
Ns = c(300,1000,10000)
b0s = c(-4, -3,-2,-1,0)
var_errors = c(.82,.3)
method=1#c(1,2)
alg = c(1,2)
before=c(1,2) # oversample before or after


grid = as.matrix(expand.grid(whichRep,P=Ps,N=Ns,b0=b0s,var_error=var_errors,method=method,alg=alg,before=before))

library(foreach)
#library(parallelly)
#cl <- makeCluster(6) #makeCluster(nSlots)
#cl <- parallelly::autoStopCluster(cl)
#clusterExport(cl, c("grid"))

#ret <- parLapply(cl,grid,sim.fun) # have to use parLapply if not passing grid, just vector
library(parallel)
cores = 2#nSlots-4

library(doParallel)
registerDoParallel(cores=cores)
getDoParWorkers()

ret =  foreach (i = 1:nrow(grid), .errorhandling = 'pass') %dopar% {
  sim.fun(i)
}

saveRDS(ret,paste0("res6/imbalance_res6_",whichRep,".rds"))
