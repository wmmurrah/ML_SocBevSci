
sim_regDat <- function(N,P,nonzero.num=NULL,beta.nonzero=NULL){
  
  
  x=data.frame(matrix(rnorm(P*N),ncol=P))
  
  if(is.null(nonzero.num)==TRUE){
    beta <- rep(0,P)
  }else{
    beta <- c(beta.nonzero,rep(0,P-nonzero.num))
  }
  
  y <- as.matrix(x)%*%beta + rnorm(nrow(x),0,1)
  
  return(data.frame(y,x))
}



library(parallel)







niter <- 500
samps <- c(100,500,2000)
npreds <- c(3,10,50)
iters <- 1:500

grid <- expand.grid(iters=iters,samps=samps,npreds=npreds)





no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterExport(cl, c("sim_regDat","grid"))

library(earth)

par.fun <- function(vec){
  library(dtree)
  dat <- sim_regDat(vec["samps"],vec["npreds"])
  dat2 <- round(dat[,2:ncol(dat)],0)
  dat3 <- data.frame(y=dat$y,dat2)
  
  glmnet.out <- glmnet::cv.glmnet(as.matrix(dat2),as.matrix(dat$y))
  glmnet.errors <- sum(coef(glmnet.out,glmnet.out$lambda.1se)[-1,]!=0)
  
  #mars
  
  earth.out <- earth::earth(y ~ ., dat3,nfold=10,ncross=1,degree=2)
  
  earth.errors <- length(coef(earth.out))-1
  
  earth.out2 <- caret::train(y ~ ., dat3,method="earth",tuneLength=10,trControl=trainControl(method="repeatedcv",repeats=10))
  earth.errors2 <- length(coef(earth.out2$finalModel))-1
  
  
  out2 <- data.frame(glmnet.errors,earth.errors,earth.errors2,vec["samps"],vec["npreds"])
  colnames(out2) = c(c("glmnet","earth","train_earth"),"samps","npreds")
  rownames(out2) <- NULL
  out2
}

ret <- parRapply(cl,grid,par.fun)
save.image("null_earth_glmnet_sim.Rdata")


ret2 <- matrix(unlist(ret),9000,5,byrow=T)

summary(ret2[ret2[,4]==2000,])
summary(ret2[ret2[,4]==500,])
summary(ret2[ret2[,4]==100,])

summary(ret2[ret2[,5]==3,])
summary(ret2[ret2[,5]==10,])
summary(ret2[ret2[,5]==50,])
