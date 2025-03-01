rm(list=ls())
whichRep <- as.numeric(Sys.getenv("SGE_TASK_ID"))
nSlots <- as.numeric(Sys.getenv("NSLOTS"))
if(is.na(whichRep)) whichRep <- 1
library(parallel)




# eventually add in keras
# don't include adaboost because only for classification
# can't get party or partykit on CRC



sim.fun <- function(index){

  
  
mat = rep(NA,10)
library(caret);library(gbm);library(randomForest);library(xgboost);#library(party)



N = grid[index,"samps"]
rel = grid[index,"reliabilities"]


mat[1] = N
mat[2] = rel


lat.var1 = rnorm(N)
lat.var2 = rnorm(N)
y = cos(lat.var1) + sin(lat.var2) + tan(0.1*lat.var1*lat.var2) + rnorm(N,0,.1)
#plot(lat.var1,y)
#plot(lat.var2,y)

dat.comb = data.frame(y=y,lat.var1=lat.var1,lat.var2=lat.var2)


gbm.out = train(y ~ lat.var1 + lat.var2,dat.comb,method="gbm")
mat[3] = as.numeric(gbm.out$results["Rsquared"])

lm.out = train(y ~ lat.var1 + lat.var2,dat.comb,method="lm")
mat[4] = as.numeric(lm.out$results["Rsquared"])

rf.out = train(y ~ lat.var1 + lat.var2,dat.comb,method="rf")
mat[5] = as.numeric(rf.out$results["Rsquared"])

#cf.out = train(y ~ lat.var1 + lat.var2,dat.comb,method="cforest")
#mat[6] = as.numeric(cf.out$results["Rsquared"])

xgb.out = train(y ~ lat.var1 + lat.var2,dat.comb,method="xgbLinear")
mat[6] = as.numeric(xgb.out$results["Rsquared"])

# create unreliable indicators

# reliability

x11 = rel*lat.var1 + rnorm(N,0,sqrt(1-rel**2))
x12 = rel*lat.var1 + rnorm(N,0,sqrt(1-rel**2))
x13 = rel*lat.var1 + rnorm(N,0,sqrt(1-rel**2))
x14 = rel*lat.var1 + rnorm(N,0,sqrt(1-rel**2))

x21 = rel*lat.var2 + rnorm(N,0,sqrt(1-rel**2))
x22 = rel*lat.var2 + rnorm(N,0,sqrt(1-rel**2))
x23 = rel*lat.var2 + rnorm(N,0,sqrt(1-rel**2))
x24 = rel*lat.var2 + rnorm(N,0,sqrt(1-rel**2))


dat.comb2 = data.frame(y,x11,x12,x13,x14,x21,x22,x23,x24)


gbm.out.rel = train(y ~ x11+x12+x13+x14+x21+x22+x23+x24,dat.comb2,method="gbm")
mat[7] = as.numeric(gbm.out.rel$results["Rsquared"])

lm.out.rel2 = train(y ~ x11+x12+x13+x14+x21+x22+x23+x24,dat.comb2,method="lm")
mat[8] = as.numeric(lm.out.rel2$results["Rsquared"])

rf.out.rel2 = train(y ~ x11+x12+x13+x14+x21+x22+x23+x24,dat.comb2,method="rf")
mat[9] = as.numeric(rf.out.rel2$results["Rsquared"])

#cf.out.rel2 = train(y ~ x11+x12+x13+x14+x21+x22+x23+x24,dat.comb2,method="cforest")
#mat[10] = as.numeric(cf.out.rel2$results["Rsquared"])

xgb.out.rel2 = train(y ~ x11+x12+x13+x14+x21+x22+x23+x24,dat.comb2,method="xgbLinear")
mat[10] = as.numeric(xgb.out.rel2$results["Rsquared"])


return(mat)
}

samps = c(200,500,2000)
reliabilities = c(.3,.6,.9)


grid = expand.grid(samps=samps,reliabilities=reliabilities)



no_cores <- detectCores()
cl <- makeCluster(no_cores)
clusterExport(cl, c("grid"))

ret <- parLapply(cl,1:nrow(grid),sim.fun) # have to use parLapply if not passing grid, just vector

setwd("/afs/crc.nd.edu/user/r/rjacobuc/ensemble_noise/v1_res")

saveRDS(ret,file = paste0("ret_run1_",whichRep,".rds"))