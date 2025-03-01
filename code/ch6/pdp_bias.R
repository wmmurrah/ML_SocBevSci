
library(caret);library(gbm);library(randomForest);library(xgboost);library(kernlab)#library(party)
library(ggplot2)



# ---------- doesn't demonstrate difference
subsamp = 500

set.seed(1)
lat.var1 = rnorm(500)
lat.var2 = lat.var1 + rnorm(500,0,.5)

cor(lat.var1,lat.var2)
#y = cos(lat.var1) + sin(lat.var2) + tan(-.2*lat.var1*lat.var2) + rnorm(500,0,.1)
#plot(lat.var1,y)


#y2 = cos(lat.var1) + 0*sin(lat.var2) + tan(0*lat.var1*lat.var2) + rnorm(500,0,.1)
#plot(lat.var1,y2)


y = lat.var1 + lat.var2 + 0.3*(lat.var1*lat.var2) + rnorm(500,0,.1)
plot(lat.var2,y)

cor(lat.var1,lat.var2)


#y2 = 0*lat.var1 + 0*lat.var1**2 + lat.var2 - 0*lat.var1*lat.var2 + rnorm(500,0,.1)
#plot(lat.var2,y2)

dat.comb = data.frame(y=y,lat.var1=lat.var1,lat.var2=lat.var2)



rf.out <- train(y ~ ., dat.comb,method="rf",tuneLength=1,ntree=100,importance=T)


library(DALEX)
# https://shirinsplayground.netlify.com/2018/07/explaining_ml_models_code_caret_iml/


explainer_rf <- DALEX::explain(rf.out, label = "rf",
                               data = dat.comb)
pdp_rf  <- variable_response(explainer_rf, variable = "lat.var1", type = "pdp")
plot(pdp_rf)

ale_rf  <- variable_response(explainer_rf, variable = "lat.var1", type = "ale")
plot(ale_rf)



# try number 2 ------- does demonstrate difference

set.seed(1)
lat.var1 = rnorm(500)
lat.var2 = lat.var1 + rnorm(500,0,.5)

cor(lat.var1,lat.var2)
#y = cos(lat.var1) + sin(lat.var2) + tan(-.2*lat.var1*lat.var2) + rnorm(500,0,.1)
#plot(lat.var1,y)


#y2 = cos(lat.var1) + 0*sin(lat.var2) + tan(0*lat.var1*lat.var2) + rnorm(500,0,.1)
#plot(lat.var1,y2)


y = lat.var1 + sin(lat.var2) + rnorm(500,0,.1)
plot(lat.var2,y)

cor(lat.var1,lat.var2)


#y2 = 0*lat.var1 + 0*lat.var1**2 + lat.var2 - 0*lat.var1*lat.var2 + rnorm(500,0,.1)
#plot(lat.var2,y2)

dat.comb = data.frame(Y=y,X1=lat.var1,X2=lat.var2)



rf.out <- train(Y ~ ., dat.comb,method="rf",tuneLength=1,ntree=100,importance=T)


library(iml)

X =  dat.comb$X1
model = Predictor$new(rf.out, data = dat.comb, y = dat.comb$Y)
effect = FeatureEffects$new(model)
effect$plot(features = c("X1"),ncols=1)

effect2 = FeatureEffects$new(model,method="pdp")
effect2$plot(features = c("X1"),ncols=1)




# try heterogeneous effects




set.seed(1)
lat.var1 = rnorm(500)
lat.var2 = lat.var1 + rnorm(500)
y = rep(NA,500)

y[1:250] = 0.8*lat.var1[1:250]+ 0.4*lat.var2[1:250]  + rnorm(250,0,.1)
plot(lat.var2,y)

y[251:500] = 0.2*lat.var1[251:500] + 0.1*lat.var2[251:500]  + rnorm(250,0,.8)
plot(y[251:500],lat.var1[251:500])


dat.comb = data.frame(y=y,lat.var1=lat.var1,lat.var2=lat.var2)
plot(y, lat.var1)


gbmGrid <-  expand.grid(interaction.depth = c(1, 2,3), 
                        n.trees = c(100,1000), 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

rf.out <- train(y ~ ., dat.comb,method="earth")
rf.out

# ICE plot


library(iml)
predictor <- Predictor$new(rf.out, data = dat.comb, y = dat.comb$y)
str(predictor)

pdp_obj <- Partial$new(predictor, feature = "lat.var1")



pdp_obj$plot()


# second example




set.seed(1)
lat.var1 = rnorm(500)
lat.var2 = 0.5*lat.var1 + rnorm(500,0,.5)
y = rep(NA,500)

y = 0.8*lat.var1 + 0.8*lat.var2 - 0.3*lat.var1*lat.var2 + rnorm(500,0,.1)


dat.comb = data.frame(y=y,lat.var1=lat.var1,lat.var2=lat.var2)
plot(y, lat.var1)


rf.out <- train(y ~ ., dat.comb,method="gbm")


# ICE plot


library(iml)
predictor <- Predictor$new(rf.out, data = dat.comb, y = dat.comb$y)
str(predictor)

pdp_obj <- Partial$new(predictor, feature = "lat.var1")



pdp_obj$plot()

