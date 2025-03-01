library(rpart.plot)
ecls.1 = read.table('C:/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');
ecls.1 = read.table('/Users/rjacobuc/Documents/GitHub/edm_book/ch5_trees/scripts/ecls_DM.dat', na='.');

names(ecls.1) = c('gender','kage',
                  'k_read_irt','k_read1','k_read2','k_read3','k_read4',	
                  'k_print','k_read_tht',
                  'k_math_irt','k_math1','k_math2','k_math3','k_math4',
                  'k_math_tht',
                  'k_gk_irt','k_gk_tht',
                  'f_mtr','g_mtr',
                  'P1LEARN','P1CONTRO','P1SOCIAL','P1SADLON','P1IMPULS',	
                  'ars_lit','ars_mth','ars_gk',
                  'T1LEARN','T1CONTRO','T1INTERP','T1EXTERN','T1INTERN',	
                  'height','weight','bmi',
                  'hisp','na_amer','asian','black','pac_isl','white','m_race',
                  'ses_c','ses_cat','poor','income',
                  'g8_read','g8_read_tht','g8_math','g8_math_tht',
                  'g8_sci','g8_sci_tht')

head(ecls.1)

myvars = c('k_math_irt','g8_sci','k_gk_irt','weight','height','income','ses_c','g8_read')

ecls.2 = ecls.1[ 1:500,myvars]
colnames(ecls.2) <- c("math","science","knowledge",'weight','height','income','ses','read')

plot(ecls.2[,c(1,3)])

library(caret)
library(randomForest)
# get bagging importance
bag.out <- randomForest(science ~ ., ecls.2,importance=T,mtry=7)
bag.out$importance
varImpPlot(bag.out)
xtable::xtable(importance(bag.out))


rf.out <- train(science ~ ., ecls.2,method="rf",tuneLength=1,ntree=100,importance=T)

library(xtable)
vi.rf = varImp(rf.out)
xtable(vi.rf$importance)

cf.out <- partykit::cforest(science ~ ., ecls.2,ntree=100)
vi.cf = partykit::varimp(cf.out,conditional=T)
xtable(as.data.frame(vi.cf))

vi.cf/max(vi.cf)*100

vi.cf2 = partykit::varimp(cf.out,conditional=F)
vi.cf2

# compare to rpart

rpart.out <- rpart(science ~ ., ecls.2)
str(rpart.out)

summary(rpart.out)


pred.y = predict(rf.out)
ecls.3 = data.frame(pred.y=pred.y,ecls.2[,-2])

glm.out = lm(pred.y ~ ., ecls.3)
summary(glm.out)


R# partial dependence plot
# https://bgreenwell.github.io/pdp/articles/pdp.html#constructing-pdps-in-r
library(vip)
library(pdp)
vip(rf.out)
p1 = partial(rf.out,"read",rug=T)
p2 = partial(rf.out,"math",rug=T)
plot(partial(rf.out,"math"),type="l",cex.axis=1.5,cex.lab=1.5)
plot(partial(rf.out,"read"),type="l",cex.axis=1.5,cex.lab=1.5)
pi3 = partial(rf.out,c("read","math"))
pi4 = partial(rf.out,c("read","knowledge"))

plotPartial(p1,plot.engine = "ggplot2",xlab="Read",rug=T,train=ecls.2)
plotPartial(p2,plot.engine = "ggplot2",xlab="Math",rug=T,train=ecls.2)
plotPartial(pi3,plot.engine = "ggplot2")
plotPartial(pi4,plot.engine = "ggplot2")
plotPartial(pi3,plot.engine = "ggplot2",levelplot=F,xlab="Read",ylab="Math")


plot(partial(cf.out,"read",train=ecls.2))
plot(partial(cf.out,"math",train=ecls.2))


#rm("ecls.2")
#save.image("ecls_rf.RData")



# accumulated local effects

library(ALEPlot)
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata, type="raw"))


ALEPlot(ecls.2,rf.out,pred.fun=yhat,J=c(1))
ALEPlot(ecls.2,rf.out,pred.fun=yhat,J=c(8))


# different type
library(DALEX)
# https://shirinsplayground.netlify.com/2018/07/explaining_ml_models_code_caret_iml/


explainer_rf <- DALEX::explain(rf.out, label = "rf",
                                       data = ecls.2)
pdp_rf  <- variable_response(explainer_rf, variable = "income", type = "pdp")
plot(pdp_rf)

ale_rf  <- variable_response(explainer_rf, variable = "income", type = "ale")
plot(ale_rf)



pdp_rf2  <- variable_response(explainer_rf, variable = "weight", type = "pdp")
plot(pdp_rf2)

ale_rf2  <- variable_response(explainer_rf, variable = "weight", type = "ale")
plot(ale_rf2)



# ICE plots
library(iml)
predictor <- Predictor$new(rf.out, data = ecls.2, y = ecls.2$science)
str(predictor)

pdp_obj <- Partial$new(predictor, feature = "read")
pdp_obj$center(min(ecls.2$science))

glimpse(pdp_obj$results)

tt = pdp_obj$plot() + theme_bw() #+ scale_color_brewer(palette="Accent")#scale_colour_manual(values="red")

# really weird results
pdp_obj2 <- Partial$new(predictor, feature = "knowledge")
pdp_obj2$center(min(ecls.2$science))

glimpse(pdp_obj2$results)

pdp_obj2$plot()




# look at interactions

interact <- Interaction$new(predictor, feature = "math")


library(gridExtra)
library(grid)
library(ggridges)
library(ggthemes)
theme_set(theme_minimal())

interact$results %>%
  ggplot(aes(x = reorder(.feature, .interaction), y = .interaction)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_tableau() +
  coord_flip() +
  guides(fill = FALSE)
