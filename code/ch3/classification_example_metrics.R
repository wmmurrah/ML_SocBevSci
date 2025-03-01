library(psych)
library(caret)
library(psychTools)

data(epi.bfi)
eb2 <- epi.bfi[,1:11]
eb2$bdi_cat = as.numeric(eb2$bdi > 9) # cut score of 9 from wikipedia
eb2$bdi_cat_fac = as.factor(as.numeric(eb2$bdi > 9)) # cut score of 9 from wikipedia
levels(eb2$bdi_cat_fac) <- c("no","dep")

#eb2$bdi_cat = forcats::fct_rev(eb2$bdi_cat)

out = glm(bdi_cat ~ bfagree+bfcon+bfext+bfneur+bfopen, eb2,family=stats::binomial)
summary(out)


probabilities = predict(out,type="response")



hist(probabilities,xlab="Probability",main="")
df = data.frame(Probability=probabilities)
ggplot(df, aes(x=Probability)) + geom_histogram(color="black", fill="white",bins=15) + theme_classic() + xlim(0,1)

dat = data.frame(Probability=probabilities,Class=eb2$bdi_cat_fac)

ggplot(dat, aes(Probability, col=Class, fill=Class)) + 
  geom_density(alpha=.5) + xlim(0,1) + theme_classic() + scale_fill_grey() + scale_color_grey()


class.preds = as.factor(as.numeric(predict(out,type="response") > .5))
levels(class.preds) <- c("no","dep")


# calibration

# add random forests
# can reduce to tuneLength=1 to quicken
out.rf = train(as.factor(bdi_cat) ~ bfagree+bfcon+bfext+bfneur+bfopen, eb2,method="rf",tuneLength=3)
out.rf

prob.rf = predict(out.rf,type="prob")[,2]

cal = calibration(eb2$bdi_cat_fac ~ probabilities + prob.rf,class="dep")
plot(cal,col=c("black","grey"))

xyplot(cal,key=simpleKey(c("Logistic Regression","Random Forests")),points=c("black","grey")) 



# other metrics



confusionMatrix(class.preds,eb2$bdi_cat_fac,positive="dep")

library(epiR)

epi.kappa(table(eb2$bdi_cat_fac,class.preds)) # pabak kappa


# optimal cutoff

library(ROCR)
pred <- prediction(probabilities,eb2$bdi_cat_fac)
roc.perf = performance(pred, "f")
plot(roc.perf)

roc.perf2 = performance(pred, "acc")
plot(roc.perf2)

roc.perf3 = performance(pred, "lift")
plot(roc.perf3)


id = which(roc.perf@y.values[[1]]==max(roc.perf@y.values[[1]],na.rm=T))
cut = roc.perf@x.values[[1]][id]

library(cutpointr)


cp <- cutpointr(x=probabilities,class=eb2$bdi_cat_fac,
                method = maximize_metric, metric = sum_sens_spec)
plot_roc(cp)


# just using probabilities


library(pROC)
roc2 = pROC::roc(eb2$bdi_cat,probabilities)
auc2 = pROC::auc(eb2$bdi_cat,probabilities)
plot(roc2)


# show how different cutoffs correspond to ROC

# 0.5
confusionMatrix(class.preds,eb2$bdi_cat_fac,positive="dep")$byClass[1:2]
# 0.2
class.preds.2 = as.factor(as.numeric(1- predict(out,type="response") > .2))
levels(class.preds.2) <- c("no","dep")
confusionMatrix(class.preds.2,eb2$bdi_cat_fac,positive="dep")$byClass[1:2]
# 0.8
class.preds.8 = as.factor(as.numeric(1- predict(out,type="response") > .8))
levels(class.preds.8) <- c("no","dep")
confusionMatrix(class.preds.8,eb2$bdi_cat_fac,positive="dep")$byClass[1:2]

roc2.rf = roc(eb2$bdi_cat,prob.rf)
lines(roc2.rf,col=2)
legend(0.2,.5, legend=c("Logistic", "RF"),
  col=c("black", "red"),lty=1,  cex=1)


fg = probabilities[eb2$bdi_cat==1]
bg = probabilities[eb2$bdi_cat==0]

library(PRROC)
roc3<-roc.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(roc3)
pr3<-pr.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(pr3,color=F)




# different model to get clearer cutoff decision

eb2$neur_cat = as.numeric(eb2$epiNeur > mean(eb2$epiNeur)) # cut score of 9 from wikipedia
#eb2$bdi_cat_fac = as.factor(as.numeric(eb2$bdi > 9)) # cut score of 9 from wikipedia
#levels(eb2$bdi_cat_fac) <- c("no","dep")

#eb2$bdi_cat = forcats::fct_rev(eb2$bdi_cat)

out44 = glm(neur_cat ~ bfagree+bfcon+bfext+bfneur+bfopen, eb2,family=stats::binomial)
summary(out44)

probabilities44 = predict(out44,type="response")

pred.neur44 <- prediction(probabilities44,eb2$neur_cat)
roc.perf44 = performance(pred.neur44, "f")
plot(roc.perf44)

id44 = which(roc.perf44@y.values[[1]]==max(roc.perf44@y.values[[1]],na.rm=T))
cut44 = roc.perf44@x.values[[1]][id44]



