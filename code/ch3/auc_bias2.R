
neg_samples <- runif(2000,0,.1)
neg_samples2 <- runif(200,.9,1)
neg_combine <- c(neg_samples,neg_samples2)
pos_samples <- runif(200,0,1)
true <- c(rep(0,2200),rep(1,200))
comb_prob <- c(neg_combine,pos_samples)


#x11()
plot(density(neg_combine),xlab="Predicted Probability",main="")
lines(density(pos_samples),col=2)
legend(.2, 6, legend=c("Negative", "Positive"),
       col=c("black", "red"),lty=1)




library(pROC)
roc2 = roc(true,comb_prob)
auc2 = pROC::auc(true,comb_prob)

n = c(4, 3, 5) 
b = c(TRUE, FALSE, TRUE) 
par(mar = c(4, 4, 4, 4)+.1)
plot(roc2, asp = NA)




fg = comb_prob[true==1]
bg = comb_prob[true==0]

library(PRROC)
roc3<-roc.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(roc3)
pr3<-pr.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(pr3)



ret <- rep(NA,10000)
for(i in 1:10000){
  id1 = sample(1:length(pos_samples),1)
  id2 = sample(1:length(neg_combine),1)
  
  ret[i] = pos_samples[id1] > neg_combine[id2]
}
mean(ret)




# change distribution



neg_samples <- runif(2000,0,.1)
neg_samples2 <- runif(200,.9,1)
#neg_combine <- c(neg_samples,neg_samples2)
neg_combine = c(neg_samples[1:400],neg_samples2)
pos_samples <- runif(200,0,1)
#true <- c(rep(0,2200),rep(1,200))
true <- c(rep(0,600),rep(1,200))
comb_prob <- c(neg_combine,pos_samples)


#x11()
plot(density(neg_combine),xlab="Predicted Probability",main="")
lines(density(pos_samples),lty=2)
legend(.2, 2, legend=c("Negative", "Positive"),
       col=c("black", "black"),lty=c(1,2))



library(pROC)
roc2 = roc(true,comb_prob)
auc2 = pROC::auc(true,comb_prob)
plot(roc2)
auc2



fg = comb_prob[true==1]
bg = comb_prob[true==0]

library(PRROC)
roc3<-roc.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(roc3)
pr3<-pr.curve(scores.class0 = fg, scores.class1 = bg,curve=T)
plot(pr3)
